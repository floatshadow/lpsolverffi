#include "glpk_header.h"
#include "ffi.h"

struct model {
    glp_prob *problem;
    int log_level;
    int solve_code;
};

#define Model_val(v) ((struct model *)Data_custom_val(v))

static void finalize_model(value model) {
    if (Model_val(model)->problem) glp_delete_prob(Model_val(model)->problem);
}

static struct custom_operations model_ops = {
    "lp.glpk.model", finalize_model,
    custom_compare_default, custom_hash_default,
    custom_serialize_default, custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default
};

CAMLprim value ocaml_lp_glpk_create(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(model);
    model = caml_alloc_custom(&model_ops, sizeof(struct model), 0, 1);
    Model_val(model)->problem = NULL;
    Model_val(model)->log_level = GLP_MSG_ALL;
    Model_val(model)->solve_code = 0;
    Model_val(model)->problem = glp_create_prob();
    CAMLreturn(model);
}

CAMLprim value ocaml_lp_glpk_number_rows(value model) {
    return Val_int(glp_get_num_rows(Model_val(model)->problem));
}

CAMLprim value ocaml_lp_glpk_number_columns(value model) {
    return Val_int(glp_get_num_cols(Model_val(model)->problem));
}

CAMLprim value ocaml_lp_glpk_number_elements(value model) {
    return Val_int(glp_get_num_nz(Model_val(model)->problem));
}

static int bound_type(double lower, double upper) {
    if (lower == -INFINITY && upper == INFINITY) return GLP_FR;
    if (lower == -INFINITY) return GLP_UP;
    if (upper == INFINITY) return GLP_LO;
    return lower == upper ? GLP_FX : GLP_DB;
}

static value add_batch(value model, value batch, value count, int columns) {
    CAMLparam3(model, batch, count);
    struct model *m = Model_val(model);
    glp_prob *p = m->problem;
    int dimension = columns ? glp_get_num_rows(p) : glp_get_num_cols(p);
    lp_check_batch(batch, count, columns, dimension);
    int n = Long_val(count);
    if (!n) CAMLreturn(Val_unit);
    int old = columns ? glp_get_num_cols(p) : glp_get_num_rows(p);
    if (n > INT_MAX - old) caml_invalid_argument("lp.glpk: too many rows/columns");
    mlsize_t capacity = 1;
    for (int i = 0; i < n; ++i) {
        value item = Field(batch, i);
        if (Double_val(Field(item, columns)) == INFINITY ||
            Double_val(Field(item, columns + 1)) == -INFINITY)
            caml_invalid_argument("lp.glpk: invalid infinite bound");
        mlsize_t size = Wosize_val(Field(item, columns + 2)) + 1;
        if (size > capacity) capacity = size;
    }
    uint64_t bytes = (uint64_t)capacity * (sizeof(double) + sizeof(int))
        + (uint64_t)dimension * sizeof(int);
    if (bytes > SIZE_MAX) caml_raise_out_of_memory();
    double *coeff = lp_alloc((size_t)bytes, 1);
    int *index = (int *)(coeff + capacity), *seen = index + capacity;
    /* GLPK aborts on duplicate indices; reject them before changing the model. */
    for (int i = 0; i < n; ++i) {
        value elements = Field(Field(batch, i), columns + 2);
        for (mlsize_t j = 0; j < Wosize_val(elements); ++j) {
            int k = Long_val(Field(Field(elements, j), 0));
            if (seen[k] == i + 1) {
                free(coeff);
                caml_invalid_argument("lp.glpk: duplicate matrix index");
            }
            seen[k] = i + 1;
        }
    }
    int first = columns ? glp_add_cols(p, n) : glp_add_rows(p, n);
    for (int i = 0; i < n; ++i) {
        value item = Field(batch, i), elements = Field(item, columns + 2);
        double lower = Double_val(Field(item, columns));
        double upper = Double_val(Field(item, columns + 1));
        int type = bound_type(lower, upper);
        if (columns) {
            glp_set_col_bnds(p, first + i, type, lower, upper);
            glp_set_obj_coef(p, first + i, Double_val(Field(item, 0)));
        } else glp_set_row_bnds(p, first + i, type, lower, upper);
        int size = Wosize_val(elements);
        for (int j = 0; j < size; ++j) {
            value element = Field(elements, j);
            index[j + 1] = Long_val(Field(element, 0)) + 1;
            coeff[j + 1] = Double_val(Field(element, 1));
        }
        if (columns) glp_set_mat_col(p, first + i, size, index, coeff);
        else glp_set_mat_row(p, first + i, size, index, coeff);
    }
    free(coeff);
    m->solve_code = 0;
    CAMLreturn(Val_unit);
}

CAMLprim value ocaml_lp_glpk_add_rows(value model, value rows, value count) {
    return add_batch(model, rows, count, 0);
}

CAMLprim value ocaml_lp_glpk_add_columns(value model, value columns, value count) {
    return add_batch(model, columns, count, 1);
}

CAMLprim value ocaml_lp_glpk_objective_coefficients(value model) {
    CAMLparam1(model);
    CAMLlocal1(result);
    glp_prob *p = Model_val(model)->problem;
    int n = glp_get_num_cols(p);
    result = caml_alloc_float_array(n);
    for (int i = 0; i < n; ++i) Store_double_field(result, i, glp_get_obj_coef(p, i + 1));
    CAMLreturn(result);
}

CAMLprim value ocaml_lp_glpk_change_objective_coefficients(value model, value array) {
    CAMLparam2(model, array);
    struct model *m = Model_val(model);
    int n = glp_get_num_cols(m->problem);
    lp_check_float_array(array, n);
    for (int i = 0; i < n; ++i)
        if (!isfinite(Double_field(array, i)))
            caml_invalid_argument("lp.glpk: non-finite objective coefficient");
    for (int i = 0; i < n; ++i) glp_set_obj_coef(m->problem, i + 1, Double_field(array, i));
    m->solve_code = 0;
    CAMLreturn(Val_unit);
}

CAMLprim value ocaml_lp_glpk_set_direction(value model, value direction) {
    CAMLparam2(model, direction);
    struct model *m = Model_val(model);
    glp_set_obj_dir(m->problem, Int_val(direction) == 0 ? GLP_MAX : GLP_MIN);
    m->solve_code = 0;
    CAMLreturn(Val_unit);
}

CAMLprim value ocaml_lp_glpk_set_log_level(value model, value level) {
    CAMLparam2(model, level);
    intnat l = Long_val(level);
    Model_val(model)->log_level = l <= 0 ? GLP_MSG_OFF : l >= GLP_MSG_DBG ? GLP_MSG_DBG : l;
    CAMLreturn(Val_unit);
}

static value solve(value model, int primal) {
    CAMLparam1(model);
    struct model *m = Model_val(model);
    glp_smcp options;
    glp_init_smcp(&options);
    options.msg_lev = m->log_level;
    options.meth = primal ? GLP_PRIMAL : GLP_DUALP;
    options.presolve = primal ? GLP_OFF : GLP_ON;
    m->solve_code = glp_simplex(m->problem, &options);
    CAMLreturn(Val_unit);
}

CAMLprim value ocaml_lp_glpk_primal(value model) {
    return solve(model, 1);
}

CAMLprim value ocaml_lp_glpk_initial_solve(value model) {
    return solve(model, 0);
}

CAMLprim value ocaml_lp_glpk_status(value model) {
    struct model *m = Model_val(model);
    if (m->solve_code == GLP_ENOPFS) return Val_int(1);
    if (m->solve_code == GLP_ENODFS) return Val_int(3);
    if (m->solve_code && m->solve_code != GLP_EITLIM && m->solve_code != GLP_ETMLIM)
        return Val_int(5);
    switch (glp_get_status(m->problem)) {
        case GLP_OPT: return Val_int(0);
        case GLP_NOFEAS: return Val_int(1);
        case GLP_UNBND: return Val_int(2);
        case GLP_FEAS: return Val_int(4);
        default: return Val_int(5);
    }
}

CAMLprim value ocaml_lp_glpk_primal_column_solution(value model) {
    CAMLparam1(model);
    CAMLlocal1(result);
    glp_prob *p = Model_val(model)->problem;
    int n = glp_get_num_cols(p);
    result = caml_alloc_float_array(n);
    for (int i = 0; i < n; ++i) Store_double_field(result, i, glp_get_col_prim(p, i + 1));
    CAMLreturn(result);
}

CAMLprim value ocaml_lp_glpk_objective_value(value model) {
    CAMLparam1(model);
    CAMLreturn(caml_copy_double(glp_get_obj_val(Model_val(model)->problem)));
}

CAMLprim value ocaml_lp_glpk_read_mps(value model, value filename) {
    CAMLparam2(model, filename);
    struct model *m = Model_val(model);
    int previous = glp_term_out(m->log_level ? GLP_ON : GLP_OFF);
    int code = glp_read_mps(m->problem, GLP_MPS_FILE, NULL, String_val(filename));
    glp_term_out(previous);
    m->solve_code = 0;
    if (code) caml_failwith("lp.glpk: read_mps failed");
    CAMLreturn(Val_unit);
}

CAMLprim value ocaml_lp_glpk_write_mps(value model, value filename) {
    CAMLparam2(model, filename);
    struct model *m = Model_val(model);
    int previous = glp_term_out(m->log_level ? GLP_ON : GLP_OFF);
    int code = glp_write_mps(m->problem, GLP_MPS_FILE, NULL, String_val(filename));
    glp_term_out(previous);
    if (code) caml_failwith("lp.glpk: write_mps failed");
    CAMLreturn(Val_unit);
}
