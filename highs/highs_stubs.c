#include "highs_header.h"
#include "ffi.h"

#define Model_val(v) (*((void **)Data_custom_val(v)))

static void check(HighsInt status, const char *operation) {
    if (status == kHighsStatusError) caml_failwith(operation);
}

static void finalize_model(value model) {
    if (Model_val(model)) Highs_destroy(Model_val(model));
}

static struct custom_operations model_ops = {
    "lp.highs.model", finalize_model,
    custom_compare_default, custom_hash_default,
    custom_serialize_default, custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default
};

CAMLprim value ocaml_lp_highs_create(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(model);
    model = caml_alloc_custom(&model_ops, sizeof(void *), 0, 1);
    Model_val(model) = Highs_create();
    if (!Model_val(model)) caml_raise_out_of_memory();
    CAMLreturn(model);
}

CAMLprim value ocaml_lp_highs_number_rows(value model) {
    return Val_long(Highs_getNumRow(Model_val(model)));
}

CAMLprim value ocaml_lp_highs_number_columns(value model) {
    return Val_long(Highs_getNumCol(Model_val(model)));
}

CAMLprim value ocaml_lp_highs_number_elements(value model) {
    return Val_long(Highs_getNumNz(Model_val(model)));
}

static value add_batch(value model, value batch, value count, int columns) {
    CAMLparam3(model, batch, count);
    void *m = Model_val(model);
    HighsInt dimension = columns ? Highs_getNumRow(m) : Highs_getNumCol(m);
    int nnz = lp_check_batch(batch, count, columns, dimension);
    HighsInt n = Long_val(count);
    if (!n) CAMLreturn(Val_unit);
    if ((int64_t)n + (columns ? Highs_getNumCol(m) : Highs_getNumRow(m)) > INT_MAX)
        caml_invalid_argument("lp.highs: too many rows/columns");
    /* One allocation keeps cleanup atomic, including on an API error. */
    uint64_t bytes = (3 * (uint64_t)n + nnz) * sizeof(double)
        + ((uint64_t)n + 1 + nnz) * sizeof(HighsInt);
    if (bytes > SIZE_MAX) caml_raise_out_of_memory();
    double *buffer = lp_alloc((size_t)bytes, 1);
    double *lower = buffer, *upper = lower + n, *cost = upper + n, *coeff = cost + n;
    HighsInt *start = (HighsInt *)(coeff + nnz), *index = start + n + 1;
    HighsInt k = 0;
    double inf = Highs_getInfinity(m);
    for (HighsInt i = 0; i < n; ++i) {
        value item = Field(batch, i), elements = Field(item, columns + 2);
        double lo = Double_val(Field(item, columns));
        double hi = Double_val(Field(item, columns + 1));
        lower[i] = isinf(lo) ? copysign(inf, lo) : lo;
        upper[i] = isinf(hi) ? copysign(inf, hi) : hi;
        if (columns) cost[i] = Double_val(Field(item, 0));
        start[i] = k;
        for (mlsize_t j = 0; j < Wosize_val(elements); ++j) {
            value element = Field(elements, j);
            index[k] = Long_val(Field(element, 0));
            coeff[k++] = Double_val(Field(element, 1));
        }
    }
    start[n] = k;
    HighsInt status = columns
        ? Highs_addCols(m, n, cost, lower, upper, nnz, start, index, coeff)
        : Highs_addRows(m, n, lower, upper, nnz, start, index, coeff);
    free(buffer);
    check(status, columns ? "lp.highs: add_columns failed" : "lp.highs: add_rows failed");
    CAMLreturn(Val_unit);
}

CAMLprim value ocaml_lp_highs_add_rows(value model, value rows, value count) {
    return add_batch(model, rows, count, 0);
}

CAMLprim value ocaml_lp_highs_add_columns(value model, value columns, value count) {
    return add_batch(model, columns, count, 1);
}

CAMLprim value ocaml_lp_highs_objective_coefficients(value model) {
    CAMLparam1(model);
    CAMLlocal1(result);
    void *m = Model_val(model);
    HighsInt n = Highs_getNumCol(m), actual = 0, nnz = 0;
    result = caml_alloc_float_array(n);
    if (n) {
        double *cost = lp_alloc(n, sizeof(double));
        HighsInt status = Highs_getColsByRange(m, 0, n - 1, &actual, cost,
                                               NULL, NULL, &nnz, NULL, NULL, NULL);
        for (HighsInt i = 0; i < n; ++i) Store_double_field(result, i, cost[i]);
        free(cost);
        check(status, "lp.highs: objective_coefficients failed");
    }
    CAMLreturn(result);
}

CAMLprim value ocaml_lp_highs_change_objective_coefficients(value model, value array) {
    CAMLparam2(model, array);
    void *m = Model_val(model);
    HighsInt n = Highs_getNumCol(m);
    lp_check_float_array(array, n);
    if (!n) CAMLreturn(Val_unit);
    double *cost = lp_alloc(n, sizeof(double));
    for (HighsInt i = 0; i < n; ++i) cost[i] = Double_field(array, i);
    HighsInt status = Highs_changeColsCostByRange(m, 0, n - 1, cost);
    free(cost);
    check(status, "lp.highs: change_objective_coefficients failed");
    CAMLreturn(Val_unit);
}

CAMLprim value ocaml_lp_highs_set_direction(value model, value direction) {
    CAMLparam2(model, direction);
    check(Highs_changeObjectiveSense(Model_val(model),
          Int_val(direction) == 0 ? kHighsObjSenseMaximize : kHighsObjSenseMinimize),
          "lp.highs: set_direction failed");
    CAMLreturn(Val_unit);
}

CAMLprim value ocaml_lp_highs_set_log_level(value model, value level) {
    CAMLparam2(model, level);
    /* HiGHS exposes an output switch, rather than CLP's integer verbosity. */
    check(Highs_setBoolOptionValue(Model_val(model), "output_flag", Long_val(level) > 0),
          "lp.highs: set_log_level failed");
    CAMLreturn(Val_unit);
}

CAMLprim value ocaml_lp_highs_primal(value model) {
    CAMLparam1(model);
    void *m = Model_val(model);
    check(Highs_setStringOptionValue(m, "solver", "simplex"), "lp.highs: solver option failed");
    check(Highs_setIntOptionValue(m, "simplex_strategy", 4), "lp.highs: primal option failed");
    check(Highs_run(m), "lp.highs: primal failed");
    CAMLreturn(Val_unit);
}

CAMLprim value ocaml_lp_highs_initial_solve(value model) {
    CAMLparam1(model);
    void *m = Model_val(model);
    check(Highs_setStringOptionValue(m, "solver", "choose"), "lp.highs: solver option failed");
    check(Highs_setIntOptionValue(m, "simplex_strategy", 1), "lp.highs: simplex option failed");
    check(Highs_run(m), "lp.highs: initial_solve failed");
    CAMLreturn(Val_unit);
}

CAMLprim value ocaml_lp_highs_status(value model) {
    CAMLparam1(model);
    void *m = Model_val(model);
    HighsInt status = Highs_getModelStatus(m);
    int result = 5;
    if (status == kHighsModelStatusOptimal || status == kHighsModelStatusModelEmpty) result = 0;
    else if (status == kHighsModelStatusInfeasible) result = 1;
    else if (status == kHighsModelStatusUnbounded) result = 2;
    else if (status == kHighsModelStatusUnboundedOrInfeasible) result = 3;
    else {
        HighsInt primal_status = kHighsSolutionStatusNone;
        if (Highs_getIntInfoValue(m, "primal_solution_status", &primal_status) == kHighsStatusOk
            && primal_status == kHighsSolutionStatusFeasible) result = 4;
    }
    CAMLreturn(Val_int(result));
}

CAMLprim value ocaml_lp_highs_primal_column_solution(value model) {
    CAMLparam1(model);
    CAMLlocal1(result);
    void *m = Model_val(model);
    HighsInt n = Highs_getNumCol(m);
    result = caml_alloc_float_array(n);
    if (n) {
        double *solution = lp_alloc(n, sizeof(double));
        HighsInt status = Highs_getSolution(m, solution, NULL, NULL, NULL);
        for (HighsInt i = 0; i < n; ++i) Store_double_field(result, i, solution[i]);
        free(solution);
        check(status, "lp.highs: primal_column_solution failed");
    }
    CAMLreturn(result);
}

CAMLprim value ocaml_lp_highs_objective_value(value model) {
    CAMLparam1(model);
    CAMLreturn(caml_copy_double(Highs_getObjectiveValue(Model_val(model))));
}

CAMLprim value ocaml_lp_highs_read_mps(value model, value filename) {
    CAMLparam2(model, filename);
    check(Highs_readModel(Model_val(model), String_val(filename)), "lp.highs: read_mps failed");
    CAMLreturn(Val_unit);
}

CAMLprim value ocaml_lp_highs_write_mps(value model, value filename) {
    CAMLparam2(model, filename);
    check(Highs_writeModel(Model_val(model), String_val(filename)), "lp.highs: write_mps failed");
    CAMLreturn(Val_unit);
}
