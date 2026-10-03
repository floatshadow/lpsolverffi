#ifndef OCAML_LP_FFI_H
#define OCAML_LP_FFI_H

#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

static inline void lp_check_float_array(value array, intnat length) {
    if (Wosize_val(array) / Double_wosize != (uintnat)length)
        caml_invalid_argument("lp: wrong float array length");
}

/* Validate the complete batch before mutating the solver or allocating buffers. */
static inline int lp_check_batch(value batch, value count, int columns, intnat dimension) {
    intnat n = Long_val(count);
    if (n < 0 || n > INT_MAX || (uintnat)n > Wosize_val(batch))
        caml_invalid_argument("lp: invalid batch count");
    uintnat nnz = 0;
    for (intnat i = 0; i < n; ++i) {
        value item = Field(batch, i);
        double lower = Double_val(Field(item, columns));
        double upper = Double_val(Field(item, columns + 1));
        if (isnan(lower) || isnan(upper))
            caml_invalid_argument("lp: NaN bound");
        if (columns && !isfinite(Double_val(Field(item, 0))))
            caml_invalid_argument("lp: non-finite objective coefficient");
        value elements = Field(item, columns + 2);
        nnz += Wosize_val(elements);
        if (nnz > INT_MAX) caml_invalid_argument("lp: too many matrix elements");
        for (mlsize_t j = 0; j < Wosize_val(elements); ++j) {
            value element = Field(elements, j);
            intnat index = Long_val(Field(element, 0));
            if (index < 0 || index >= dimension)
                caml_invalid_argument("lp: matrix index out of range");
            if (!isfinite(Double_val(Field(element, 1))))
                caml_invalid_argument("lp: non-finite matrix coefficient");
        }
    }
    return (int)nnz;
}

static inline void lp_check_indices(value indices, int dimension) {
    if (Wosize_val(indices) > INT_MAX) caml_invalid_argument("lp: too many indices");
    for (mlsize_t i = 0; i < Wosize_val(indices); ++i) {
        intnat index = Long_val(Field(indices, i));
        if (index < 0 || index >= dimension)
            caml_invalid_argument("lp: index out of range");
    }
}

static inline void *lp_alloc(size_t count, size_t size) {
    if (count > SIZE_MAX / size) caml_raise_out_of_memory();
    void *buffer = calloc(count ? count : 1, size);
    if (!buffer) caml_raise_out_of_memory();
    return buffer;
}

#endif
