#include "vctrs.h"

static
int int_equal_scalar(int* x, int* y, bool na_equal) {
  if (na_equal) {
    return *x == *y;
  } else {
    return (*x == NA_LOGICAL || *y == NA_LOGICAL) ? NA_LOGICAL : *x == *y;
  }
}

SEXP vctrs_equal_template2(SEXP x, SEXP y, SEXP na_equal_) {
  enum vctrs_type type = vctrs_typeof(x);
  if (type != vctrs_typeof(y) || vec_size(x) != vec_size(y)) {
    Rf_errorcall(R_NilValue, "`x` and `y` must have same types and lengths");
  }

  bool na_equal = Rf_asLogical(na_equal_);

  R_len_t n = vec_size(x);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int32_t* p_out = LOGICAL(out);

  switch (type) {
  case vctrs_type_integer: {
    int* x_ptr = INTEGER(x);
    int* y_ptr = INTEGER(y);
    for (R_len_t i = 0; i < n; ++i, ++x_ptr, ++y_ptr) {
      p_out[i] = int_equal_scalar(x_ptr, y_ptr, na_equal);
    }
    break;
  }
  default:
    Rf_error("Unimplemented");
  }

  UNPROTECT(1);
  return out;
}
