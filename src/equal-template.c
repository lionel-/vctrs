#include "vctrs.h"

int int_equal_scalar(int* x, int* y) {
  return *x == *y;
}
int int_equal_scalar_propagate(int* x, int* y) {
  if (*x == NA_INTEGER || *y == NA_INTEGER) {
    return NA_INTEGER;
  } else {
    return *x == *y;
  }
}
int lgl_equal_scalar_propagate(int* x, int* y) {
  if (*x == NA_LOGICAL || *y == NA_LOGICAL) {
    return NA_LOGICAL;
  } else {
    return *x == *y;
  }
}

int lgl_equal_scalar_branch(int*x, int* y, bool na_equal) {
  if (*x == NA_LOGICAL) return na_equal ? *y == NA_LOGICAL : NA_LOGICAL;
  if (*y == NA_LOGICAL) return na_equal ? *x == NA_LOGICAL : NA_LOGICAL;
  return *x == *y;
}
int int_equal_scalar_branch(int*x, int* y, bool na_equal) {
  if (*x == NA_INTEGER) return na_equal ? *y == NA_INTEGER : NA_INTEGER;
  if (*y == NA_INTEGER) return na_equal ? *x == NA_INTEGER : NA_INTEGER;
  return *x == *y;
}


SEXP vctrs_equal_template_full(SEXP x, SEXP y, SEXP na_equal_) {
  enum vctrs_type type = vctrs_typeof(x);
  if (type != vctrs_typeof(y) || vec_size(x) != vec_size(y)) {
    Rf_errorcall(R_NilValue, "`x` and `y` must have same types and lengths");
  }

  bool na_equal = Rf_asLogical(na_equal_);

  R_len_t n = vec_size(x);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int32_t* p_out = LOGICAL(out);

  if (na_equal_) {
    switch (type) {
    case vctrs_type_logical:
    case vctrs_type_integer: {
      int* x_ptr = INTEGER(x);
      int* y_ptr = INTEGER(y);
      for (R_len_t i = 0; i < n; ++i, ++x_ptr, ++y_ptr) {
        p_out[i] = int_equal_scalar(x_ptr, y_ptr);
      }
      break;
    }
    default:
      Rf_error("Unimplemented");
    }
  } else {
    switch (type) {
    case vctrs_type_logical: {
      int* x_ptr = INTEGER(x);
      int* y_ptr = INTEGER(y);
      for (R_len_t i = 0; i < n; ++i, ++x_ptr, ++y_ptr) {
        p_out[i] = lgl_equal_scalar_propagate(x_ptr, y_ptr);
      }
    }
    case vctrs_type_integer: {
      int* x_ptr = INTEGER(x);
      int* y_ptr = INTEGER(y);
      for (R_len_t i = 0; i < n; ++i, ++x_ptr, ++y_ptr) {
        p_out[i] = int_equal_scalar_propagate(x_ptr, y_ptr);
      }
      break;
    }
    default:
      Rf_error("Unimplemented");
    }
  }

  UNPROTECT(1);
  return out;
}

SEXP vctrs_equal_template(SEXP x, SEXP y, SEXP na_equal_) {
  enum vctrs_type type = vctrs_typeof(x);
  if (type != vctrs_typeof(y) || vec_size(x) != vec_size(y)) {
    Rf_errorcall(R_NilValue, "`x` and `y` must have same types and lengths");
  }

  bool na_equal = Rf_asLogical(na_equal_);

  R_len_t n = vec_size(x);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int32_t* p_out = LOGICAL(out);

  switch (type) {
  case vctrs_type_logical: {
    int* x_ptr = LOGICAL(x);
    int* y_ptr = LOGICAL(y);
    for (R_len_t i = 0; i < n; ++i, ++x_ptr, ++y_ptr) {
      p_out[i] = lgl_equal_scalar_branch(x_ptr, y_ptr, na_equal_);
    }
  }
  case vctrs_type_integer: {
    int* x_ptr = INTEGER(x);
    int* y_ptr = INTEGER(y);
    for (R_len_t i = 0; i < n; ++i, ++x_ptr, ++y_ptr) {
      p_out[i] = int_equal_scalar_branch(x_ptr, y_ptr, na_equal_);
    }
    break;
  }
  default:
    Rf_error("Unimplemented");
  }

  UNPROTECT(1);
  return out;
}
