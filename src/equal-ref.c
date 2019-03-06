#include "vctrs.h"

// Callers should handle the NULL case for non-array types
void* vctrs_deref_array(SEXP x) {
  switch (vctrs_typeof(x)) {
  case vctrs_type_logical: return (void*) LOGICAL(x);
  case vctrs_type_integer: return (void*) INTEGER(x);
  case vctrs_type_double: return (void*) REAL(x);
  case vctrs_type_complex: return (void*) COMPLEX(x);
  case vctrs_type_character: return (void*) STRING_PTR(x);
  case vctrs_type_raw: return (void*) RAW(x);
  default: return NULL;
  }
}

struct vctrs_ref {
  SEXP object;
  enum vctrs_type type;
  void* pointer;
  R_len_t i;
};

struct vctrs_ref vctrs_deref(SEXP x) {
  struct vctrs_ref info;

  info.object = x;
  info.type = vctrs_typeof(x);
  info.pointer = vctrs_deref_array(x);
  info.i = 0;

  return info;
};

void vctrs_inc(struct vctrs_ref* ref) {
  if (ref->pointer) {
    switch (ref->type) {
    case vctrs_type_logical:
    case vctrs_type_integer:
      ref->pointer = ((int*) ref->pointer) + 1;
      break;
    case vctrs_type_double:
      ref->pointer = ((double*) ref->pointer) + 1;
      break;
    case vctrs_type_complex:
      ref->pointer = ((Rcomplex*) ref->pointer) + 1;
      break;
    case vctrs_type_character:
      ref->pointer = ((SEXP*) ref->pointer) + 1;
      break;
    case vctrs_type_raw:
      ref->pointer = ((Rbyte*) ref->pointer) + 1;
      break;
    default:
      break;
    }
  }
  ++(ref->i);
}


int equal_scalar_ref(struct vctrs_ref* x_ref, struct vctrs_ref* y_ref, bool na_equal) {
  if (x_ref->type != y_ref->type) {
    return false;
  }

  switch (x_ref->type) {
  case vctrs_type_logical: {
    int xi = *((int*) x_ref->pointer);
    int yj = *((int*) y_ref->pointer);
    if (xi == NA_LOGICAL) return na_equal ? yj == NA_LOGICAL : NA_LOGICAL;
    if (yj == NA_LOGICAL) return na_equal ? xi == NA_LOGICAL : NA_LOGICAL;
    return xi == yj;
  }
  case vctrs_type_integer: {
    int xi = *((int*) x_ref->pointer);
    int yj = *((int*) y_ref->pointer);
    if (xi == NA_INTEGER) return na_equal ? yj == NA_INTEGER : NA_LOGICAL;
    if (yj == NA_INTEGER) return na_equal ? xi == NA_INTEGER : NA_LOGICAL;
    return xi == yj;
  }
  case vctrs_type_double:
  case vctrs_type_character:
  case vctrs_type_list:
    Rf_error("TODO equal ref");
  default:
    Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x_ref->object)));
  }
}

int equal_scalar_simple(struct vctrs_ref* x_ref, struct vctrs_ref* y_ref) {
  if (x_ref->type != y_ref->type) {
    return false;
  }

  switch (x_ref->type) {
  case vctrs_type_logical:
  case vctrs_type_integer: {
    int xi = *((int*) x_ref->pointer);
    int yj = *((int*) y_ref->pointer);
    return xi == yj;
  }
  case vctrs_type_double:
  case vctrs_type_character:
  case vctrs_type_list:
    Rf_error("TODO equal ref");
  default:
    Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x_ref->object)));
  }
}

int equal_scalar_propagate(struct vctrs_ref* x_ref, struct vctrs_ref* y_ref) {
  if (x_ref->type != y_ref->type) {
    return false;
  }

  switch (x_ref->type) {
  case vctrs_type_logical: {
    int xi = *((int*) x_ref->pointer);
    int yj = *((int*) y_ref->pointer);
    if (xi == NA_LOGICAL || yj == NA_LOGICAL) {
      return NA_LOGICAL;
    } else {
      return xi == yj;
    }
  }
  case vctrs_type_integer: {
    int xi = *((int*) x_ref->pointer);
    int yj = *((int*) y_ref->pointer);
    if (xi == NA_INTEGER || yj == NA_INTEGER) {
      return NA_INTEGER;
    } else {
      return xi == yj;
    }
  }
  case vctrs_type_double:
  case vctrs_type_character:
  case vctrs_type_list:
    Rf_error("TODO equal ref");
  default:
    Rf_errorcall(R_NilValue, "Unsupported type %s", Rf_type2char(TYPEOF(x_ref->object)));
  }
}

SEXP vctrs_equal_no_deref(SEXP x, SEXP y, SEXP na_equal_) {
  SEXPTYPE type = TYPEOF(x);
  if (type != TYPEOF(y) || vec_size(x) != vec_size(y)) {
    Rf_errorcall(R_NilValue, "`x` and `y` must have same types and lengths");
  }

  bool na_equal = Rf_asLogical(na_equal_);

  R_len_t n = vec_size(x);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int32_t* p_out = LOGICAL(out);

  struct vctrs_ref x_ref = vctrs_deref(x);
  struct vctrs_ref y_ref = vctrs_deref(y);

  for (R_len_t i = 0; i < n; ++i, vctrs_inc(&x_ref), vctrs_inc(&y_ref)) {
    p_out[i] = equal_scalar_ref(&x_ref, &y_ref, na_equal);
  }

  UNPROTECT(1);
  return out;
}

SEXP vctrs_equal_no_branch(SEXP x, SEXP y, SEXP na_equal_) {
  SEXPTYPE type = TYPEOF(x);
  if (type != TYPEOF(y) || vec_size(x) != vec_size(y)) {
    Rf_errorcall(R_NilValue, "`x` and `y` must have same types and lengths");
  }

  bool na_equal = Rf_asLogical(na_equal_);

  R_len_t n = vec_size(x);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int32_t* p_out = LOGICAL(out);

  struct vctrs_ref x_ref = vctrs_deref(x);
  struct vctrs_ref y_ref = vctrs_deref(y);

  if (na_equal) {
    for (R_len_t i = 0; i < n; ++i, vctrs_inc(&x_ref), vctrs_inc(&y_ref)) {
      p_out[i] = equal_scalar_simple(&x_ref, &y_ref);
    }
  } else {
    for (R_len_t i = 0; i < n; ++i, vctrs_inc(&x_ref), vctrs_inc(&y_ref)) {
      p_out[i] = equal_scalar_propagate(&x_ref, &y_ref);
    }
  }

  UNPROTECT(1);
  return out;
}
