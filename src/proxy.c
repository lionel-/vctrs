#include "vctrs.h"
#include "type-data-frame.h"
#include "utils.h"

// Initialised at load time
SEXP syms_vec_proxy = NULL;
SEXP syms_vec_proxy_equal_dispatch = NULL;
SEXP fns_vec_proxy_equal_dispatch = NULL;

// Defined below
SEXP vec_proxy_method(SEXP);
SEXP vec_proxy_invoke(SEXP, SEXP);
static SEXP vec_proxy_unwrap(SEXP);
static SEXP vec_proxy_kind_recursive(SEXP, enum vctrs_proxy_kind);


// [[ register(); include("vctrs.h") ]]
SEXP vec_proxy(SEXP x) {
  int nprot = 0;
  struct vctrs_type_info info = vec_type_info(x);
  PROTECT_TYPE_INFO(&info, &nprot);

  SEXP out;
  if (info.type == vctrs_type_s3) {
    out = vec_proxy_invoke(x, info.proxy_method);
  } else {
    out = x;
  }

  UNPROTECT(nprot);
  return out;
}

// [[ register(); include("vctrs.h") ]]
SEXP vec_proxy_equal(SEXP x) {
  SEXP proxy = PROTECT(vec_proxy_kind_recursive(x, vctrs_proxy_equal));

  if (is_data_frame(proxy)) {
    // Flatten df-cols so we don't have to recurse to work with data
    // frame proxies
    proxy = PROTECT(df_flatten(proxy));

    // Unwrap data frames of size 1 since the data frame wrapper
    // doesn't impact rowwise equality or identity
    proxy = vec_proxy_unwrap(proxy);

    UNPROTECT(1);
  }

  UNPROTECT(1);
  return proxy;
}
static SEXP vec_proxy_unwrap(SEXP x) {
  if (TYPEOF(x) == VECSXP && XLENGTH(x) == 1 && is_data_frame(x)) {
    x = vec_proxy_unwrap(VECTOR_ELT(x, 0));
  }
  return x;
}

// [[ register() ]]
SEXP vctrs_unset_s4(SEXP x) {
  x = r_clone_referenced(x);
  r_unmark_s4(x);
  return x;
}

SEXP vec_proxy_equal_dispatch(SEXP x) {
  if (vec_typeof(x) == vctrs_type_s3) {
    return vctrs_dispatch1(syms_vec_proxy_equal_dispatch, fns_vec_proxy_equal_dispatch,
                           syms_x, x);
  } else {
    return x;
  }
}

static
SEXP vec_proxy_kind_recursive(SEXP x, enum vctrs_proxy_kind kind) {
  switch (kind) {
  case vctrs_proxy_default: x = vec_proxy(x); break;
  case vctrs_proxy_equal: x = vec_proxy_equal_dispatch(x); break;
  case vctrs_proxy_compare: Rf_error("Internal error: Unimplemented proxy kind");
  }
  PROTECT(x);

  if (is_data_frame(x)) {
    x = PROTECT(r_clone_referenced(x));
    R_len_t n = Rf_length(x);

    for (R_len_t i = 0; i < n; ++i) {
      SEXP col = vec_proxy_kind_recursive(VECTOR_ELT(x, i), kind);
      SET_VECTOR_ELT(x, i, col);
    }

    UNPROTECT(1);
  }

  UNPROTECT(1);
  return x;
}

// [[ register() ]]
SEXP vctrs_proxy_kind_recursive(SEXP x, SEXP kind_) {
  enum vctrs_proxy_kind kind;
  if (kind_ == Rf_install("default")) {
    kind = vctrs_proxy_default;
  } else if (kind_ == Rf_install("equal")) {
    kind = vctrs_proxy_equal;
  } else if (kind_ == Rf_install("compare")) {
    kind = vctrs_proxy_compare;
  } else {
    Rf_error("Internal error: Unexpected proxy kind `%s`.", CHAR(PRINTNAME(kind_)));
  }

  return vec_proxy_kind_recursive(x, kind);
}

SEXP vec_proxy_method(SEXP x) {
  return s3_find_method("vec_proxy", x, vctrs_method_table);
}

// This should be faster than normal dispatch but also means that
// proxy methods can't call `NextMethod()`. This could be changed if
// it turns out a problem.
SEXP vec_proxy_invoke(SEXP x, SEXP method) {
  if (method == R_NilValue) {
    return x;
  } else {
    return vctrs_dispatch1(syms_vec_proxy, method, syms_x, x);
  }
}


SEXP df_restore_target_attrib = NULL;

static
SEXP new_df_restore_target(R_len_t n_col) {
  SEXP out = PROTECT(r_new_list(2));
  SET_ATTRIB(out, df_restore_target_attrib);

  SEXP cols = PROTECT(r_new_list(n_col));
  r_list_poke(out, 1, cols);

  UNPROTECT(2);
  return out;
}
// [[ include("proxy.h") ]]
bool is_df_restore_target(SEXP x) {
  return ATTRIB(x) == df_restore_target_attrib;
}

// [[ include("proxy.h") ]]
SEXP vec_proxy_recursive(SEXP x, SEXP* target) {
  SEXP proxy = PROTECT(vec_proxy(x));

  if (!is_data_frame(proxy)) {
    *target = x;
    UNPROTECT(1);
    return x;
  }

  proxy = PROTECT(r_clone_referenced(proxy));
  R_len_t n = Rf_length(proxy);

  SEXP df_target = PROTECT(new_df_restore_target(n));
  SEXP df_target_cols = r_list_get(df_target, 1);

  r_poke_names(df_target_cols, PROTECT(r_names(proxy)));
  UNPROTECT(1);

  r_list_poke(df_target, 0, x);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP col_target = R_NilValue;
    SEXP col = vec_proxy_recursive(r_list_get(proxy, i), &col_target);

    r_list_poke(proxy, i, col);
    r_list_poke(df_target_cols, i, col_target);
  }

  UNPROTECT(3);
  *target = df_target;
  return proxy;
}
// [[ register() ]]
SEXP vctrs_proxy_recursive(SEXP x) {
  SEXP target = R_NilValue;
  SEXP proxy = vec_proxy_recursive(x, &target);
  PROTECT2(proxy, target);

  SEXP out = PROTECT(r_new_list(2));
  r_list_poke(out, 0, proxy);
  r_list_poke(out, 1, target);

  SEXP nms = PROTECT(r_new_character(2));
  r_chr_poke(nms, 0, r_str("proxy"));
  r_chr_poke(nms, 1, r_str("target"));

  r_poke_names(out, nms);

  UNPROTECT(4);
  return out;
}

SEXP vec_proxy_restore_recursive(SEXP x, SEXP to) {
  if (!is_df_restore_target(to)) {
    return vec_restore(x, to, R_NilValue);
  }

  SEXP cols_to = r_list_get(to, 1);
  SEXP out = PROTECT(r_clone_referenced(x));
  R_len_t n = Rf_length(out);

  if (n != Rf_length(cols_to)) {
    Rf_error("Internal error in `vec_proxy_restore_recursive()`: Unequal lengths.");
  }

  for (R_len_t i = 0; i < n; ++i) {
    SEXP col = r_list_get(x, i);
    SEXP col_to = r_list_get(cols_to, i);
    r_list_poke(x, i, vec_restore(col, col_to, R_NilValue));
  }

  SEXP df_to = r_list_get(to, 0);
  out = vec_restore(out, df_to, R_NilValue);

  UNPROTECT(1);
  return out;
}

void vctrs_init_data(SEXP ns) {
  syms_vec_proxy = Rf_install("vec_proxy");
  syms_vec_proxy_equal_dispatch = Rf_install("vec_proxy_equal_dispatch");

  fns_vec_proxy_equal_dispatch = r_env_get(ns, syms_vec_proxy_equal_dispatch);

  const char* code = "structure(list(df = 1, cols = 2), class = 'vctrs:::df_restore_target')";
  SEXP mold = PROTECT(r_parse_eval(code, R_BaseEnv));
  df_restore_target_attrib = ATTRIB(mold);
  R_PreserveObject(df_restore_target_attrib);
  UNPROTECT(1);
}
