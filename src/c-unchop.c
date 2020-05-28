#include "vctrs.h"
#include "c.h"
#include "ptype-common.h"
#include "slice.h"
#include "slice-assign.h"
#include "utils.h"

// Defined in slice-chop.c
SEXP vec_as_indices(SEXP indices, R_len_t n, SEXP names);


static SEXP vec_unchop(SEXP x,
                       SEXP indices,
                       SEXP ptype,
                       SEXP name_spec,
                       const struct name_repair_opts* name_repair);

// [[ register() ]]
SEXP vctrs_unchop(SEXP x, SEXP indices, SEXP ptype, SEXP name_spec, SEXP name_repair) {
  struct name_repair_opts name_repair_opts = new_name_repair_opts(name_repair, args_empty, false);
  PROTECT_NAME_REPAIR_OPTS(&name_repair_opts);

  SEXP out = vec_unchop(x, indices, ptype, name_spec, &name_repair_opts);

  UNPROTECT(1);
  return out;
}

static inline bool needs_vec_unchop_fallback(SEXP x, SEXP ptype);

enum fallback_homogeneous {
  FALLBACK_HOMOGENEOUS_false = 0,
  FALLBACK_HOMOGENEOUS_true
};
static SEXP vec_unchop_fallback(SEXP x,
                                SEXP indices,
                                SEXP name_spec,
                                enum fallback_homogeneous homogenous);

static SEXP vec_unchop(SEXP x,
                       SEXP indices,
                       SEXP ptype,
                       SEXP name_spec,
                       const struct name_repair_opts* name_repair) {
  if (!vec_is_list(x)) {
    Rf_errorcall(R_NilValue, "`x` must be a list");
  }

  if (indices == R_NilValue) {
    return vec_c(x, ptype, name_spec, name_repair);
  }

  R_len_t x_size = vec_size(x);

  // Apply size/type checking to `indices` before possibly exiting early from
  // having a `NULL` common type
  if (x_size != vec_size(indices)) {
    Rf_errorcall(R_NilValue, "`x` and `indices` must be lists of the same size");
  }

  if (!vec_is_list(indices)) {
    Rf_errorcall(R_NilValue, "`indices` must be a list of integers, or `NULL`");
  }

  ptype = PROTECT(vec_ptype_common_params(x, ptype, DF_FALLBACK_DEFAULT, S3_FALLBACK_true));

  if (needs_vec_c_fallback(ptype)) {
    SEXP out = vec_unchop_fallback(x, indices, name_spec, FALLBACK_HOMOGENEOUS_false);
    UNPROTECT(1);
    return out;
  }
  // FIXME: Needed for dplyr::summarise() which passes a non-fallback ptype
  if (needs_vec_c_homogeneous_fallback(x, ptype)) {
    SEXP out = vec_unchop_fallback(x, indices, name_spec, FALLBACK_HOMOGENEOUS_true);
    UNPROTECT(1);
    return out;
  }

  if (ptype == R_NilValue) {
    UNPROTECT(1);
    return R_NilValue;
  }

  x = PROTECT(vec_cast_common(x, ptype));

  SEXP x_names = PROTECT(r_names(x));

  bool has_outer_names = (x_names != R_NilValue);
  bool assign_names = !Rf_inherits(name_spec, "rlang_zap");
  bool has_names =
    assign_names &&
    !is_data_frame(ptype) &&
    (has_outer_names || list_has_inner_vec_names(x, x_size));

  // Element sizes are only required for applying the `name_spec`
  SEXP sizes = vctrs_shared_empty_int;
  if (has_names) {
    sizes = Rf_allocVector(INTSXP, x_size);
  }
  PROTECT(sizes);
  int* p_sizes = INTEGER(sizes);

  R_len_t out_size = 0;

  // `out_size` is computed from `indices`
  for (R_len_t i = 0; i < x_size; ++i) {
    SEXP elt = VECTOR_ELT(x, i);

    if (elt == R_NilValue) {
      continue;
    }

    R_len_t index_size = vec_size(VECTOR_ELT(indices, i));
    out_size += index_size;

    if (has_names) {
      p_sizes[i] = index_size;
    }

    // Each element of `x` is recycled to its corresponding index's size
    elt = vec_recycle(elt, index_size, args_empty);
    SET_VECTOR_ELT(x, i, elt);
  }

  indices = PROTECT(vec_as_indices(indices, out_size, R_NilValue));

  PROTECT_INDEX proxy_pi;
  SEXP proxy = vec_proxy(ptype);
  PROTECT_WITH_INDEX(proxy, &proxy_pi);
  proxy = vec_init(proxy, out_size);
  REPROTECT(proxy, proxy_pi);

  PROTECT_INDEX out_names_pi;
  SEXP out_names = vctrs_shared_empty_chr;
  if (has_names) {
    out_names = Rf_allocVector(STRSXP, out_size);
  }
  PROTECT_WITH_INDEX(out_names, &out_names_pi);

  const struct vec_assign_opts unchop_assign_opts = {
    .assign_names = assign_names
  };

  for (R_len_t i = 0; i < x_size; ++i) {
    SEXP elt = VECTOR_ELT(x, i);

    if (elt == R_NilValue) {
      continue;
    }

    SEXP index = VECTOR_ELT(indices, i);

    // Total ownership of `proxy` because it was freshly created with `vec_init()`
    proxy = vec_proxy_assign_opts(proxy, index, elt, vctrs_ownership_total, &unchop_assign_opts);
    REPROTECT(proxy, proxy_pi);

    if (has_names) {
      R_len_t size = p_sizes[i];
      SEXP outer = (has_outer_names) ? STRING_ELT(x_names, i) : R_NilValue;
      SEXP inner = PROTECT(vec_names(elt));
      SEXP elt_names = PROTECT(apply_name_spec(name_spec, outer, inner, size));
      if (elt_names != R_NilValue) {
        out_names = chr_assign(out_names, index, elt_names, vctrs_ownership_total);
        REPROTECT(out_names, out_names_pi);
      }
      UNPROTECT(2);
    }
  }

  SEXP out_size_sexp = PROTECT(r_int(out_size));

  SEXP out = PROTECT(vec_restore(proxy, ptype, out_size_sexp));

  if (has_names) {
    out_names = vec_as_names(out_names, name_repair);
    REPROTECT(out_names, out_names_pi);
    out = vec_set_names(out, out_names);
  } else if (!assign_names) {
    // FIXME: `vec_ptype2()` doesn't consistently zaps names, so `out`
    // might have been initialised with names. This branch can be
    // removed once #1020 is resolved.
    out = vec_set_names(out, R_NilValue);
  }

  UNPROTECT(9);
  return out;
}

// Unchopping is a just version of `vec_c()` that controls the ordering,
// so they both fallback to `c()` in the same situations
static inline bool needs_vec_unchop_fallback(SEXP x, SEXP ptype) {
  return needs_vec_c_homogeneous_fallback(x, ptype);
}

// This is essentially:
// vec_slice_fallback(vec_c_fallback_invoke(!!!x), order(vec_c(!!!indices)))
// with recycling of each element of `x` to the corresponding index size
static SEXP vec_unchop_fallback(SEXP x,
                                SEXP indices,
                                SEXP name_spec,
                                enum fallback_homogeneous homogenous) {
  R_len_t x_size = vec_size(x);
  x = PROTECT(r_clone_referenced(x));

  R_len_t out_size = 0;

  // Recycle `x` elements to the size of their corresponding index
  for (R_len_t i = 0; i < x_size; ++i) {
    SEXP elt = VECTOR_ELT(x, i);

    R_len_t index_size = vec_size(VECTOR_ELT(indices, i));
    out_size += index_size;

    SET_VECTOR_ELT(x, i, vec_recycle_fallback(elt, index_size, args_empty));
  }

  indices = PROTECT(vec_as_indices(indices, out_size, R_NilValue));

  SEXP out = PROTECT(vec_c_fallback_invoke(x, name_spec));

  const struct name_repair_opts name_repair_opts = {
    .type = name_repair_none,
    .fn = R_NilValue
  };

  indices = PROTECT(vec_c(
    indices,
    vctrs_shared_empty_int,
    R_NilValue,
    &name_repair_opts
  ));

  const int* p_indices = INTEGER(indices);

  SEXP locations = PROTECT(Rf_allocVector(INTSXP, out_size));
  int* p_locations = INTEGER(locations);

  // Initialize with missing to handle locations that are never selected
  for (R_len_t i = 0; i < out_size; ++i) {
    p_locations[i] = NA_INTEGER;
  }

  for (R_len_t i = 0; i < out_size; ++i) {
    const int index = p_indices[i];

    if (index == NA_INTEGER) {
      continue;
    }

    p_locations[index - 1] = i + 1;
  }

  out = PROTECT(vec_slice_fallback(out, locations));

  UNPROTECT(6);
  return out;
}
