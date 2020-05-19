#include "vctrs.h"
#include "utils.h"


__attribute__((noreturn))
void stop_incompatible_op(SEXP op, SEXP x, SEXP y) {
  vctrs_eval_mask3(Rf_install("stop_incompatible_op"),
                   Rf_install("op"), op,
                   syms_x, x,
                   syms_y, y,
                   vctrs_ns_env);
  never_reached("stop_incompatible_op");
}

SEXP vctrs_arith(SEXP op, SEXP x, SEXP y) {
  SEXP method_sym = R_NilValue;
  SEXP method = s3_find_method_xy("vec_arith", x, y, vctrs_method_table, &method_sym);

  // Compatibility with legacy double dispatch mechanism
  if (method == R_NilValue) {
    SEXP x_method_sym = R_NilValue;
    SEXP x_method = PROTECT(s3_find_method2("vec_arith",
                                             x,
                                             vctrs_method_table,
                                             &x_method_sym));

    if (x_method != R_NilValue) {
      const char* x_method_str = CHAR(PRINTNAME(x_method_sym));
      SEXP x_table = s3_get_table(CLOENV(x_method));

      method = s3_find_method2(x_method_str,
                               y,
                               x_table,
                               &method_sym);
    }

    UNPROTECT(1);
  }

  PROTECT(method);

  if (method == R_NilValue) {
    stop_incompatible_op(op, x, y);
  }

  SEXP out = vctrs_dispatch3(method_sym, method,
                             Rf_install("op"), op,
                             syms_x, x,
                             syms_y, y);

  UNPROTECT(1);
  return out;
}
