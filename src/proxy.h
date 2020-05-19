#ifndef VCTRS_PROXY_H
#define VCTRS_PROXY_H


bool is_df_restore_target(SEXP x);
SEXP vec_proxy_recursive(SEXP x, SEXP* target);


#endif
