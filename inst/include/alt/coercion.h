#ifndef VCTRS_COERCION_H
#define VCTRS_COERCION_H

namespace vctrs {

class vec_t;


template <typename T>
inline
bool is_coercible_to(LogicalVector x, T target) { Rcpp::stop(""); }

template <>
inline
bool is_coercible_to(LogicalVector x, SEXP target) {
  return is_coercible_to(x, (vec_t) target);
}

template <>
inline
bool is_coercible_to(LogicalVector x, vec_integer target) {
  return is_coercible_to(x, (vec_t) target);
}


/* Default methods */


// template <typename T1,                                     // Models the left-hand side type
//           template <typename T1, typename T2> typename Op> // X models a polymorphic container

// template <typename T1,
//           template <T1, typename T2> class Op>
// struct dispatcher {
//   dispatcher(T1 x) { }
// };

// template <typename T1,
//           template <T1, typename T3> class Op,
//           typename T2>
// void dispatch(T1 x, T2 y) {
//   y_typed
//   dispatcher<T1, Op<T1, bim>> test { x };
// }

// // Need to coerce lhs or rhs?
// // Then append method. Could vec_t support it?

// template <typename T1, typename T2>
// struct op_concat {
//   SEXP operator() (const T1& x, const T2& y) {
//     Rcpp::stop("Invalid combination");
//   }

//   SEXP operator() (const vec_factor& x, const vec_integer& y) {
//     return R_NilValue;
//   }
// };

// template <typename T1, typename T2>
// inline
// SEXP concatenate(const T1& x, const T2& y) {

//   dispatch<T1, op_concat>(x, y);

//   return x;
//   // return concatenate((vec_t) x, (vec_t) y);
// }

// // Does this work??
// template <>
// inline
// vec_t concatenate(const vec_t& x, const vec_t& y) {
//   Rcpp::stop("These types are not concatenable");
// }


/* Double dispatch */

template <>
inline
vec_t concatenate(const vec_factor& x, const SEXP& y) {
  Rprintf("hop?");
  return concatenate(x, (vec_t) y);
}

template <>
inline
vec_t concatenate(const vec_factor& x, const vec_factor& y) {
  Rprintf("hop\n");
  return (vec_t) y;
}

template <>
inline
vec_t concatenate(const LogicalVector& x, const vec_factor& y) {
  return (vec_t) y;
}


} // namespace vctrs

#endif
