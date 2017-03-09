#ifndef VCTRS_VCTRS_TRAITS_SELECTOR_H
#define VCTRS_VCTRS_TRAITS_SELECTOR_H

#include <vctrs/types.h>
#include <vctrs/traits/class.h>

namespace vctrs { namespace traits {

/*
   Compile-time recursion through the vector type enumeration
   Parameterised by a polymorphic worker X containing methods.
   Returns the relevant specialised worker coerced to its base class

   Inherits from a version of itself parameterised by the preceding
   enumerated type in order to allow static casting.

   Question: type_ is a runtime value. How much of this can get
   inlined at compile-time? If the function calls do not get inlined,
   we're better off with a switch.
*/
template <template <class C> class X,                  // Models a polymorphic worker
          class Base = typename X<void>::base_class,   // Models the returned worker's base class
          VctrTypes type = VCTR_DEFAULT>               // Models the current enumed type
struct vctr_type_selector : public vctr_type_selector<X, Base, (VctrTypes)(type - 1)> {
  Base& select(VctrTypes type_)  {
    if (type_ != type) {
      return static_cast<vctr_type_selector<X, Base, (VctrTypes)(type - 1)>*>(this)->select(type_);
    }

    static X<typename traits::vctr_class<type>::type> x;
    return x;
  }

  Base& select(const Vctr& v) {
    return select(v.get_type());
  }
};

template <template <class C> class X,  // Models a polymorphic worker
          class Base>                  // Models return value (unused)
struct vctr_type_selector<X, Base, VCTR_NONE> {
  Base& select(VctrTypes type_)  {
    Rcpp::stop("Unknown type: %d", type_);
  }
};


template <template <class C1, class C2> class X,
          class Base = typename X<void, void>::base_class,
          VctrTypes type1 = VCTR_DEFAULT,
          VctrTypes type2 = VCTR_DEFAULT>
struct vctr_type_selector2 : public vctr_type_selector2<X, Base, type1, (VctrTypes)(type2 - 1)> {
  template <class C1>
  class XX : public X<C1, typename traits::vctr_class<type2>::type> {};

  Base& select(VctrTypes type1_, VctrTypes type2_)  {
    if (type2_ != type2) {
      return static_cast<vctr_type_selector2<X, Base, type1, (VctrTypes)(type2 - 1)>*>(this)->select(type1_, type2_);
    }

    return vctr_type_selector<XX, Base>().select(type1_);
  }

  Base& select(const Vctr& v1, const Vctr& v2) {
    return select(v1.get_type(), v2.get_type());
  }
};

template <template <class C1, class C2> class X, class Base, VctrTypes type1>
struct vctr_type_selector2<X, Base, type1, VCTR_NONE> {
  Base& select(VctrTypes type1_, VctrTypes type2_) {
    Rcpp::stop("Unknown type: ", type2_);
  }
};

}}

#endif // VCTRS_VCTRS_TRAITS_SELECTOR_H
