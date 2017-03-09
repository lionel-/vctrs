#ifndef VCTRS_TYPES_H
#define VCTRS_TYPES_H

namespace vctrs {


/* Vector discriminator */

enum vec_k {
  nil_e,
  lgl_e,
  int_e,
  fct_e,
  chr_e,
  dft_e
};

template <typename T>
vec_k vec_kind(T x) {
  return x.kind;
}

template <typename T>
bool is_integral(T x) {
  vec_k e = vec_kind(x);
  return (e <= fct_e + 1) || (e >= int_e);
}

// Will not return a constexpr so it's of limited use
inline
vec_k vctr_dispatch(SEXP x) {
  vec_k e;

  switch(TYPEOF(x)) {
  case INTSXP: {
    if (Rf_isFactor(x))
      e = fct_e;
    else
      e = int_e;
    break;
  }
  case LGLSXP: e = lgl_e; break;
  default: Rcpp::stop("Unimplemented vec_t wrapper");
  }

  return e;
}


/* Typed vectors */

struct vec_base
{
  vec_base(SEXP x, vec_k kind_) :
      sexp(x),
      kind(kind_)
  { }
  operator SEXP () const { return (SEXP) sexp; }
  const SEXP sexp;
  const vec_k kind;
};

struct vec_logical : vec_base
{
  vec_logical(SEXP x) : vec_base(x, lgl_e)
  {
    if (TYPEOF(x) != LGLSXP)
      Rcpp::stop("vctr type error");
  }
};

struct vec_integer : vec_base
{
  vec_integer(SEXP x, vec_k kind_ = int_e) : vec_base(x, kind_)
  {
    if (TYPEOF(x) != INTSXP)
      Rcpp::stop("vctr type error");
  }
};

struct vec_factor : vec_integer
{
  vec_factor(SEXP x) : vec_integer(x, fct_e)
  {
    if (!Rf_isFactor(x))
      Rcpp::stop("vctr type error");
  }
};



/* Typed vector traits */

template <vec_k E>
struct vec_type;

template <>
struct vec_type<lgl_e> {
  typedef vec_logical type;
};
template <>
struct vec_type<int_e> {
  typedef vec_integer type;
};
template <>
struct vec_type<fct_e> {
  typedef vec_factor type;
};


// We want to avoid a big switch like that:
vec_k common_type(vec_k e1, vec_k e2);


// http://martinecker.com/martincodes/lambda-expression-overloading/

// -> does not seem to work

// Or simply keep a struct of overloaded operator()
// auto f = overload(
//   []() { return 1; },
//   template <typename T>
//   [](vec_t x, T y) { return y; }
//   [](vec_factor x, vec_factor y) { return y; }
// );

// vec_t x;
// vec_t y;

// mintype(vec_factor x, vctr_)


/*

  More on ADL and customisation points:
  http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2015/n4381.html

 */


/* Generic object type. Hides polymorphism with PIMPL. Type-based
   dispatch via free functions, overloading, and ADL scoping.
   We first declare all supported methods. */

class vec_t;

template <typename T1, typename T2>
vec_t concatenate(T1& x, T2& y);

class vec_t
{
 public:
  template <typename T>
  vec_t(T x) : self_(make_unique<vec_model_t<T>>(x))
  { }
  vec_t(SEXP x);
  vec_t(const vec_t& x);

  // Dispatched methods
  operator SEXP () const { return this->self_->get_(); }
  friend vec_k vec_kind(const vec_t& x) { return x.self_->kind_(); }
  template <typename T>
  friend vec_t concatenate(const vec_t& x, const T& y) { return x.self_->concatenate_((vec_t) y); }
  // friend vctr_info_t vctr_info(const vec_t& x) { return x.self_->vctr_info_(); }

  // Member functions
  template <typename T>
  void reset(T* x) { self_.reset(x); }

  // Dispatch utils
  template <template <vec_k K1, vec_k K2> class W,
            typename T1, typename T2,
            typename R = typename W<dft_e, dft_e>::return_type>
  R dispatch(T1 x, T2 y);

  // Is the unary version necessary? Probably for constexprness in template dispatch
  template <template <vec_k K> class W,
            typename T1,
            typename R = typename W<dft_e>::return_type>
  R dispatch(T1 x);

 private:
  struct vec_interface_t
  {
    virtual ~vec_interface_t() = default;
    virtual SEXP get_() const = 0;
    virtual vec_k kind_() const = 0;
    virtual vec_t concatenate_(const vec_t& y) const = 0;
    // virtual vctr_info_t vctr_info_() const = 0;
  };

  template <typename T>
  struct vec_model_t : vec_interface_t
  {
    typedef T stored_type;
    T vctr_object;

    // Methods should be called unqualified to ensure ADL scoping
    vec_model_t(T x) : vctr_object(x) { }
    SEXP get_() const { return (SEXP) vctr_object; };
    vec_k kind_() const { return vec_kind(vctr_object); }
    vec_t concatenate_(const vec_t& y) const { return concatenate(vctr_object, y); }
  };

  std::unique_ptr<const vec_interface_t> self_;
};

template <template <vec_k K1, vec_k K2> class W,
          vec_k K,
          typename B = typename W<dft_e, dft_e>::base_type>
B kind_dispatch_rhs(vec_t y) {
  switch(vec_kind(y)) {
  case lgl_e: return W<K, lgl_e>{};
  case int_e: return W<K, int_e>{};
  case fct_e: return W<K, fct_e>{};
  default: Rcpp::stop("Internal error");
  }
}

template <template <vec_k K1, vec_k K2> class W,
          typename B = typename W<dft_e, dft_e>::base_type>
B kind_dispatch(vec_t x, vec_t y) {
  switch(vec_kind(x)) {
  case lgl_e: return kind_dispatch_rhs<W, lgl_e>(y);
  case int_e: return kind_dispatch_rhs<W, int_e>(y);
  case fct_e: return kind_dispatch_rhs<W, fct_e>(y);
  default: Rcpp::stop("Internal error");
  }
}

// template <template <class C1, class C2> class W,
//           typename T1, typename T2,
//           typename R = typename W<void, void>::return_type>
// struct tmp_select_type {
// };

// vector kind is not known at compile-time. Yet we want to
// instantiate the correct templated worker based on the two kinds.
// Can't directly select it with a runtime value though. A switch is
// not maintainable because it has to be changed when type hierarchy
// changes. Hence the template recursion. Could also have a recursive
// function.

// template <template <class C1, class C2> class W,
//           typename T1, typename T2,
//           typename R = typename W<void, void>::return_type>
// R vec_t::dispatch(T1 x, T2 y) {
//   ;
// }



/* The following vec_t constructors store fully typed objects. ADL
   scoping will ensure proper dispatch by overload resolution. */

// It is a bit annoying to resort to the preprocessor, but I don't see
// another easy way of sharing code between constructors. Hmm...
// Should vec_t just use a copyable shared_ptr? We need to copy
// because concatenate() returns a vec_t.
#define DISPATCH_VCTR(x) do {                                           \
    switch(TYPEOF(x)) {                                                 \
    case INTSXP: {                                                      \
      Rprintf("coucou\n");                                              \
      if (Rf_isFactor(x))                                               \
        self_ = make_unique<vec_model_t<vec_factor>>(x);                 \
      else                                                              \
        self_ = make_unique<vec_model_t<vec_integer>>(x);                 \
      break;                                                            \
    }                                                                   \
    case LGLSXP: self_ = make_unique<vec_model_t<vec_logical>>(x); break; \
    default: Rcpp::stop("Unimplemented vec_t wrapper");                \
    }                                                                   \
  } while(0)

// #define DISPATCH_VCTR(x) do {                                       \
//     vec_k e = vctr_dispatch(x);                                    \
//     self_ = make_unique<vec_model_t<typename vec_type<e>::type>>; \
//   } while(0)


inline
vec_t::vec_t(SEXP x)
{
  DISPATCH_VCTR(x);
}

inline
vec_t::vec_t(const vec_t& x)
{
  DISPATCH_VCTR((SEXP) x);
}


} // namespace vctrs

#endif
