
namespace vctrs {


using sexp = SEXPREC;

inline
void dbg_print(sexp* x) {
  Rf_PrintValue(x);
}

template <typename... Args>
inline
void r_throw(const char* msg, Args&& ...args) {
  Rf_error(msg, args...);
}

template <typename... Args>
inline
void r_printf(const char* msg, Args&& ...args) {
  Rprintf(msg, args...);
}

inline
void r_print(const char* msg) {
  Rprintf("%s\n", msg);
}


struct r_string;

template <typename T> struct vec_traits { };
template <> struct vec_traits<int> { enum{ sxp_type = INTSXP }; };
template <> struct vec_traits<double> { enum{ sxp_type = REALSXP }; };
template <> struct vec_traits<Rcomplex> { enum{ sxp_type = CPLXSXP }; };
template <> struct vec_traits<sexp*> { enum{ sxp_type = VECSXP }; };
template <> struct vec_traits<const char**> { enum{ sxp_type = STRSXP }; };
template <> struct vec_traits<r_string> { enum{ sxp_type = STRSXP }; };

template <typename T>
inline
sexp* vec_alloc(size_t n) {
  return Rf_allocVector(vec_traits<T>::sxp_type, n);
}



inline
void sxp_protect(sexp* x) {
  PROTECT(x);
}
inline
void sxp_unprotect(sexp* x) {
  UNPROTECT(1);
}

inline
void sxp_preserve(sexp* x) {
  R_PreserveObject(x);
}
inline
void sxp_release(sexp* x) {
  R_ReleaseObject(x);
}

template <void (*Shield)(sexp* x), void (*Unshield)(sexp* x)>
struct shield_t
{
  sexp* s;

  shield_t(sexp* x) : s{ x } { Shield(s); }
  ~shield_t() { Unshield(s); }

  template <typename T>
  shield_t(const T& x) : s{ (sexp*) x } { Shield(s); }

  template <typename T>
  shield_t& operator= (const T& x) {
    s = (sexp*) x;
    Shield(s);
    return *this;
  }

  template <typename T>
  shield_t& operator= (const T&& x) {
    s = (sexp*) x;
    Shield(s);
    return *this;
  }

  operator sexp* () const { return s; }
};

using preserved_t = shield_t<&sxp_preserve, &sxp_release>;
using protected_t = shield_t<&sxp_protect, &sxp_unprotect>;


// Maybe not necessary?
struct r_vector_base
{
  sexp* s;
  r_vector_base(sexp* x) : s{ x }
  { }
  operator sexp* () const { return s; }
};

template <typename T>
struct r_vector : protected r_vector_base
{ };

template <>
struct r_vector<bool> : protected r_vector_base
{
  r_vector<bool>(sexp* x) : r_vector_base(x)
  {
    if (TYPEOF(x) != LGLSXP)
      Rcpp::stop("vctr type error");
  }
};

template <>
struct r_vector<int> : protected r_vector_base
{
  r_vector<int>(sexp* x) : r_vector_base(x)
  {
    if (TYPEOF(x) != INTSXP)
      Rcpp::stop("vctr type error");
  }
};

struct r_factor : protected r_vector<int>
{
  r_factor(sexp* x) : r_vector<int>(x)
  {
    if (!Rf_isFactor(x))
      Rcpp::stop("vctr type error");
  }
};


// Note that CHARSXP objects don't need GC protection because they are
// cached within a VECSXP.
struct r_string {
  sexp* s;

  r_string(sexp* x) : s{ x }
  {
    if (TYPEOF(x) != CHARSXP)
      Rcpp::stop("Expected an R string");
  }
  r_string(const char* x)
  {
    s = Rf_mkChar(x);
  }
  r_string(std::string x)
  {
    s = Rf_mkChar(x.c_str());
  }

  operator sexp* () const { return s; }

  template <typename T>
  r_string operator= (T y) {
    Rcpp::stop("R strings cannot be reassigned");
  }
};

/*

  Rcpp iterator proxy:

  - https://github.com/RcppCore/Rcpp/blob/master/inst/include/Rcpp/vector/proxy.h
    Dispatch proxies

  - https://github.com/RcppCore/Rcpp/blob/master/inst/include/Rcpp/internal/Proxy_Iterator.h
    Common proxy structure

  - https://github.com/RcppCore/Rcpp/blob/master/inst/include/Rcpp/vector/string_proxy.h
    String proxy

*/

// http://www.gotw.ca/publications/mill09.htm
// http://ericniebler.com/2015/01/28/to-be-or-not-to-be-an-iterator/
// Here cast-slicing to r_string is desirable
struct r_string_elt : r_string {
  sexp* vec;
  int vec_index;

  r_string_elt(sexp* x, sexp* vec_, int index)
      : r_string{ x },
        vec{ vec_ },
        vec_index{ index }
  { }

  template <typename T>
  r_string operator= (T y) {
    r_string str { ((std::string) y).c_str() };
    set_string_elt(vec, vec_index, str);
    return str;
  }

  r_string operator= (const char* y) {
    r_string str { y };
    set_string_elt(vec, vec_index, str);
    return str;
  }
};

template <>
struct r_vector<r_string> : protected r_vector_base
{
  r_vector<r_string>(sexp* x) : r_vector_base{ x }
  {
    if (TYPEOF(x) != STRSXP)
      Rcpp::stop("Expected an R string vector");
  }
  r_vector<r_string>(const char* x) : r_vector_base{ Rf_mkString(x) }
  { }
  r_vector<r_string>(std::string x) : r_vector_base{ Rf_mkString(x.c_str()) }
  { }
  r_vector<r_string>(std::vector<std::string> x)
      : r_vector_base{ vec_alloc<r_string>(x.size()) }
  {
    int i = 0;
    for (auto& elt : x) {
      set_string_elt(s, i, (sexp*) r_string{ elt.c_str() });
      ++i;
    }
  }
  r_vector<r_string>(std::initializer_list<const char*> x)
      : r_vector_base{ vec_alloc<r_string>(x.size()) }
  {
    int i = 0;
    for (auto& elt : x) {
      set_string_elt(s, i, (sexp*) r_string{ elt });
      ++i;
    }
  }

  operator sexp* () const { return s; }

  r_string_elt operator[] (const int i) {
    return { get_string_elt(s, i), *this, i };
  }
};


// r_vector<bool> x;
// r_vector<int> y;
// r_vector<double> z;
// r_vector<r_string*> z;
// r_vector<sexp*> z;
// r_factor z;


} // namespace vctrs
