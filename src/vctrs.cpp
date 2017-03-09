#include <Rcpp.h>
#include <vctrs.h>


using namespace vctrs;

// [[Rcpp::export]]
SEXP combine(SEXP x1, SEXP x2) {
  std::auto_ptr<Vctr> v1(VctrFactory::create(x1));
  VctrBuilder vb(*v1);
  std::auto_ptr<Vctr> v2(VctrFactory::create(x2));
  vb.append(*v2);

  return vb.get_vctr().get_sexp();
}

// // [[Rcpp::export]]
// SEXP concatenate_(const SEXP x, const SEXP y) {
//   return concatenate(x, y);
// }

// [[Rcpp::export]]
vctrs::sexp* mk_string(std::string x) {
  // return Rf_mkChar(x.c_str());
  // return r_string{ x.c_str() };
  // return r_vector<r_string>{ x.c_str() };
  // return r_vector<r_string>{ "hop", "hip" };

  // r_vector<r_string> y { "hop", "hip" };
  // y[1] = "hap";
  // // y[1] = x;
  // return y;

  // Expect failure:
  // r_string z = y[1];
  // z = "bim";

  // /* Can use algorithms on std::string */
  // std::string hip("couac");
  // std::fill(hip.begin(), hip.end(), 'b');
  // return r_string{hip};

  Rcpp::CharacterVector hop (10);
  std::fill(hop.begin(), hop.end(), "hips");

  // /* string proxy iterator is const char*, so doesn't work */
  // auto test = hop[1];
  // std::fill(test.begin(), test.end(), 'c');

  return hop;
}


// concept polymorphism: copy via clone()
//   Could offer rewrap()?

// https://stlab.adobe.com/wiki/images/c/c9/Boost_poly.pdf
