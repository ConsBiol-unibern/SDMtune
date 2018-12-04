
#include <Rcpp.h>
using namespace Rcpp;

//' Clamp
//'
//' Clamp a given vector between the provided lower and upper values.
//'
//' @param x numeric. Vector to be clamped.
//' @param lower numeric. The lowest value used to clamp.
//' @param upper numeric. The highest value used to clamp
//'
//' @details The code is taken from
//' \url{http://gallery.rcpp.org/articles/sugar-function-clamp/}.
//'
//' @return numeric. The clamped vector.
//' @export
//'
//' @examples clamp(1:20, 4, 17)
//'
//' @author Sergio Vignali
// [[Rcpp::export]]
NumericVector clamp(NumericVector x, double lower, double upper) {
  return clamp(lower, x, upper);
}
