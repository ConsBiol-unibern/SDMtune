
#include <Rcpp.h>
using namespace Rcpp;

//' One Hot Encoding
//'
//' Converts a factor in One Hot matrix
//' @param x factor. Factor to be encoded
//' @param levels character. Levels to be encoded.
//'
//' @return matrix with the One Hot encoded values
//' @export
//'
//' @examples one_hot(1:10, c("1", "7"))
//' @author Sergio Vignali
//'
// [[Rcpp::export]]
IntegerMatrix one_hot(NumericVector x, CharacterVector levels) {

  int rows = x.size(), cols = levels.size();
  IntegerMatrix one_hot( rows, cols );

  for( int i = 0; i < cols; ++i ) {
    one_hot( _, i ) = x == atoi( levels[i] );
  }

  return one_hot ;
}

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
