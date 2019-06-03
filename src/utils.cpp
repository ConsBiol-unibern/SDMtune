
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix scaleClamp(NumericMatrix x, NumericVector min, NumericVector max,
                        LogicalVector do_clamp, LogicalVector scale) {
  int ncols = x.ncol();
  for (int i = 0; i < ncols; i++) {
    if (do_clamp[0] == TRUE) {
      x( _, i ) = clamp( min[i], x( _, i ), max[i] );
    }
    if (scale[0] == TRUE) {
      x( _, i ) = ( x( _, i ) - min[i] ) / ( max[i] - min[i] );
    }
  }
  return x;
}
