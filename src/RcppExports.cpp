// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// scaleClamp
NumericMatrix scaleClamp(NumericMatrix x, NumericVector min, NumericVector max, LogicalVector do_clamp, LogicalVector scale);
RcppExport SEXP _SDMtune_scaleClamp(SEXP xSEXP, SEXP minSEXP, SEXP maxSEXP, SEXP do_clampSEXP, SEXP scaleSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type min(minSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type max(maxSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type do_clamp(do_clampSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type scale(scaleSEXP);
    rcpp_result_gen = Rcpp::wrap(scaleClamp(x, min, max, do_clamp, scale));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_SDMtune_scaleClamp", (DL_FUNC) &_SDMtune_scaleClamp, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_SDMtune(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
