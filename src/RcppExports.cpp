// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// fitPiecewiseBondCurve
Rcpp::DataFrame fitPiecewiseBondCurve(std::vector<Rcpp::Date> maturityDates, std::vector<double> cleanPrices, std::vector<double> coupons, Rcpp::Date startDate, Rcpp::List config);
RcppExport SEXP _FIRVr_fitPiecewiseBondCurve(SEXP maturityDatesSEXP, SEXP cleanPricesSEXP, SEXP couponsSEXP, SEXP startDateSEXP, SEXP configSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<Rcpp::Date> >::type maturityDates(maturityDatesSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type cleanPrices(cleanPricesSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type coupons(couponsSEXP);
    Rcpp::traits::input_parameter< Rcpp::Date >::type startDate(startDateSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type config(configSEXP);
    rcpp_result_gen = Rcpp::wrap(fitPiecewiseBondCurve(maturityDates, cleanPrices, coupons, startDate, config));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_FIRVr_fitPiecewiseBondCurve", (DL_FUNC) &_FIRVr_fitPiecewiseBondCurve, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_FIRVr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}