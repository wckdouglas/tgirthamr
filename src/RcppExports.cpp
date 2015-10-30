// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/tgirthamr.h"
#include <Rcpp.h>

using namespace Rcpp;

// fdr_control
NumericVector fdr_control(NumericVector p, double alpha);
RcppExport SEXP tgirthamr_fdr_control(SEXP pSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type p(pSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    __result = Rcpp::wrap(fdr_control(p, alpha));
    return __result;
END_RCPP
}
// string_split
stringList string_split(stringList x, char sep, int num);
RcppExport SEXP tgirthamr_string_split(SEXP xSEXP, SEXP sepSEXP, SEXP numSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< stringList >::type x(xSEXP);
    Rcpp::traits::input_parameter< char >::type sep(sepSEXP);
    Rcpp::traits::input_parameter< int >::type num(numSEXP);
    __result = Rcpp::wrap(string_split(x, sep, num));
    return __result;
END_RCPP
}
// mergeTypeList
stringList mergeTypeList(stringList realbase);
RcppExport SEXP tgirthamr_mergeTypeList(SEXP realbaseSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< stringList >::type realbase(realbaseSEXP);
    __result = Rcpp::wrap(mergeTypeList(realbase));
    return __result;
END_RCPP
}
// heterozygote
vector<double> heterozygote(NumericVector A, NumericVector C, NumericVector T, NumericVector G, NumericVector cov);
RcppExport SEXP tgirthamr_heterozygote(SEXP ASEXP, SEXP CSEXP, SEXP TSEXP, SEXP GSEXP, SEXP covSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type A(ASEXP);
    Rcpp::traits::input_parameter< NumericVector >::type C(CSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type T(TSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type G(GSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type cov(covSEXP);
    __result = Rcpp::wrap(heterozygote(A, C, T, G, cov));
    return __result;
END_RCPP
}
// transformDF
DataFrame transformDF(DataFrame df, double seqErr, double pCutOff, Function binom);
RcppExport SEXP tgirthamr_transformDF(SEXP dfSEXP, SEXP seqErrSEXP, SEXP pCutOffSEXP, SEXP binomSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< DataFrame >::type df(dfSEXP);
    Rcpp::traits::input_parameter< double >::type seqErr(seqErrSEXP);
    Rcpp::traits::input_parameter< double >::type pCutOff(pCutOffSEXP);
    Rcpp::traits::input_parameter< Function >::type binom(binomSEXP);
    __result = Rcpp::wrap(transformDF(df, seqErr, pCutOff, binom));
    return __result;
END_RCPP
}
// transformPredict
DataFrame transformPredict(DataFrame df, double seqErr, double pCutOff, Function binom);
RcppExport SEXP tgirthamr_transformPredict(SEXP dfSEXP, SEXP seqErrSEXP, SEXP pCutOffSEXP, SEXP binomSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< DataFrame >::type df(dfSEXP);
    Rcpp::traits::input_parameter< double >::type seqErr(seqErrSEXP);
    Rcpp::traits::input_parameter< double >::type pCutOff(pCutOffSEXP);
    Rcpp::traits::input_parameter< Function >::type binom(binomSEXP);
    __result = Rcpp::wrap(transformPredict(df, seqErr, pCutOff, binom));
    return __result;
END_RCPP
}
