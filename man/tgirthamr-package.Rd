\name{tgirthamr-package}
\alias{tgirthamr-package}
\alias{tgirthamr}
\docType{package}
\title{
  This package intends to speed up the analysis of TGIRT-HAMR. and is needed for the tgirt-hamr softwares
}
\description{
 This package contains all of the computationally intense functions for the anlayses of tgirt-hamr, such as table transformation, pvalues extractions, etc.
}
\details{
    The main function is transformPredict, which would takes in a table with mismatch counts, coverage and deletion counts for each aligned positions of tgirt-seq libraries and return a table with pvalues for the two hypotheses described in the paper.
    In additional to the trnasform function, I also included a function for fast FDR control taking in a vector of p-values and alpha value. Returning 1 (passed) or 0 (didnt pass) as controlling false discovery rate.
}
\author{
Douglas Wu, wckdouglas@gmail.com.

Maintainer: Douglas Wu <wckdouglas@gmail.com>
}
\references{

}
% Optionally other standard keywords, one per line,
% from the file KEYWORDS in the R documentation.
\keyword{ package }
\seealso{
  https://github.com/wckdouglas/tgirt-hamr
}
