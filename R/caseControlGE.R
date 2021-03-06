#' Semiparametric Gene-Environment Interactions in Case-Control Studies
#'
#' An R package for analysis of gene-environment interactions in case-control studies,
#' using distribution-free retrospective methodology.  The method of Stalder et. al. (2017)
#' and the improvement suggested by Wang et. al (2018) use a retrospective likelihood
#' framework under the assumption of gene-environment independence (in the population)
#' to gain efficiency when estimating the interaction effects of genetic and environmental
#' variables.
#'
#' Both methods treat the genetic and environmental variables nonparametrically,
#' facilitating the analysis of polygenic risk factors for which distributional assumptions
#' are difficult to justify.
#'
#' @section Keywords:
#' case-control study; gene-environment interaction; genetic epidemiology; retrospective method;
#' semiparametric analysis; pseudolikelihood; polygenic analysis
#'
#' @section Contents:
#' \describe{
#'   \item{\code{\link{spmle}}}{implements method of Stalder et. al. (2017).  Given
#'     binary response \code{D} (disease status), a vector or matrix of genetic
#'     risk factors \code{G}, a vector or matrix of environmental risks \code{E},
#'     and the population disease rate \code{pi1}, \code{spmle} fits a model of the
#'     form \code{D ~ G * E} by maximizing the retrospective pseudolikelihood.}
#'   \item{\code{\link{spmleCombo}}}{implements the method of of Wang et. al (2018)
#'     under the same set of assumptions as \code{spmle}.  This function takes the
#'     same imput as \code{spmle} with the addition of \code{nboot} (number of
#'     bootstrap samples) and \code{ncores} (number of CPU cores to use simultaneously).
#'     \code{spmleCombo} produces estimates that, on average, have significantly
#'     smaller mean squared error than \code{spmle}, at the cost of increased
#'     computation to calculate the bootstrap standard error.}
#'   \item{\code{\link{simulateCC}}}{simulates case-control data with a wide range
#'     of possible genetic and environmental variables.}
#'   \item{methods for class \code{"spmle"}}{both \code{spmleCombo} and \code{spmle}
#'     return objects of class \code{"spmle"}.  A range of S3 methods are provided:
#'     \code{anova.spmle}, \code{confint.spmle}, \code{logLik.spmle},
#'     \code{model.matrix.spmle}, \code{plot.spmle}, \code{\link{predict.spmle}},
#'     \code{print.spmle}, \code{print.summary.spmle}, \code{summary.spmle}, \code{vcov.spmle}}.
#' }
#'
#' @section References:
#' Stalder, O., Asher, A., Liang, L., Carroll, R. J., Ma, Y., and Chatterjee, N. (2017).
#' \emph{Semi-parametric analysis of complex polygenic gene-environment interactions in case-control studies.}
#' Biometrika, 104, 801–812.
#'
#' Wang, T., Asher, A., Carroll, R. J. (2018).
#' \emph{Improved Semiparametric Analysis of Polygenic Gene-Environment Interactions in Case-Control Studies}
#' Unpublished.
#'
#' @useDynLib caseControlGE
#' @importFrom Rcpp evalCpp
#' @importFrom utils head
#' @importFrom stats binomial coef cor cov glm lm model.matrix plogis qlogis qnorm rbinom rgamma rnorm rt runif sd model.frame setNames .checkMFClasses complete.cases delete.response fitted formula logLik model.matrix.lm na.pass naprint nobs pchisq pnorm printCoefmat qt quantile residuals symnum terms vcov
#' @importFrom graphics abline panel.smooth plot
"_PACKAGE"
