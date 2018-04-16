#' @include FittedFunctionalModel.R
#' @include tools.R

#' @title Apply a Levenberg-Marquardt Algorithm to Fit a Functional Model
#'
#' @description Apply the Levenberg-Marquardt algorithm to fit a functional
#'   model.
#'
#' @param metric an instance of
#'   \code{regressoR.quality::RegressionQualityMetric}
#' @param model an instance of \code{\link{FunctionalModel}}
#' @param par the initial starting point
#' @param q the effort to spent in learning, a value between 0 (min) and 1
#'   (max). Higher values may lead to much more computational time, lower values
#'   to potentially lower result quality.
#' @return On success, an instance of \code{\link{FittedFunctionalModel}}.
#'   \code{NULL} on failure.
#' @importFrom minpack.lm nls.lm
#' @importFrom learnerSelectoR learning.checkQuality
#' @importClassesFrom regressoR.quality RegressionQualityMetric
#' @importFrom regressoR.functional.models FunctionalModel.par.estimate
#'   FunctionalModel.par.check
#' @export FunctionalModel.fit.nlslm
FunctionalModel.fit.nlslm <- function(metric, model, par=NULL, q=0.75) {
  if(is.null(metric) || is.null(model) ||
     is.null(metric@residuals)) { return(NULL); }

  if(is.null(par)) {
    par <- FunctionalModel.par.estimate(model, metric@x, metric@y);
  }

  fn <- function(par) metric@residuals(model@f, par);

  if(!(is.null(model@gradient) || is.null(metric@jacobian))) {
    jac <- function(par) { metric@jacobian(model@gradient, par); }
  } else {
    jac <- NULL;
  }

  limits <- .fix.boundaries(model, par=par);
  if(is.null(limits)) {
    lower <- NULL;
    upper <- NULL;
  } else {
    lower <- limits$lower;
    upper <- limits$upper;
  }

  ignoreErrors({
    result <- nls.lm(par=par, lower=lower, upper=upper, fn=fn, jac=jac);
    if(is.null(result)) { return(NULL); }
    if(!(FunctionalModel.par.check(model, result$par))) { return(NULL); }
    if(!(is.finite(result$deviance))) { return(NULL); }
    quality <- metric@quality(model@f, result$par);
    if(!(learning.checkQuality(quality))) { return(NULL); }
    return(FittedFunctionalModel.new(model, result$par, quality));
  });

  return(NULL);
}
