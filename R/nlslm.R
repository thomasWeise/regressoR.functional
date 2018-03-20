#' @include FittedFunctionalModel.R
#' @include utils.R
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
#' @return On success, an instance of \code{\link{FittedFunctionalModel}}.
#'   \code{NULL} on failure.
#' @importFrom minpack.lm nls.lm
#' @importFrom learnerSelectoR learning.checkQuality
#' @importClassesFrom regressoR.quality RegressionQualityMetric
#' @importFrom regressoR.functional.models FunctionalModel.par.estimate
#'   FunctionalModel.par.check
#' @export FunctionalModel.fit.nlslm
FunctionalModel.fit.nlslm <- function(metric, model, par=NULL) {
  if(base::is.null(metric) || base::is.null(model) ||
     base::is.null(metric@residuals)) { return(NULL); }

  if(base::is.null(par)) {
    par <- regressoR.functional.models::FunctionalModel.par.estimate(model, metric);
  }

  fn <- function(par) metric@residuals(model@f, par);

  if(!(base::is.null(model@gradient) || base::is.null(metric@jacobian))) {
    jac <- function(par) { metric@jacobian(model@gradient, par); }
  } else {
    jac <- NULL;
  }

  limits <- .fix.boundaries(model);
  if(base::is.null(limits)) {
    lower <- NULL;
    upper <- NULL;
  } else {
    lower <- limits$lower;
    upper <- limits$upper;
  }

  .ignore.errors({
    result <- minpack.lm::nls.lm(par=par, lower=lower, upper=upper, fn=fn, jac=jac);
    if(base::is.null(result)) { return(NULL); }
    if(!(regressoR.functional.models::FunctionalModel.par.check(model, result$par))) { return(NULL); }
    if(!(base::is.finite(result$deviance))) { return(NULL); }
    quality <- metric@quality(model@f, result$par);
    if(!(learnerSelectoR::learning.checkQuality(quality))) { return(NULL); }
    return(FittedFunctionalModel.new(model, result$par, quality));
  });

  return(NULL);
}
