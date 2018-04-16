#' @include FittedFunctionalModel.R
#' @include tools.R

#' @title Use CMA-ES to Optimize the Parameters
#'
#' @description Apply the CMA-ES algorithm to fit a functional model.
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
#' @importFrom cmaes cma_es
#' @importFrom learnerSelectoR learning.checkQuality
#' @importClassesFrom regressoR.quality RegressionQualityMetric
#' @importFrom regressoR.functional.models FunctionalModel.par.estimate
#'   FunctionalModel.par.check
#' @export FunctionalModel.fit.cmaes
#' @importFrom utilizeR ignoreErrors
FunctionalModel.fit.cmaes <- function(metric, model, par=NULL, q=0.75) {
  if(is.null(metric) || is.null(model) ) { return(NULL); }

  if(is.null(par)) {
    par <- FunctionalModel.par.estimate(model, metric@x, metric@y);
  }

  fn <- function(par) metric@quality(model@f, par);

  limits <- .fix.boundaries(model, par=par);
  if(is.null(limits)) {
    lower <- NULL;
    upper <- NULL;
  } else {
    lower <- limits$lower;
    upper <- limits$upper;
  }

  ignoreErrors({
    if(is.null(lower)) {
      if(is.null(upper)) {
        result <- cma_es(par=par, fn=fn);
      } else {
        result <- cma_es(par=par, fn=fn, upper=upper);
      }
    } else {
      if(is.null(upper)) {
        result <- cma_es(par=par, fn=fn, lower=lower);
      } else {
        result <- cma_es(par=par, fn=fn, lower=lower, upper=upper);
      }
    }

    if(is.null(result)) { return(NULL); }
    if(!(FunctionalModel.par.check(model, result$par))) { return(NULL); }
    if(!(learning.checkQuality(result$value))) { return(NULL); }
    return(FittedFunctionalModel.new(model, result$par, result$value));
  });

  return(NULL);
}
