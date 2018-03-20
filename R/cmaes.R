#' @include FittedFunctionalModel.R
#' @include utils.R
#' @include tools.R

#' @title Use CMA-ES to Optimize the Parameters
#'
#' @description Apply the CMA-ES algorithm to fit a functional model.
#'
#' @param metric an instance of \code{regressoR.quality::RegressionQualityMetric}
#' @param model an instance of \code{\link{FunctionalModel}}
#' @param par the initial starting point
#' @return On success, an instance of
#'   \code{\link{FittedFunctionalModel}}. \code{NULL} on failure.
#' @importFrom cmaes cma_es
#' @importFrom learnerSelectoR learning.checkQuality
#' @importClassesFrom regressoR.quality RegressionQualityMetric
#' @importFrom regressoR.functional.models par.estimate par.check
#' @export model.fit.cmaes
model.fit.cmaes <- function(metric, model, par=NULL) {
  if(base::is.null(metric) || base::is.null(model) ) { return(NULL); }

  if(base::is.null(par)) {
    par <- regressoR.functional.models::par.estimate(model, metric);
  }

  fn <- function(par) metric@quality(model@f, par);

  limits <- .fix.boundaries(model);
  if(base::is.null(limits)) {
    lower <- NULL;
    upper <- NULL;
  } else {
    lower <- limits$lower;
    upper <- limits$upper;
  }

  .ignore.errors({
    if(base::is.null(lower)) {
      if(base::is.null(upper)) {
        result <- cmaes::cma_es(par=par, fn=fn);
      } else {
        result <- cmaes::cma_es(par=par, fn=fn, upper=upper);
      }
    } else {
      if(base::is.null(upper)) {
        result <- cmaes::cma_es(par=par, fn=fn, lower=lower);
      } else {
        result <- cmaes::cma_es(par=par, fn=fn, lower=lower, upper=upper);
      }
    }

    if(base::is.null(result)) { return(NULL); }
    if(!(regressoR.functional.models::par.check(model, result$par))) { return(NULL); }
    if(!(learnerSelectoR::learning.checkQuality(result$value))) { return(NULL); }
    return(FittedFunctionalModel.new(model, result$par, result$value));
  });

  return(NULL);
}
