#' @include FittedFunctionalModel.R
#' @include tools.R

#' @title Use the \code{\link{psoptim}} Method from the \code{pso} Package for
#'   Fitting a Model
#'
#' @description Apply the Particle Swarm Optimization algorithm to fit the
#'   parameters of a model.
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
#' @importFrom pso psoptim
#' @importFrom learnerSelectoR learning.checkQuality
#' @importClassesFrom regressoR.quality RegressionQualityMetric
#' @importFrom regressoR.functional.models FunctionalModel.par.estimate
#'   FunctionalModel.par.check
#' @export FunctionalModel.fit.pso
#' @importFrom utilizeR ignoreErrors
FunctionalModel.fit.pso <- function(metric, model, par=NULL, q=0.75) {
  if(is.null(metric) || is.null(model) ) { return(NULL); }

  if(is.null(par)) {
    par <- FunctionalModel.par.estimate(model, metric@x, metric@y);
  }

  limits <- .fix.boundaries(model, par=par, need=TRUE);
  if(is.null(limits)) {
    lower <- NULL;
    upper <- NULL;
  } else {
    lower <- limits$lower;
    upper <- limits$upper;
  }

  fn <- function(par) metric@quality(model@f, par);

  # for some reason, I cannot get this to work with using a gradient

  ignoreError({
    result <- NULL;
    if(is.null(lower)) {
      if(is.null(upper)) {
        ignoreError({ result <- psoptim(par=par, fn=fn) });
      } else {
        ignoreError({ result <- psoptim(par=par, fn=fn, upper=upper) });
      }
    } else {
      if(is.null(upper)) {
        ignoreError({ result <- psoptim(par=par, fn=fn, lower=lower) });
      } else {
        ignoreError({ result <- psoptim(par=par, fn=fn, lower=lower, upper=upper) });
      }
    }

    if((!(is.null(result))) &&
          FunctionalModel.par.check(model, result$par) &&
          learning.checkQuality(result$value)) {
      return(FittedFunctionalModel.new(model, result$par, result$value));
    }
  });

  return(NULL);
}
