#' @include FittedFunctionalModel.R
#' @include tools.R

#' @title Use Derivative-Free Local Searches to Optimize the Parameters
#'
#' @description Apply two derivative-free algorithms, namely Hook-Jeeves and
#'   Nelder-Mead, to fit a functional model.
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
#' @importFrom dfoptim hjk nmk hjkb nmkb
#' @importFrom learnerSelectoR learning.checkQuality
#' @importClassesFrom regressoR.quality RegressionQualityMetric
#' @importFrom regressoR.functional.models FunctionalModel.par.estimate
#'   FunctionalModel.par.check
#' @importFrom utilizeR ignoreErrors
#' @export FunctionalModel.fit.dfoptim
FunctionalModel.fit.dfoptim <- function(metric, model, par=NULL, q=0.75) {
  if(is.null(metric) || is.null(model) ) { return(NULL); }

  if(is.null(par)) {
    par <- FunctionalModel.par.estimate(model, metric@x, metric@y);
  }

  limits <- .fix.boundaries(model, par=par);
  if(is.null(limits)) {
    lower <- NULL;
    upper <- NULL;
  } else {
    lower <- limits$lower;
    upper <- limits$upper;
  }

  fn <- function(par) metric@quality(model@f, par);

  .ignoreErrors({
    control <- list(maxfeval=2000);
    result1 <- NULL;
    result2 <- NULL;
    if(is.null(lower)) {
      if(is.null(upper)) {
        .ignoreErrors({ result1 <- hjk(par=par, fn=fn, control=control) });
        .ignoreErrors({ result2 <- nmk(par=par, fn=fn, control=control) });
      } else {
        .ignoreErrors({ result1 <- hjkb(par=par, fn=fn, upper=upper, control=control) });
        .ignoreErrors({ result2 <- nmkb(par=par, fn=fn, upper=upper, control=control) });
      }
    } else {
      if(is.null(upper)) {
        .ignoreErrors({ result1 <- hjkb(par=par, fn=fn, lower=lower, control=control) });
        .ignoreErrors({ result2 <- nmkb(par=par, fn=fn, lower=lower, control=control) });
      } else {
        .ignoreErrors({ result1 <- hjkb(par=par, fn=fn, lower=lower, upper=upper, control=control) });
        .ignoreErrors({ result2 <- nmkb(par=par, fn=fn, lower=lower, upper=upper, control=control) });
      }
    }

    if(is.null(result1) && is.null(result2)) { return(NULL); }

    if(is.null(result1)) {
      result1par <- NULL;
      result1q <- +Inf;
    } else {
      result1par <- result1$par;
      if(!(FunctionalModel.par.check(model, result1par))) {
        result1par <- NULL;
        result1q <- +Inf;
      } else {
        result1q <- result1$value;
        if(!(learning.checkQuality(result1q))) {
          result1par <- NULL;
          result1q <- +Inf;
        }
      }
    }

    if(is.null(result2)) {
      result2par <- NULL;
      result2q <- +Inf;
    } else {
      result2par <- result2$par;
      if(!(FunctionalModel.par.check(model, result2par))) {
        result2par <- NULL;
        result2q <- +Inf;
      } else {
        result2q <- result2$value;
        if(!(learning.checkQuality(result2q))) {
          result2par <- NULL;
          result2q <- +Inf;
        }
      }
    }

    if(is.null(result1par) && is.null(result2par)) { return(NULL); }

    if(is.null(result1par)) {
      result1par <- result2par;
      result1q <- result2q;
    } else {
      if( (!(is.null(result2par))) && (result2q < result1q) ) {
        result1par <- result2par;
        result1q <- result2q;
      }
    }

    return(FittedFunctionalModel.new(model, result1par, result1q));
  });

  return(NULL);
}
