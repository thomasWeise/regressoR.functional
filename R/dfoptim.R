#' @include FittedFunctionalModel.R
#' @include utils.R
#' @include tools.R

#' @title Use Derivative-Free Local Searches to Optimize the Parameters
#'
#' @description Apply twi derivative-free algorithms, namely Hook-Jeeves and
#'   Nelder-Mead, to fit a functional model.
#'
#' @param metric an instance of \code{regressoR.quality::RegressionQualityMetric}
#' @param model an instance of \code{\link{FunctionalModel}}
#' @param par the initial starting point
#' @return On success, an instance of
#'   \code{\link{FittedFunctionalModel}}. \code{NULL} on failure.
#' @importFrom dfoptim hjk nmk
#' @importFrom learnerSelectoR learning.checkQuality
#' @importClassesFrom regressoR.quality RegressionQualityMetric
#' @importFrom regressoR.functional.models par.estimate par.check
#' @export model.fit.dfoptim
model.fit.dfoptim <- function(metric, model, par=NULL) {
  if(base::is.null(metric) || base::is.null(model) ) { return(NULL); }

  if(base::is.null(par)) {
    par <- regressoR.functional.models::par.estimate(model, metric);
  }

  limits <- .fix.boundaries(model);
  if(base::is.null(limits)) {
    lower <- NULL;
    upper <- NULL;
  } else {
    lower <- limits$lower;
    upper <- limits$upper;
  }

  fn <- function(par) metric@quality(model@f, par);

  .ignore.errors({
    control <- base::list(maxfeval=2000);
    result1 <- NULL;
    result2 <- NULL;
    if(base::is.null(lower)) {
      if(base::is.null(upper)) {
        .ignore.errors({ result1 <- dfoptim::hjk(par=par, fn=fn, control=control) });
        .ignore.errors({ result2 <- dfoptim::nmk(par=par, fn=fn, control=control) });
      } else {
        .ignore.errors({ result1 <- dfoptim::hjkb(par=par, fn=fn, upper=upper, control=control) });
        .ignore.errors({ result2 <- dfoptim::nmkb(par=par, fn=fn, upper=upper, control=control) });
      }
    } else {
      if(base::is.null(model@paramUpper)) {
        .ignore.errors({ result1 <- dfoptim::hjkb(par=par, fn=fn, lower=lower, control=control) });
        .ignore.errors({ result2 <- dfoptim::nmkb(par=par, fn=fn, lower=lower, control=control) });
      } else {
        .ignore.errors({ result1 <- dfoptim::hjkb(par=par, fn=fn, lower=lower, upper=upper, control=control) });
        .ignore.errors({ result2 <- dfoptim::nmkb(par=par, fn=fn, lower=lower, upper=upper, control=control) });
      }
    }

    if(base::is.null(result1) && base::is.null(result2)) { return(NULL); }

    if(base::is.null(result1)) {
      result1par <- NULL;
      result1q <- +Inf;
    } else {
      result1par <- result1$par;
      if(!(regressoR.functional.models::par.check(model, result1par))) {
        result1par <- NULL;
        result1q <- +Inf;
      } else {
        result1q <- result1$value;
        if(!(learnerSelectoR::learning.checkQuality(result1q))) {
          result1par <- NULL;
          result1q <- +Inf;
        }
      }
    }

    if(base::is.null(result2)) {
      result2par <- NULL;
      result2q <- +Inf;
    } else {
      result2par <- result2$par;
      if(!(regressoR.functional.models::par.check(model, result2par))) {
        result2par <- NULL;
        result2q <- +Inf;
      } else {
        result2q <- result2$value;
        if(!(learnerSelectoR::learning.checkQuality(result2q))) {
          result2par <- NULL;
          result2q <- +Inf;
        }
      }
    }

    if(base::is.null(result1par) && base::is.null(result2par)) { return(NULL); }

    if(base::is.null(result1par)) {
      result1par <- result2par;
      result1q <- result2q;
    } else {
      if( (!(base::is.null(result2par))) && (result2q < result1q) ) {
        result1par <- result2par;
        result1q <- result2q;
      }
    }

    return(FittedFunctionalModel.new(model, result1par, result1q));
  });

  return(NULL);
}
