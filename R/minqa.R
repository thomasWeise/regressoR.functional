#' @include FittedFunctionalModel.R
#' @include utils.R
#' @include tools.R

#' @title Use Powell's BOBYQA Approach to Optimize the Parameters
#'
#' @description Apply Powell's BOBYQA Approach to fit a functional model.
#'
#' @param metric an instance of
#'   \code{regressoR.quality::RegressionQualityMetric}
#' @param model an instance of \code{\link{FunctionalModel}}
#' @param par the initial starting point
#' @return On success, an instance of \code{\link{FittedFunctionalModel}}.
#'   \code{NULL} on failure.
#' @importFrom minqa bobyqa newuoa
#' @importFrom learnerSelectoR learning.checkQuality
#' @importClassesFrom regressoR.quality RegressionQualityMetric
#' @importFrom regressoR.functional.models FunctionalModel.par.estimate
#'   FunctionalModel.par.check
#' @export FunctionalModel.fit.minqa
FunctionalModel.fit.minqa <- function(metric, model, par=NULL) {
  if(is.null(metric) || is.null(model) ) { return(NULL); }

  if(is.null(par)) {
    par <- FunctionalModel.par.estimate(model, metric@x, metric@y);
  }

  fn <- function(par) metric@quality(model@f, par);

  limits <- .fix.boundaries(model);
  if(is.null(limits)) {
    lower <- NULL;
    upper <- NULL;
  } else {
    lower <- limits$lower;
    upper <- limits$upper;
  }

  .ignore.errors({
    result <- NULL;

    .ignore.errors({
      if(is.null(lower)) {
        if(is.null(upper)) {
          result <- bobyqa(par=par, fn=fn);
        } else {
          result <- bobyqa(par=par, fn=fn, upper=upper);
        }
      } else {
        if(is.null(upper)) {
          result <- bobyqa(par=par, fn=fn, lower=lower);
        } else {
          result <- bobyqa(par=par, fn=fn, lower=lower, upper=upper);
        }
      }
      });

    if(!is.null(result)) {
      resultpar <- result$par;
      if(FunctionalModel.par.check(model, resultpar)) {
        resultq <- result$fval;
        if(learning.checkQuality(resultq)) {
          return(FittedFunctionalModel.new(model, resultpar, resultq));
        }
      }
    }

    result <- newuoa(par=par, fn=fn);
    if(!is.null(result)) {
      resultpar <- result$par;
      if(FunctionalModel.par.check(model, resultpar)) {
        resultq <- result$fval;
        if(learning.checkQuality(resultq)) {
          return(FittedFunctionalModel.new(model, resultpar, resultq));
        }
      }
    }
  });

  return(NULL);
}
