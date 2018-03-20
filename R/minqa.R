#' @include FittedFunctionalModel.R
#' @include utils.R
#' @include tools.R


#' @title Use Powell's BOBYQA Approach to Optimize the Parameters
#'
#' @description Apply Powell's BOBYQA Approach to fit a functional model.
#'
#' @param metric an instance of \code{regressoR.quality::RegressionQualityMetric}
#' @param model an instance of \code{\link{FunctionalModel}}
#' @param par the initial starting point
#' @return On success, an instance of
#'   \code{\link{FittedFunctionalModel}}. \code{NULL} on failure.
#' @importFrom minqa bobyqa newuoa
#' @importFrom learnerSelectoR learning.checkQuality
#' @importClassesFrom regressoR.quality RegressionQualityMetric
#' @importFrom regressoR.functional.models par.estimate par.check
#' @export model.fit.minqa
model.fit.minqa <- function(metric, model, par=NULL) {
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
    result <- NULL;

    .ignore.errors({
      if(base::is.null(lower)) {
        if(base::is.null(upper)) {
          result <- minqa::bobyqa(par=par, fn=fn);
        } else {
          result <- minqa::bobyqa(par=par, fn=fn, upper=upper);
        }
      } else {
        if(base::is.null(upper)) {
          result <- minqa::bobyqa(par=par, fn=fn, lower=lower);
        } else {
          result <- minqa::bobyqa(par=par, fn=fn, lower=lower, upper=upper);
        }
      }
      });

    if(!base::is.null(result)) {
      resultpar <- result$par;
      if(regressoR.functional.models::par.check(model, resultpar)) {
        resultq <- result$fval;
        if(learnerSelectoR::learning.checkQuality(resultq)) {
          return(FittedFunctionalModel.new(model, resultpar, resultq));
        }
      }
    }

    result <- minqa::newuoa(par=par, fn=fn);
    if(!base::is.null(result)) {
      resultpar <- result$par;
      if(regressoR.functional.models::par.check(model, resultpar)) {
        resultq <- result$fval;
        if(learnerSelectoR::learning.checkQuality(resultq)) {
          return(FittedFunctionalModel.new(model, resultpar, resultq));
        }
      }
    }
  });

  return(NULL);
}
