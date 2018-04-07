#' @include FittedFunctionalModel.R
#' @include utils.R
#' @include tools.R

#' @title Use the \code{\link{optim}} Method from the \code{stats} Package for Fitting a Model
#'
#' @description Apply one of the algorithms provided by the \code{\link{optim}}
#'   method from the \code{stats} package. Although the \code{\link{optim}}
#' method should support using gradients, I just did not find out how to get
#' this to work.
#'
#' @param metric an instance of
#'   \code{regressoR.quality::RegressionQualityMetric}
#' @param model an instance of \code{\link{FunctionalModel}}
#' @param par the initial starting point
#' @param q the effort to spent in learning, a value between 0 (min) and 1
#'   (max). Higher values may lead to much more computational time, lower values
#'   to potentially lower result quality.
#' @param method the method to apply, see the documentation of
#'   \code{\link{optim}}
#' @return On success, an instance of \code{\link{FittedFunctionalModel}}.
#'   \code{NULL} on failure.
#' @importFrom stats optim
#' @importFrom learnerSelectoR learning.checkQuality
#' @importClassesFrom regressoR.quality RegressionQualityMetric
#' @importFrom regressoR.functional.models FunctionalModel.par.estimate
#'   FunctionalModel.par.check
#' @export FunctionalModel.fit.optim
FunctionalModel.fit.optim <- function(metric, model, par=NULL, q=0.75, method="L-BFGS-B") {
  if(is.null(metric) || is.null(model) ) { return(NULL); }

  if(is.null(par)) {
    par <- FunctionalModel.par.estimate(model, metric@x, metric@y);
  }

  limits <- .fix.boundaries(model);
  if(is.null(limits)) {
    lower <- NULL;
    upper <- NULL;
  } else {
    lower <- limits$lower;
    upper <- limits$upper;
  }

  fn <- function(par) metric@quality(model@f, par);

  # for some reason, I cannot get this to work with using a gradient

  .ignore.errors({
    result <- NULL;
    if(is.null(lower)) {
      if(is.null(upper)) {
        .ignore.errors({ result <- optim(par=par, fn=fn, method=method) });
      } else {
        .ignore.errors({ result <- optim(par=par, fn=fn, upper=upper, method=method) });
      }
    } else {
      if(is.null(model@paramUpper)) {
        .ignore.errors({ result <- optim(par=par, fn=fn, lower=lower, method=method) });
      } else {
        .ignore.errors({ result <- optim(par=par, fn=fn, lower=lower, upper=upper, method=method) });
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


#' @title Use the L-BFGS-B Algorithm from the \code{\link{optim}} Method of the
#'   \code{stats} Package for Fitting a Model
#'
#' @description Apply the "L-BFGS-B" algorithm provided by the
#'   \code{\link{optim}} method from the \code{stats} package. Although the
#'   \code{\link{optim}} method should support using gradients, I just did not
#'   find out how to get this to work.
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
#' @export FunctionalModel.fit.lbfgsb
FunctionalModel.fit.lbfgsb <- function(metric, model, par=NULL, q=0.75)
     FunctionalModel.fit.optim(metric=metric, model=model, par=par, q=q, method="L-BFGS-B")
