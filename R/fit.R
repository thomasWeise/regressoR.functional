#' @include FittedFunctionalModel.R
#' @include defaultFitters.R
#' @include tools.R
#' @include utils.R

#' @title Fit the Given Model Blueprint to the Specified Data
#'
#' @description Apply a set of fitters iteratively to fit the specified model to
#'   the given data. First, we generate a starting guess about the
#'   parameterization via \code{\link{par.estimate}} (or accept it
#'   via the parameter \code{par}). From then on, we apply the different
#'   function fitters one by one. All the fitters who have not produced the
#'   current best solution are applied again, to the now-best guess. However, we
#'   do not apply the fitters that have produced that very guess in the next
#'   round. (They may get a chance again in a later turn.) Anyway, this
#'   procedure is iterated until no improvement can be made anymore. After
#'   finishing the fitting, we attempt whether rounding the fitted parameters to
#'   integers can improve the fitting quality.
#'
#' @param metric an instance of \code{regressoR.quality::RegressionQualityMetric}
#' @param model an instance of \code{\link{FunctionalModel}}
#' @param par the initial starting point
#' @param fitters the fitters
#' @return On success, an instance of
#'   \code{\link{FittedFunctionalModel}}. \code{NULL} on failure.
#' @export model.fit
#' @importFrom learnerSelectoR learning.checkQuality
#' @importFrom regressoR.functional.models par.check par.estimate
#' @examples
#'
#' set.seed(12555)
#' x <- (1:201 - 100) * 0.1;
#' fxpar <- function(x, par) par[1] + x * (par[2] + x * (par[3]));
#' par <- c(4, -3, 2);
#' fx <- function(x) fxpar(x, par);
#' y <- fx(x);
#' model <- regressoR.functional.models::quadratic();
#' metric1 <- regressoR.quality::default(x, y);
#' res1 <- regressoR.functional::model.fit(metric1, model);
#' res1@quality
#' # [1] 0
#' res1@par
#' # [1]  4 -3  2
#'
#' xr <- x + 0.1*rnorm(length(x));
#' yr <- y + 0.1*rnorm(length(y));
#' metric2 <- regressoR.quality::default(xr, yr);
#' res2 <- regressoR.functional::model.fit(metric2, model);
#' res2@quality
#' # [1] 0.2439082
#' res2@par
#' # [1]  3.919365 -2.938202  1.998102
model.fit <- function(metric, model, par=NULL,
                      fitters = model.fit.defaultFitters(base::length(metric@x), model@paramCount)) {
  if(base::is.null(metric) || base::is.null(model)
                           || base::is.null(fitters)) { return(NULL); }

  fitterCount <- base::length(fitters);
  if(fitterCount <= 0L) { return(NULL); }

  # get a starting point
  bestParams <- regressoR.functional.models::par.estimate(model, metric, par);
  if(regressoR.functional.models::par.check(model, bestParams)) {
    bestQuality <- metric@quality(model@f, bestParams);
    if(!(learnerSelectoR::learning.checkQuality(bestQuality))) {
      bestQuality <- +Inf;
      bestParams <- NULL;
    }
  } else {
    bestQuality <- Inf;
    bestParams <- NULL;
  }
  bestResult <- NULL;

  # we can apply all algorithms
  canUse <- base::rep(TRUE, fitterCount);

  # apply a fitter
  applyFunc <- function(index) {
    if(canUse[index]) {
      res <- (fitters[[index]](metric=metric, model=model,
                               par=bestParams));
      if((!(base::is.null(res))) && (res@quality < bestQuality)) {
        return(res);
      }
    }
    return(NULL);
  }

  # In the next iteration, we only apply fitters which did not already see the current solution
  canUseFun <- function(x) (base::is.null(x) || (x@quality > bestQuality))

  improved = TRUE;
  while(improved) {
    # apply all fitters
    current <- base::sapply(X=1L:fitterCount, FUN=applyFunc);

    # was there any improvement?
    improved = FALSE;
    for(res in current) {
      if((!(base::is.null(res))) && (res@quality < bestQuality)) {
        improved = TRUE;
        bestResult <- res;
        bestQuality <- res@quality;
        bestParams <- res@par;
      }
    }

    # was there any fitter that did not yet receive the current solution as input?
    canUse <- base::vapply(X=current, FUN=canUseFun, FUN.VALUE = FALSE);
  }

  if(base::is.null(bestResult) &&
     regressoR.functional.models::par.check(model, bestParams) &&
     learning.checkQuality(bestQuality)) {
    # strange, ok, let's try to build a new solution
    return(FittedFunctionalModel.new(model, bestParams, bestQuality));
  }

  if(base::is.null(bestResult)) {
    # No dice: we simply could not make the model fit in any way
    return(NULL);
  }

  # OK, now we try to integer-fy all parameters
  bestCopy <- bestParams;
  changed <- FALSE;
  improved <- TRUE;
  while(improved) {
    improved <- FALSE;
    for(i in 1:base::length(bestParams)) {
      x <- bestParams[i];
      xr <- base::round(x);
      if(xr != x) {
        if((xr >= (-.Machine$integer.max)) && (xr <= .Machine$integer.max)) {
          xr <- base::as.integer(xr);
        }
        bestCopy[i] <- xr;
        if(regressoR.functional.models::par.check(model, bestCopy)) {
          test <- metric@quality(model@f, bestCopy);
          if(learnerSelectoR::learning.checkQuality(test) && (test <= bestQuality)) {
            bestQuality <- test;
            bestParams <- bestCopy;
            changed <- TRUE;
            improved <- TRUE;
          }
        }
      }
    }
  }
  # now let's try all parameters at once
  improved <- FALSE;
  for(i in 1:base::length(bestParams)) {
    x <- bestParams[i];
    xr <- base::round(x);
    if(xr != x) {
      if((xr >= (-.Machine$integer.max)) && (xr <= .Machine$integer.max)) {
        xr <- base::as.integer(xr);
      }
      bestCopy[i] <- xr;
      improved <- TRUE;
    }
  }
  if(improved && regressoR.functional.models::par.check(model, bestCopy)) {
    test <- metric@quality(model@f, bestCopy);
    if(learnerSelectoR::learning.checkQuality(test) && (test <= bestQuality)) {
      bestQuality <- test;
      bestParams <- bestCopy;
      changed <- TRUE;
    }
  }

  if(changed) {
    # OK, we have improved upon the result, return it.
    return(FittedFunctionalModel.new(model, bestParams, bestQuality));
  }

  return(bestResult);
}
