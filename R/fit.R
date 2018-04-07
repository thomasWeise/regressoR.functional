#' @include FittedFunctionalModel.R
#' @include cmaes.R
#' @include minqa.R
#' @include nlslm.R
#' @include de.R
#' @include dfoptim.R
#' @include nls.R
#' @include optim.R
#' @include tools.R
#' @include utils.R

# the fitters to be used in a normal situation
.fitters <- c(
  FunctionalModel.fit.nlslm,
  FunctionalModel.fit.minqa,
  FunctionalModel.fit.lbfgsb,
  FunctionalModel.fit.nls
)

# the indexes of the fitters to use when we have a starting point
.fitters.for.starting.point     <- 1L:length(.fitters);
# the indexes of the fitters to use if we do not have a starting point: apply
# some fitters multiple times for more diversity (as each application gets a
# different, unique starting point)
.fitters.without.starting.point <- c(1L, 1L, 1L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L);

# the list of fitters to use if could not produce any feasible parameterization
.fitters.fallback <- unlist(c(
  FunctionalModel.fit.de,
  FunctionalModel.fit.dfoptim,
  .fitters,
  FunctionalModel.fit.cmaes
))

#' @title Fit the Given Model Blueprint to the Specified Data
#'
#' @description This is the general method to be used to fit a \code{\link{FunctionalModel}} to
#' a \code{\link{RegressionQualityMetric}}.
#'
#' @param metric an instance of \code{RegressionQualityMetric}
#' @param model an instance of \code{\link{FunctionalModel}}
#' @param par the initial starting point
#' @param q the effort to spent in learning, a value between 0 (min) and 1
#'   (max). Higher values may lead to much more computational time, lower values
#'   to potentially lower result quality.
#' @return On success, an instance of
#'   \code{\link{FittedFunctionalModel}}. \code{NULL} on failure.
#' @export FunctionalModel.fit
#' @importFrom learnerSelectoR learning.checkQuality
#' @importFrom regressoR.functional.models FunctionalModel.par.check FunctionalModel.par.estimate
FunctionalModel.fit <- function(metric, model, par=NULL, q=0.75) {
  #  is the input data valid?
  if(is.null(metric) || is.null(model)) { return(NULL); }

  if(is.null(par)) {
    # We have no starting point, so we want to try some of the fitters several
    # times to get a more robust answer
    fitters <- .fitters.without.starting.point;
  } else {
    # We have a starting point, so apply each of the default fitters only once.
    fitters <- .fitters.for.starting.point;
  }

  # Create an initial population of several candidate vectors which each are
  # slightly different from each other. This brings some diversity and makes
  # sure that each fitter starts at a slightly different point. Thus, we can
  # maybe avoid landing in a bad local optimum.
  candidates <- .make.initial.pop(par, metric@x, metric@y, length(fitters), model);
  best <- NULL;
  # Apply all the fitters and record their fitting qualities.
  qualities  <- vapply(X=fitters,
                       FUN=function(i, env) {
                         # Apply the selected fitter to the selected candidate point.
                         result <- .fitters[[i]](metric=metric, model=model, par=candidates[i,]);
                         # If it failed, return infinity.
                         if(is.null(result)) { return(+Inf); }
                         best <- get(x="best", pos=env);
                         if(is.null(best) || (result@quality < best@quality)) {
                           # Oh, the best solution has been beaten (or the first valid fitting has
                           # been found). Update the best record.
                           assign(x="best", value=result, pos=env);
                         }

                         # We will not allow nls a second chance because it is
                         # generally a bad method.
                         if(i < 4){ return(result@quality); }
                         return(+Inf);
                       }, FUN.VALUE=+Inf, env=environment());

  if(is.null(best)) {
    # OK, if we get here, all the standard fitters have failed. We now try other
    # methods to compensate, but these methods may be slow
    for(fitter in .fitters.fallback) {
      best <- fitter(metric=metric, model=model);
      if(!(is.null(best))) {
        # if we find a solution, we can immediately stop and try to refine it
        break;
      }
    }
  }

  if(is.null(best)) {
    # OK, so all fitters failed?
    if(!is.null(par)) {
      # But we have an initial point? Then there are two options: Either par is
      # invalid too, or it is a very solidary optimum surrounded by infeasible
      # solutions.
      if(FunctionalModel.par.check(model, par)) {
        # OK, par is within the specified bounds.
        quality <- metric@quality(model@f, par);
        if(learning.checkQuality(quality)) {
          # And also has a valid quality - return it.
          return(FittedFunctionalModel.new(model, par, quality));
        } # the quality of par is invalid
      } # par is outside the bounds
    } # par is null
    # If we get here, no dice, everything failed
    return(NULL);
  }

  # If we get here, at least some of our fitters have discovered a reasonable
  # solution. But maybe it was discovered by a bad fitter, say nls, due to
  # having a good initial point. Hence, we now make sure that all of the fitters
  # that did not yet see the result get a chance to refine it.
  if(q > 0.55) {
    fitters <- .fitters[unique(fitters[qualities > best@quality])];
    if(length(fitters) > 0L) {
      # iterate over all the standard fitters
      for(fitter in fitters) {
        # apply the standard fitter
        result <- fitter(metric=metric, model=model, par=best@par);
        if((!(is.null(result))) && (result@quality < best@quality)) {
          # record any improvement
          best <- result;
        }
      }
    }
  }

  # return the best result
  return(best);
}
