#' @include utils.R
#' @include FittedFunctionalModel.R

# compute the population size for DE based on the effort parameter q and the
# parameter count n
.de.ps <- function(q, n) {
  q          <- max(0L, min(1L, q));
  as.integer(round(
       (5L + q*((6L*q) - 1L)) + # offset
       ((1L + 19L*((q*q)*q*(q*q)))   * # multiplier
        (n^(q*((8L*q) - 2L) / 3L))))) # exponent
}

#' @title Use Differential Evolution to Optimize the Parameters
#'
#' @description Apply the Differential Evolution (DE) algorithm to fit a
#'   functional model.
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
#' @importFrom DEoptim DEoptim DEoptim.control
#' @importFrom learnerSelectoR learning.checkQuality
#' @importClassesFrom regressoR.quality RegressionQualityMetric
#' @importFrom regressoR.functional.models FunctionalModel.par.estimate
#'   FunctionalModel.par.check
#' @export FunctionalModel.fit.de
FunctionalModel.fit.de <- function(metric, model, par=NULL, q=0.75) {
  if(is.null(metric) || is.null(model) ) { return(NULL); }

  fn <- function(par) metric@quality(model@f, par);

  limits <- .fix.boundaries(model);
  if(is.null(limits)) {
    lower <- NULL;
    upper <- NULL;
  } else {
    lower <- limits$lower;
    upper <- limits$upper;
  }

  NP <- .de.ps(q=q, n=model@paramCount);

  .ignore.errors({
    initialPop <- .make.initial.pop(par, metric@x, metric@y, NP, model);

    if(is.null(lower)) {
      lower <- rep(-1e10 - max(abs(initialPop)), model@paramCount);
    }
    if(is.null(upper)) {
      upper <- rep(1e10 + max(abs(initialPop)), model@paramCount);
    }

    result <- DEoptim(fn=fn, lower=lower, upper=upper,
                      DEoptim.control(NP=NP, initialpop=initialPop, trace=FALSE));

    if(is.null(result) || (length(result) < 2)) { return(NULL); }
    result <- result[[1]];
    if(is.null(result) ) { return(NULL); }
    if(!(FunctionalModel.par.check(model, result$bestmem))) { return(NULL); }
    if(!(learning.checkQuality(result$bestval))) { return(NULL); }
    return(FittedFunctionalModel.new(model, result$bestmem, result$bestval));
  });

  return(NULL);
}
