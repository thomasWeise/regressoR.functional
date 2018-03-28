#' @include utils.R
#' @include FittedFunctionalModel.R

#' @title Use Differential Evolution to Optimize the Parameters
#'
#' @description Apply the Differential Evolution (DE) algorithm to fit a
#'   functional model.
#'
#' @param metric an instance of
#'   \code{regressoR.quality::RegressionQualityMetric}
#' @param model an instance of \code{\link{FunctionalModel}}
#' @param par the initial starting point
#' @return On success, an instance of \code{\link{FittedFunctionalModel}}.
#'   \code{NULL} on failure.
#' @importFrom DEoptim DEoptim DEoptim.control
#' @importFrom learnerSelectoR learning.checkQuality
#' @importClassesFrom regressoR.quality RegressionQualityMetric
#' @importFrom regressoR.functional.models FunctionalModel.par.estimate
#'   FunctionalModel.par.check
#' @export FunctionalModel.fit.de
FunctionalModel.fit.de <- function(metric, model, par=NULL) {
  if(is.null(metric) || is.null(model) ) { return(NULL); }

  if(is.null(par)) {
    par <- regressoR.functional.models::FunctionalModel.par.estimate(model, metric@x, metric@y);
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

  NP <- 10L*model@paramCount;

  .ignore.errors({
    initialPop <- .make.initial.pop(par, lower, upper, NP, model@paramCount);

    if(is.null(lower)) {
      lower <- rep(-1e10 - max(abs(initialPop)), model@paramCount);
    }
    if(is.null(upper)) {
      upper <- rep(1e10 + max(abs(initialPop)), model@paramCount);
    }

    result <- DEoptim::DEoptim(fn=fn, lower=lower, upper=upper,
                      DEoptim::DEoptim.control(NP=NP, initialpop=initialPop, trace=FALSE));

    if(is.null(result) || (length(result) < 2)) { return(NULL); }
    result <- result[[1]];
    if(is.null(result) ) { return(NULL); }
    if(!(regressoR.functional.models::FunctionalModel.par.check(model, result$bestmem))) { return(NULL); }
    if(!(learnerSelectoR::learning.checkQuality(result$bestval))) { return(NULL); }
    return(FittedFunctionalModel.new(model, result$bestmem, result$bestval));
  });

  return(NULL);
}
