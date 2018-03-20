#' @include utils.R
#' @include FittedFunctionalModel.R

#' @title Use Differential Evolution to Optimize the Parameters
#'
#' @description Apply the Differential Evolution (DE) algorithm to fit a
#'   functional model.
#'
#' @param metric an instance of \code{regressoR.quality::RegressionQualityMetric}
#' @param model an instance of \code{\link{FunctionalModel}}
#' @param par the initial starting point
#' @return On success, an instance of
#'   \code{\link{FittedFunctionalModel}}. \code{NULL} on failure.
#' @importFrom DEoptim DEoptim DEoptim.control
#' @importFrom learnerSelectoR learning.checkQuality
#' @importClassesFrom regressoR.quality RegressionQualityMetric
#' @importFrom regressoR.functional.models par.estimate par.check
#' @export model.fit.de
model.fit.de <- function(metric, model, par=NULL) {
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

  NP <- 10L*model@paramCount;

  .ignore.errors({
    initialPop <- .make.initial.pop(par, lower, upper, NP, model@paramCount);

    if(base::is.null(lower)) {
      lower <- base::rep(-1e10 - base::max(base::abs(initialPop)), model@paramCount);
    }
    if(base::is.null(upper)) {
      upper <- base::rep(1e10 + base::max(base::abs(initialPop)), model@paramCount);
    }

    result <- DEoptim::DEoptim(fn=fn, lower=lower, upper=upper,
                      DEoptim::DEoptim.control(NP=NP, initialpop=initialPop, trace=FALSE));

    if(base::is.null(result) || (base::length(result) < 2)) { return(NULL); }
    result <- result[[1]];
    if(base::is.null(result) ) { return(NULL); }
    if(!(regressoR.functional.models::par.check(model, result$bestmem))) { return(NULL); }
    if(!(learnerSelectoR::learning.checkQuality(result$bestval))) { return(NULL); }
    return(FittedFunctionalModel.new(model, result$bestmem, result$bestval));
  });

  return(NULL);
}
