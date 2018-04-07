#' @include FittedFunctionalModel.R
#' @include utils.R
#' @include tools.R

#' @title Apply a the Standard Non-Linear Least Squares Approach from Package
#'   \code{stats}
#'
#' @description Apply the default non-linear least squares algorithm to fit a
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
#' @seealso \code{\link{nls}}
#' @importFrom stats nls
#' @importFrom learnerSelectoR learning.checkQuality
#' @importClassesFrom regressoR.quality RegressionQualityMetric
#' @importFrom regressoR.functional.models FunctionalModel.par.estimate
#'   FunctionalModel.par.check
#' @export FunctionalModel.fit.nls
FunctionalModel.fit.nls <- function(metric, model, par=NULL, q=0.75) {
  if(is.null(metric) || is.null(model) ||
     is.null(metric@x) || is.null(metric@y)) { return(NULL); }

  if(is.null(par)) {
    par <- FunctionalModel.par.estimate(model, metric@x, metric@y);
  }

  .ignore.errors({
    if(is.null(metric@weights)) {
      result <- nls(y ~ model@f(x, par), data=list(x=metric@x, y=metric@y), start=list(par=par));
    } else {
      result <- nls(y ~ model@f(x, par), data=list(x=metric@x, y=metric@y),
                    start=list(par=par), weights=metric@weights);
    }

    if(is.null(result) || is.null(result$m) || is.null(result$m$getPars)) { return(NULL); }
    if(!(result$convInfo$isConv)) { return(NULL); }
    result <- result$m$getPars();
    if(!(FunctionalModel.par.check(model, result))) { return(NULL); }
    quality <- metric@quality(model@f, result);
    if(!(learning.checkQuality(quality))) { return(NULL); }
    return(FittedFunctionalModel.new(model, result, quality));
  });

  return(NULL);
}
