#' @include fitTransformed.R


#' @title Create Learners for the Given Set of Models
#' @description Create the learners for the use in
#'   \code{\link{regressoR.applyLearners}} from a set of models.
#' @param models the set of models to use, by default this is
#'   \code{\link{FunctionalModel.all}}
#' @param fitter the model fitter to use
#' @return a list of fitters that can be applied.
#' @export FunctionalModel.makeLearners
#' @importFrom regressoR.functional.models FunctionalModel.all
FunctionalModel.makeLearners <- function(models = FunctionalModel.all(),
                                         fitter = FunctionalModel.fit) {
  if(is.null(models) || (!(is.list(models))) || (length(models) <= 0L)) {
    stop("The list of models cannot be empty or NULL.");
  }
  if(is.null(fitter) || (!(is.function(fitter)))) {
    stop("Fitter chooser must be a proper function.");
  }

  result <- lapply(X=models, FUN = function(model) {
    model <- force(model);
    func <- function(metric, transformation.x, transformation.y, metric.transformed, q)
                 FunctionalModel.fit.transformed(
                    metric=metric, model=model, transformation.x=transformation.x,
                    transformation.y=transformation.y, metric.transformed=metric.transformed,
                    par=NULL, q=q, fitter=fitter);
    func <- force(func);
    return(func);
    });
  result <- force(result);
  return(result);
}
