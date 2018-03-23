#' @include defaultFitters.R
#' @include fitTransformed.R

.defFit <- FunctionalModel.fit.defaultFitters;

#' @title Create Learners for the Given Set of Models
#' @description Create the learners for the use in
#'   \code{\link{regressoR.applyLearners}} from a set of models.
#' @param models the set of models to use, by default this is
#'   \code{\link{FunctionalModel.all}}
#' @param fitters the algorithm selecting which fitters to apply for each
#'   model and data set, by default this is
#'   \code{\link{FunctionalModel.fit.defaultFitters}}.
#' @return a list of fitters that can be applied.
#' @export FunctionalModel.makeLearners
#' @importFrom regressoR.functional.models FunctionalModel.all
FunctionalModel.makeLearners <- function(models = FunctionalModel.all(), fitters=.defFit) {
  if(is.null(models) || (!(is.list(models))) || (length(models) <= 0L)) {
    stop("The list of models cannot be empty or NULL.");
  }
  if(is.null(fitters) || (!(is.function(fitters)))) {
    stop("Fitter chooser must be a proper function.");
  }

  result <- lapply(X=models, FUN = function(model) {
    model <- force(model);
    func <- function(metric, transformation.x, transformation.y, metric.transformed)
                 FunctionalModel.fit.transformed(
                    metric=metric, model=model, transformation.x=transformation.x,
                    transformation.y=transformation.y, metric.transformed=metric.transformed,
                    par=NULL,
                    fitters = fitters(length(metric@x), model@paramCount));
    func <- force(func);
    return(func);
    });
  result <- force(result);
  return(result);
}
