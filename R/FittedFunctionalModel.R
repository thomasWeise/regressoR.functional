#' @title A Fitted Functional Model
#'
#' @description This class holds a fully parameterized, fitted functional model.
#' Instances of this class are typically the result of a model fitting approach,
#' i.e., the application of an algorithm which accepts an instance of
#' \code{\link{FunctionalModel}} and an instance of
#' \code{\link{RegressionQualityMetric}} and finds the parameters for the
#' functional model which minimize the return value of the regression quality
#' metric.
#'
#' @slot model the model Blueprint
#' @slot par the model parameters
#' @exportClass FittedFunctionalModel
#' @importFrom methods setClass representation is validObject
#' @importClassesFrom regressoR.functional.models FunctionalModel
#' @importClassesFrom regressoR.base FittedModel
#' @importFrom regressoR.functional.models FunctionalModel.par.check
#' @seealso FittedFunctionalModel.new
FittedFunctionalModel <- setClass(
  Class = "FittedFunctionalModel",
  contains = "FittedModel",
  representation = representation(model="FunctionalModel",
                                           par="numeric"),
  validity = function(object) {
    if (is.null(object@model) ||
       (!(is(object@model, "FunctionalModel")))) {
      return("Model model must be properly defined.");
    }
    validObject(object@model);
    if(!(FunctionalModel.par.check(object@model, object@par))) {
      return("Model parameters cannot be null and must match the number of parameters and constraints specified in model.");
    }
    if(!(object@size >= object@model@paramCount)) {
      return("Object size must be greater than or equal to number of parameters of model.");
    }
    return(TRUE);
  }
)


#' @title Helper Method to Create a Fitted Functional Model
#'
#' @description Always use this function to instantiate
#'   \code{\link{FittedFunctionalModel}}
#'
#' @param model the model \code{\link{FunctionalModel}}
#' @param par the model parameters
#' @param quality the quality of the model on the original data, computed by a
#'   quality metric, smaller values are better
#' @return a \code{\link{FittedFunctionalModel}}
#' @importFrom methods new validObject
#' @export FittedFunctionalModel.new
#' @seealso FittedFunctionalModel.finalized
FittedFunctionalModel.new <- function(model, par, quality) {
  model <- force(model);

  f <- force(model@f);
  par <- force(unname(par));
  fn <- function(x) f(x, par);

  result <- new("FittedFunctionalModel",
                         model=model, par=par, quality=quality, f=fn,
                         size=model@paramCount);
  result <- force(result);
  result@par <- force(result@par);
  result@f <- force(result@f);
  result@size <- force(result@size);
  result@quality <- force(result@quality);
  result <- force(result);

  validObject(result);
  return(result);
}

#' @title Finalize a Fitted Functional Model
#' @description This method makes sure that the created instance of
#'   \code{\link{FittedFunctionalModel}} is fully finalized. This involves
#'   substituting the function parameter into the function.
#' @param object the fitted model to be finalized
#' @return a \code{\link{FittedFunctionalModel}}
#' @importFrom functionComposeR function.substitute
#' @export FittedFunctionalModel.finalize
FittedFunctionalModel.finalize <- function(object) {
  object <- force(object);
  object@f <- function.substitute(object@model@f, list(par=object@par));
  object@f <- force(object@f);
  return(object);
}

#' @importFrom methods setMethod
#' @importMethodsFrom learnerSelectoR learning.Result.finalize
#' @aliases learning.Result.finalize,FittedFunctionalModel-method
#' @rdname learning.Result.finalize
setMethod(
  f="learning.Result.finalize",
  signature="FittedFunctionalModel",
  definition=FittedFunctionalModel.finalize)
