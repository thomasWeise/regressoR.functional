#' @title A Fitted Functional Model
#' @description This class holds a fully parameterized, fitted functional model.
#'
#' @slot model the model Blueprint
#' @slot par the model parameters
#' @exportClass FittedFunctionalModel
#' @importFrom methods setClass representation is validObject
#' @importClassesFrom regressoR.functional.models FunctionalModel
#' @importClassesFrom regressoR.base FittedModel
#' @importFrom regressoR.functional.models par.check
#' @seealso FittedFunctionalModel.new
FittedFunctionalModel <- methods::setClass(
  Class = "FittedFunctionalModel",
  contains = "FittedModel",
  representation = methods::representation(model="FunctionalModel",
                                           par="numeric"),
  validity = function(object) {
    if (base::is.null(object@model) ||
       (!(methods::is(object@model, "FunctionalModel")))) {
      return("Model model must be properly defined.");
    }
    methods::validObject(object@model);
    if(!(par.check(object@model, object@par))) {
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
  model <- base::force(model);

  f <- base::force(model@f);
  par <- base::force(base::unname(par));
  fn <- function(x) f(x, par);

  result <- methods::new("FittedFunctionalModel",
                         model=model, par=par, quality=quality, f=fn,
                         size=model@paramCount);
  result <- base::force(result);
  result@par <- base::force(result@par);
  result@f <- base::force(result@f);
  result@size <- base::force(result@size);
  result@quality <- base::force(result@quality);
  result <- base::force(result);

  methods::validObject(result);
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
  object <- base::force(object);
  object@f <- functionComposeR::function.substitute(object@model@f, base::list(par=object@par));
  object@f <- base::force(object@f);
  return(object);
}

#' @title Finalize a Fitted Functional Model
#' @description This method makes sure that the created instance of
#'   \code{\link{FittedFunctionalModel}} is fully finalized. This involves
#'   substituting the function parameter into the function.
#' @name learning.Result.finalize
#' @param object the fitted model to be finalized
#' @return a \code{\link{FittedFunctionalModel}}
#' @importFrom methods setMethod
#' @importMethodsFrom learnerSelectoR learning.Result.finalize
#' @aliases learning.Result.finalize,FittedFunctionalModel-method
methods::setMethod(
  f="learning.Result.finalize",
  signature="FittedFunctionalModel",
  definition=FittedFunctionalModel.finalize)