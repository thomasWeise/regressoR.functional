#' @include FittedFunctionalModel.R

#' @title A Functional Model Fitted on Transformed Data
#' @description This class holds a fully parameterized, functional model
#' which has been been fitted to transformed data.
#'
#' @slot transform.x the input transformation to be applied to all \code{x} coordinates before feeding them to the parameterized blueprint function
#' @slot transform.y the output transformation to be applied to all results of the blueprint function
#' @exportClass TransformedFittedFunctionalModel
#' @importFrom methods setClass representation
#' @seealso TransformedFittedFunctionalModel.new
TransformedFittedFunctionalModel <- methods::setClass(
  Class = "TransformedFittedFunctionalModel",
  contains = "FittedFunctionalModel",
  representation = methods::representation(transform.x="function",
                                           transform.y="function"),
  validity = function(object) {
    # check transform.x function
    if(base::is.null(object@transform.x) ||
       (!(base::is.function(object@transform.x)))) {
      return("transform.x function must be defined.");
    }
    if(base::is.primitive(object@transform.x)) {
      transform.x.args <- base::formals(base::args(object@transform.x));
    } else {
      transform.x.args <- base::formals(object@transform.x);
    }
    if((base::length(transform.x.args) != 1L) ||
       (!(base::identical(base::names(transform.x.args), base::c("x"))))) {
      return("transform.x function must have at exactly argument named 'x'.");
    }

    # check transform.y function
    if(base::is.null(object@transform.y) ||
       (!(base::is.function(object@transform.y)))) {
      return("transform.y function must be defined.");
    }
    if(base::is.primitive(object@transform.y)) {
      transform.y.args <- base::formals(base::args(object@transform.y));
    } else {
      transform.y.args <- base::formals(object@transform.y);
    }
    if((base::length(transform.y.args) != 1L) ||
       (!(base::identical(base::names(transform.y.args), base::c("x"))))) {
      return("transform.y function must have at exactly argument named 'x'.");
    }

    return(TRUE);
  }
)


#' @title Helper Method to Create a Transformed Fitted Functional Model
#'
#' @description Always use this function to instantiate
#'   \code{\link{FittedFunctionalModel}}
#'
#' @param model the model \code{\link{FunctionalModel}}
#' @param par the model parameters
#' @param quality the quality of the model on the original data, computed by a
#'   quality metric, smaller values are better
#' @param transform.x the input transformation to be applied to all \code{x} coordinates before feeding them to the parameterized blueprint function
#' @param transform.x.complexity the transformation of the \code{transform.x} transformation
#' @param transform.y the output transformation to be applied to all results of the blueprint function
#' @param transform.y.complexity the transformation of the \code{transform.y} transformation
#' @return an instance of either \code{\link{TransformedFittedFunctionalModel}} or
#'         \code{\link{FittedFunctionalModel}}
#' @importFrom methods new validObject
#' @export TransformedFittedFunctionalModel.new
#' @seealso TransformedFittedFunctionalModel.finalized
TransformedFittedFunctionalModel.new <- function(model, par, quality,
                                                 transform.x=base::identity,
                                                 transform.x.complexity=0L,
                                                 transform.y=base::identity,
                                                 transform.y.complexity=0L) {
  # setup parameters and default values
  if(base::is.null(transform.x) || base::missing(transform.x)) {
    transform.x <- base::identity;
    transform.x.complexity <- 0L;
  }
  if(base::is.null(transform.y) || base::missing(transform.y)) {
    transform.y <- base::identity;
    transform.y.complexity <- 0L;
  }

  # create function check whether can we just return a normal FittedFunctionalModel?
  model <- base::force(model);
  f <- base::force(model@f);
  par <- base::force(base::unname(par));

  if(base::identical(transform.x, base::identity)) {
    if(base::identical(transform.y, base::identity)) {
      # just return normal model
      return(FittedFunctionalModel.new(model=model, par=par, quality=quality));
    }
    # x transform is identity, we can skip it
    fn <- function(x) transform.y(f(x, par));
    transform.x.complexity <- 0L;
  } else {
    if(base::identical(transform.y, base::identity)) {
      # y transform is identity, we can skip it here
      fn <- function(x) f(transform.x(x), par);
      transform.y.complexity <- 0L;
    } else {
      # no identity transformation involved, create full function
      fn <- function(x) transform.y(f(transform.x(x), par));
    }
  }

  # check the complexities
  if(transform.x.complexity <= 0L) {
    if(transform.x.complexity < 0L) {
      stop("transform.x.complexity cannot be negative.");
    }
    if(!(base::identical(transform.x, base::identity))) {
      stop("transform.x.complexity cannot be 0L if transform.x is not base::identity.");
    }
  }
  if(transform.y.complexity <= 0L) {
    if(transform.y.complexity < 0L) {
      stop("transform.y.complexity cannot be negative.");
    }
    if(!(base::identical(transform.y, base::identity))) {
      stop("transform.y.complexity cannot be 0L if transform.y is not base::identity.");
    }
  }

  # create
  result <- methods::new("TransformedFittedFunctionalModel",
                         model=model,
                         par=par, quality=quality, f=fn,
                         transform.x = transform.x,
                         transform.y = transform.y,
                         size =(model@paramCount + transform.x.complexity + transform.y.complexity));

  result <- base::force(result);
  result@par <- base::force(result@par);
  result@f <- base::force(result@f);
  result@size <- base::force(result@size);
  result@quality <- base::force(result@quality);
  result@transform.x <- base::force(result@transform.x);
  result@transform.y <- base::force(result@transform.y);
  result <- base::force(result);

  methods::validObject(result);
  return(result);
}


#' @title Finalize a Fitted Transformed Functional Model
#' @description This method makes sure that the created instance of
#'   \code{\link{TransformedFittedFunctionalModel}} is fully finalized. This
#'   involves substituting the function parameter into the function.
#' @param object the fitted model to be finalized
#' @return a \code{\link{TransformedFittedFunctionalModel}} or
#'   \code{\link{FittedFunctionalModel}}
#' @importFrom functionComposeR function.substitute function.compose
TransformedFittedFunctionalModel.finalize <- function(object) {
  object <- base::force(object);

  x.i.i <- base::identical(object@transform.x, base::identity);
  y.i.i <- base::identical(object@transform.y, base::identity);

  if(x.i.i && y.i.i) {
    # transform.x and transform.y are the identity transformation
    return(FittedFunctionalModel.finalize(
      FittedFunctionalModel.new(model=object@model,
                                par=object@par,
                                quality=object@quality)));
  }

  f <- functionComposeR::function.substitute(object@model@f, base::list(par=object@par));

  if(!(x.i.i)) {
    # transform.x is not the identity transformation
    f <- functionComposeR::function.compose(f=object@transform.x, g=f, f2g="x");
  }

  if(!(y.i.i)) {
    # transform.y is not the identity transformation
    f <- functionComposeR::function.compose(f=f, g=object@transform.y, f2g="x");
  }

  object@f <- f;
  object@f <- base::force(object@f);
  return(object);
}

#' @title Finalize a Fitted Transformed Functional Model
#' @description This method makes sure that the created instance of
#'   \code{\link{TransformedFittedFunctionalModel}} is fully finalized. This
#'   involves substituting the function parameter into the function.
#' @name learning.Result.finalize
#' @param object the fitted model to be finalized
#' @return a \code{\link{TransformedFittedFunctionalModel}}
#' @importFrom methods setMethod
#' @importFrom functionComposeR function.substitute
#' @importMethodsFrom learnerSelectoR learning.Result.finalize
#' @aliases learning.Result.finalize,TransformedFittedFunctionalModel-method
methods::setMethod(
  f="learning.Result.finalize",
  signature = "TransformedFittedFunctionalModel",
  definition = TransformedFittedFunctionalModel.finalize)