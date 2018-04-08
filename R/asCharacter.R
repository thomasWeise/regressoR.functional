#' @include FittedFunctionalModel.R

#' @title Convert a \code{FittedFunctionalModel} to a String
#' @description well, convert a \code{FittedFunctionalModel} to a String
#' @param x the \code{FittedFunctionalModel}
#' @return the string
#' @export FittedFunctionalModel.as.character
FittedFunctionalModel.as.character <- function(x) as.character(x@model)

#' @title Convert a \code{\link{FittedFunctionalModel}} to a String
#' @description the \code{as.character} implementation for
#'   \code{\link{FittedFunctionalModel}}
#' @param x the object
#' @return the name of the object
#' @importFrom methods setMethod
#' @name as.character
#' @aliases as.character,FittedFunctionalModel-method
methods::setMethod("as.character", "FittedFunctionalModel", FittedFunctionalModel.as.character)

#' @title Convert a \code{TransformedFittedFunctionalModel} to a String
#' @description well, convert a \code{TransformedFittedFunctionalModel} to a String
#' @param x the \code{TransformedFittedFunctionalModel}
#' @return the string
#' @export TransformedFittedFunctionalModel.as.character
TransformedFittedFunctionalModel.as.character <- function(x) {
  name <- FittedFunctionalModel.as.character(x);
  a <- (!(is.null(x@transform.x) || identical(x@transform.x, identity)));
  b <- (!(is.null(x@transform.y) || identical(x@transform.y, identity)));
  if(a && b) {
    return(paste(name, " with transformed x and y", sep="", collapse=""));
  }
  if(a) {
    return(paste(name, " with transformed x", sep="", collapse=""));
  }
  if(b) {
    return(paste(name, " with transformed y", sep="", collapse=""));
  }
  return(name);
}


#' @title Convert a \code{\link{TransformedFittedFunctionalModel}} to a String
#' @description the \code{as.character} implementation for
#'   \code{\link{TransformedFittedFunctionalModel}}
#' @param x the object
#' @return the name of the object
#' @importFrom methods setMethod
#' @name as.character
#' @aliases as.character,TransformedFittedFunctionalModel-method
methods::setMethod("as.character", "TransformedFittedFunctionalModel", TransformedFittedFunctionalModel.as.character)
