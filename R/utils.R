# Either a \code{function} or \code{NULL}
#' @importFrom methods setClassUnion
setClassUnion(".dataModeleR.functionOrNULL", c("function","NULL"))
# Either a \code{numeric} \code{vector} or \code{NULL}
#' @importFrom methods setClassUnion
setClassUnion(".dataModeleR.vectorOrNULL", c("numeric","NULL"))

# ignore the argument
# @param e ignored
.ignore<-function(e) { }

# Execute the expression and ignore all errors
# @param exp the expression
.ignore.errors <- function(exp) {
  tryCatch(exp, error=.ignore, warning=.ignore)
}
