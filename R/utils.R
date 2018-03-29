# Some internal, general utils.

# Execute the expression and ignore all errors
# @param exp the expression
.ignore.errors <- function(exp) {
  tryCatch(suppressWarnings(exp), error=function(e) { })
}
