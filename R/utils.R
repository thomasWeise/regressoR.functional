# Some internal, general utils.

# ignore the argument
# @param e ignored
.ignore<-function(e) { }

# Execute the expression and ignore all errors
# @param exp the expression
.ignore.errors <- function(exp) {
  tryCatch(exp, error=.ignore, warning=.ignore)
}
