#' @include cmaes.R
#' @include minqa.R
#' @include nlslm.R
#' @include de.R
#' @include dfoptim.R
#' @include nls.R


.key.nlslm          <- 1L;
.key.minqa          <- 2L;
.key.de             <- 4L;
.key.cmaes          <- 8L;
.key.dfoptim        <- 16L;
.key.nls            <- 32L;


# find out which fitters are available
.fitters.make.available <- function() {
  available <- 0L;
  if(base::require("cmaes")) {
    available <- available + .key.cmaes;
  }
  if(base::require("minqa")) {
    available <- available + .key.minqa;
  }
  if(base::require("minpack.lm")) {
    available <- available + .key.nlslm;
  }
  if(base::require("stats")) {
    available <- available + .key.nls;
  }
  if(base::require("DEoptim")) {
    available <- available + .key.de;
  }
  if(base::require("dfoptim")) {
    available <- available + .key.dfoptim;
  }
  return(available);
}

# cache the availability of the fitters
.fitters.available <- .fitters.make.available()

# create the cache
.fitters.cache <- base::new.env();

#' @title Get the List of Default Functional Model Fitting Algorithms
#' @description Well, get the List of Default Functional Model Fitters.
#' @param dataSize the size of the data, assumed to be {@code 1000} if unknown
#' @param paramCount the number of model parameters, assumed to be {@code 4} if
#'   unknown
#' @return the List of Default Functional Model Fitters
#' @export FunctionalModel.fit.defaultFitters
FunctionalModel.fit.defaultFitters <- function(dataSize = 1000, paramCount = 4) {
  usage <- .fitters.available;

  complexity <- (dataSize * paramCount * paramCount);

  # nls is fast but has comparatively bad results, so for bigger problems we won't use it,
  # especially if we have nlslm or the complexity is very big
  if( (complexity > 10000L) &&
     ((complexity > 30000L) || (base::bitwAnd(usage, .key.nlslm) == .key.nlslm))) {
    usage <- base::bitwAnd(usage, base::bitwNot(.key.nls));
  }

  # cmaes gives by far the best results, but is also very slow, so we can only use it for small problems
  if(complexity > 600L) {
    usage <- base::bitwAnd(usage, base::bitwNot(.key.cmaes));
  }

  # de is about as good as nlsm, but maybe 10 times as slow
  if( (complexity > 4500L) &&
     ((complexity > 8000L) || (base::bitwAnd(usage, .key.nlslm) == .key.nlslm))) {
    usage <- base::bitwAnd(usage, base::bitwNot(.key.cmaes));
  }

  # dfoptim is usually worse than minqa, so we don't use it in larger problems if we have minqa
  if((complexity > 3000L) && (base::bitwAnd(usage, .key.minqa) == .key.minqa)) {
    usage <- base::bitwAnd(usage, base::bitwNot(.key.dfoptim));
  }

  # ok, our fitter policy has failed, take the first best fitter
  if(usage == 0L) {
    i <- 1L;
    while(i <= .fitters.available) {
      if(base::bitwAnd(i, .fitters.available) == i) {
        usage <- i;
        break;
      }
      i <- i * 2L;
    }
  }

  if(usage == 0L) {
    stop("No function fitter found!");
  }

  # now that we know the fitters, we check the cache
  usage.str <- base::toString(usage);
  result <- base::get0(x=usage.str, envir=.fitters.cache, ifnotfound = NULL, inherits=FALSE);
  if(base::is.null(result)) {
    result <- base::list();

    if(base::bitwAnd(usage, .key.nlslm) == .key.nlslm) {
      result[[base::length(result) + 1L]] <- FunctionalModel.fit.nlslm;
    }
    if(base::bitwAnd(usage, .key.minqa) == .key.minqa) {
      result[[base::length(result) + 1L]] <- FunctionalModel.fit.minqa;
    }
    if(base::bitwAnd(usage, .key.cmaes) == .key.cmaes) {
      result[[base::length(result) + 1L]] <- FunctionalModel.fit.cmaes;
    }
    if(base::bitwAnd(usage, .key.nls) == .key.nls) {
      result[[base::length(result) + 1L]] <- FunctionalModel.fit.nls;
    }
    if(base::bitwAnd(usage, .key.dfoptim) == .key.dfoptim) {
      result[[base::length(result) + 1L]] <- FunctionalModel.fit.dfoptim;
    }
    if(base::bitwAnd(usage, .key.de) == .key.de) {
      result[[base::length(result) + 1L]] <- FunctionalModel.fit.de;
    }
    result <- base::unlist(result);
    base::assign(x=usage.str, value=result, pos=.fitters.cache);
    base::lockBinding(sym=usage.str, env=.fitters.cache);
  }

  return(result);
}

