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
  if(require("cmaes")) {
    available <- available + .key.cmaes;
  }
  if(require("minqa")) {
    available <- available + .key.minqa;
  }
  if(require("minpack.lm")) {
    available <- available + .key.nlslm;
  }
  if(require("stats")) {
    available <- available + .key.nls;
  }
  if(require("DEoptim")) {
    available <- available + .key.de;
  }
  if(require("dfoptim")) {
    available <- available + .key.dfoptim;
  }
  return(available);
}

# cache the availability of the fitters
.fitters.available <- .fitters.make.available()

# create the cache
.fitters.cache <- new.env();

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
     ((complexity > 30000L) || (bitwAnd(usage, .key.nlslm) == .key.nlslm))) {
    usage <- bitwAnd(usage, bitwNot(.key.nls));
  }

  # cmaes gives by far the best results, but is also very slow, so we can only use it for small problems
  if(complexity > 600L) {
    usage <- bitwAnd(usage, bitwNot(.key.cmaes));
  }

  # de is about as good as nlsm, but maybe 10 times as slow
  if( (complexity > 4500L) &&
     ((complexity > 8000L) || (bitwAnd(usage, .key.nlslm) == .key.nlslm))) {
    usage <- bitwAnd(usage, bitwNot(.key.cmaes));
  }

  # dfoptim is usually worse than minqa, so we don't use it in larger problems if we have minqa
  if((complexity > 3000L) && (bitwAnd(usage, .key.minqa) == .key.minqa)) {
    usage <- bitwAnd(usage, bitwNot(.key.dfoptim));
  }

  # ok, our fitter policy has failed, take the first best fitter
  if(usage == 0L) {
    i <- 1L;
    while(i <= .fitters.available) {
      if(bitwAnd(i, .fitters.available) == i) {
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
  usage.str <- toString(usage);
  result <- get0(x=usage.str, envir=.fitters.cache, ifnotfound = NULL, inherits=FALSE);
  if(is.null(result)) {
    result <- list();

    if(bitwAnd(usage, .key.nlslm) == .key.nlslm) {
      result[[length(result) + 1L]] <- FunctionalModel.fit.nlslm;
    }
    if(bitwAnd(usage, .key.minqa) == .key.minqa) {
      result[[length(result) + 1L]] <- FunctionalModel.fit.minqa;
    }
    if(bitwAnd(usage, .key.cmaes) == .key.cmaes) {
      result[[length(result) + 1L]] <- FunctionalModel.fit.cmaes;
    }
    if(bitwAnd(usage, .key.nls) == .key.nls) {
      result[[length(result) + 1L]] <- FunctionalModel.fit.nls;
    }
    if(bitwAnd(usage, .key.dfoptim) == .key.dfoptim) {
      result[[length(result) + 1L]] <- FunctionalModel.fit.dfoptim;
    }
    if(bitwAnd(usage, .key.de) == .key.de) {
      result[[length(result) + 1L]] <- FunctionalModel.fit.de;
    }
    result <- unlist(result);
    assign(x=usage.str, value=result, pos=.fitters.cache);
    lockBinding(sym=usage.str, env=.fitters.cache);
  }

  return(result);
}

