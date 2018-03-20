#' @include cmaes.R
#' @include minqa.R
#' @include nlslm.R
#' @include de.R
#' @include dfoptim.R
#' @include nls.R


# The internal least squares approach: nls.lm with fallback to nls
.fitters.nls <- function(metric, model, par=NULL) {
  result <- FunctionalModel.fit.nlslm(metric, model, par);
  if(base::is.null(result)) {
    return(FunctionalModel.fit.nls(metric, model, par));
  }
  return(result);
}

# The internal population-based approach: cma-es with fallback to de
.fitters.cmaesde <- function(metric, model, par=NULL) {
  result <- FunctionalModel.fit.cmaes(metric, model, par);
  if(base::is.null(result)) {
    return(FunctionalModel.fit.de(metric, model, par));
  }
  return(result);
}


# The internal local search-like approach: minqa with fallback to df
.fitters.minqadfoptim <- function(metric, model, par=NULL) {
  result <- FunctionalModel.fit.minqa(metric, model, par);
  if(base::is.null(result)) {
    return(FunctionalModel.fit.dfoptim(metric, model, par));
  }
  return(result);
}

.key.2.nls          <- 1L;
.key.2.minqadfoptim <- 2L;
.key.2.cmaesde      <- 4L;
.key.nlslm          <- 8L;
.key.minqa          <- 16L;
.key.cmaes          <- 32L;
.key.nls            <- 64L;
.key.dfoptim        <- 128L;
.key.de             <- 256L;


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
  if(base::bitwAnd(available, (.key.cmaes + .key.de)) == (.key.cmaes + .key.de)) {
    available <- available + .key.2.cmaesde;
  }
  if(base::bitwAnd(available, (.key.nlslm + .key.nls)) == (.key.nlslm + .key.nls)) {
    available <- available + .key.2.nls;
  }
  if(base::bitwAnd(available, (.key.minqa + .key.dfoptim)) == (.key.minqa + .key.dfoptim)) {
    available <- available + .key.2.minqadfoptim;
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
  if(complexity <= 300L) {
    # we use all the methods
    usage <- base::bitwAnd(usage, base::bitwNot(.key.2.cmaesde +
                                                .key.2.minqadfoptim +
                                                .key.2.nls));
  } else {
    # we use either nls.lm or nls
    if(base::bitwAnd(usage, .key.2.nls) == .key.2.nls) {
      usage <- base::bitwAnd(usage, base::bitwNot(.key.nls + .key.nlslm));
    }

    if(complexity <= 600L) {
      # if the complexity is less than 600, we can use both cmaes and de
      usage <- base::bitwAnd(usage, base::bitwNot(.key.2.cmaesde));
    } else {
      # otherwise, we don't use de
      usage <- base::bitwAnd(usage, base::bitwNot(.key.de));

      if(complexity <= 16000L) {
        # otherwise, we use cmaes only (with fallback to cmaes-de, if necessary)
        if(base::bitwAnd(usage, .key.2.cmaesde) == .key.2.cmaesde) {
          usage <- base::bitwAnd(usage, base::bitwNot(.key.cmaes));
        }
      } else {
        # if the complexity is too big, we won't use cma-es either
        usage <- base::bitwAnd(usage, base::bitwNot(.key.2.cmaesde + .key.cmaes));
      }
    }

    if(complexity <= 8000L) {
      # if the complexity is less than 8000, we can use both minqa and dfoptim
      usage <- base::bitwAnd(usage, base::bitwNot(.key.2.minqadfoptim));
    } else {
      if(complexity <= 24000L) {
        # otherwise, if the complexity is below 24000, we only use minqa
        if(base::bitwAnd(usage, .key.dfoptim) == .key.dfoptim) {
          usage <- base::bitwAnd(usage, base::bitwNot(.key.dfoptim));
        }
        if(base::bitwAnd(usage, .key.2.minqadfoptim) == .key.2.minqadfoptim) {
          usage <- base::bitwAnd(usage, base::bitwNot(.key.minqa));
        }
      } else {
        # if the complexity is too big, we won't use minqa, just dfoptim
        usage <- base::bitwAnd(usage, base::bitwNot(.key.2.minqadfoptim + .key.minqa));
      }
    }
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

    if(base::bitwAnd(usage, .key.2.nls) == .key.2.nls) {
      result[[base::length(result) + 1L]] <- .fitters.nls;
    }
    if(base::bitwAnd(usage, .key.2.minqadfoptim) == .key.2.minqadfoptim) {
      result[[base::length(result) + 1L]] <- .fitters.minqadfoptim;
    }
    if(base::bitwAnd(usage, .key.2.cmaesde) == .key.2.cmaesde) {
      result[[base::length(result) + 1L]] <- .fitters.cmaesde;
    }
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

