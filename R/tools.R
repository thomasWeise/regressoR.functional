# In this file we have some internal tools.

# Fix up some boundaries
.fix.boundaries <- function(functionalModel, par=NULL, need=FALSE) {
  lower <- functionalModel@paramLower;
  upper <- functionalModel@paramUpper;

  if(need) {
    # if we need parameters, generate them on the spot if they do not exist
    if(is.null(lower)) { lower <- rep(-1e10, functionalModel@paramCount); }
    if(is.null(upper)) { upper <- rep( 1e10, functionalModel@paramCount); }
  }

  # check if parameters exist and fix the included NAs, if any
  if(is.null(lower)) {
    if(is.null(upper)) {
      return(NULL);
    } else {
      upper[is.na(upper)] <- 1e10;
    }
  } else {
    if(is.null(upper)) {
      lower[is.na(lower)] <- -1e10;
    } else {
      for(i in 1:functionalModel@paramCount) {
        if(is.na(upper[i])) {
          if(is.na(lower[i])) {
            upper[i] <- 1e10;
            lower[i] <- -1e10;
          } else {
            upper[i] <- (lower[i] + 1e10);
          }
        } else {
          if(is.na(lower[i])) {
            lower[i] <- (upper[i] - 1e10);
          }
        }
      }
    }
  }

  # if a parameter vector is included, modify limits to include it
  if(!(is.null(par))) {
    for(i in 1:functionalModel@paramCount) {
      v <- par[i];
      if(is.finite(v)) {
        if((!(is.null(lower))) && (v < lower[i])) {
          if(v < 0) { lower[i] <- 2*v; }
          else { lower[i] <- 0.5*v; }
        }
        if((!(is.null(upper))) && (v > upper[i])) {
          if(v > 0) { upper[i] <- 2*v; }
          else { upper[i] <- 0.5*v; }
        }
      }
    }
  }

  return(list(lower=lower, upper=upper));
}

# Make an initial population of size \code{NP}
#' @importFrom stats rnorm runif
#' @importFrom regressoR.functional.models FunctionalModel.par.fix
.make.initial.pop <- function(par, x, y, NP, model) {

  # if no starting vector is provided, we use the parameter guessers
  if(is.null(par)) {
    # we try to guess NP vectors, but the guesser might return the same vector
    # multiple times, so we keep only unique rows
    pop <- unique(t(rbind(sapply(X=1L:NP,
                          FUN=function(i) FunctionalModel.par.estimate(model, x, y)))));
  } else {
    # if we have a starting vector, we use that one
    pop <- matrix(par, nrow=1L, ncol=model@paramCount);
  }

  # if the matrix is not big enough, we expand it with more rows
  while((rows <- dim(pop)[1L]) < NP) {
    # no, it is not, we need more rows
    pop <- unique(rbind(pop,
             t(rbind(sapply(X=1L:(NP-rows), FUN = function(i) {
      # each new row is derived from an existing vector in the population
      z <- pop[(i%%rows) + 1L,];
      return(vapply(X=z, FUN=function(x) {
        if(x != 0) {
          return(rnorm(n=1L, mean=x, sd=0.1*abs(x)))
        } else {
          return(exp(rnorm(n=1L, sd=2)) * rnorm(n=1L)); }
      }, FUN.VALUE = 0));
    })))))
  }

  # Do we need to check the boundaries?
  if(!(is.null(model@paramLower) && is.null(model@paramUpper))) {
    # Yes we do
    for(i in 1:NP) {
      pop[i,] <- FunctionalModel.par.fix(pop[i, ], model@paramLower, model@paramUpper, model@paramCount);
    }
  }

  return(pop);
}
