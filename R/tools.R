# In this file we have some internal tools.

# Fix up some boundaries
.fix.boundaries <- function(functionalModel) {
  lower <- functionalModel@paramLower;
  upper <- functionalModel@paramUpper;
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
      upper <- functionalModel@paramUpper;
      lower <- functionalModel@paramLower;
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
  return(list(lower=lower, upper=upper));
}

# Make an initial population of size \code{NP}
#' @importFrom stats rnorm runif
#' @importFrom regressoR.functional.models FunctionalModel.par.fix
.make.initial.pop <- function(par, x, y, lower, upper, NP, model) {

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
  if(!(is.null(lower) && is.null(upper))) {
    # Yes we do
    for(i in 1:NP) {
      pop[i,] <- FunctionalModel.par.fix(pop[i, ], lower, upper, model@paramCount);
    }
  }

  return(pop);
}
