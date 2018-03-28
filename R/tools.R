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
.make.initial.pop <- function(par, lower, upper, NP, paramCount) {

  # Let's first randomly sample the current population based on the par vector
  pop <- matrix(sapply(X=par, FUN = function(x) {
    if(x != 0) {
      return(rnorm(n=NP, mean=x, sd=0.1*abs(x)))
    } else {
      return(exp(rnorm(n=NP, sd=2)) * rnorm(n=NP)); }
  }),
  ncol=paramCount, nrow=NP);

  # Then let's make sure that the initial guess will appear in the population
  pop[1,] <- par;

  # Do we need to check the boundaries?
  if(!(is.null(lower) && is.null(upper))) {
    # Yes we do
    for(i in 1:NP) {
      pop[i,] <- FunctionalModel.par.fix(pop[i, ], lower, upper, paramCount);
    }
  }

  return(pop);
}
