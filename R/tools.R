# In this file we have some internal tools.

# Fix up some boundaries
.fix.boundaries <- function(functionalModel) {
  lower <- functionalModel@paramLower;
  upper <- functionalModel@paramUpper;
  if(base::is.null(lower)) {
    if(base::is.null(upper)) {
      return(NULL);
    } else {
      upper[base::is.na(upper)] <- 1e10;
    }
  } else {
    if(base::is.null(upper)) {
      lower[base::is.na(lower)] <- -1e10;
    } else {
      upper <- functionalModel@paramUpper;
      lower <- functionalModel@paramLower;
      for(i in 1:functionalModel@paramCount) {
        if(base::is.na(upper[i])) {
          if(base::is.na(lower[i])) {
            upper[i] <- 1e10;
            lower[i] <- -1e10;
          } else {
            upper[i] <- (lower[i] + 1e10);
          }
        } else {
          if(base::is.na(lower[i])) {
            lower[i] <- (upper[i] - 1e10);
          }
        }
      }
    }
  }
  return(base::list(lower=lower, upper=upper));
}

# Make an initial population of size \code{NP}
#' @importFrom stats rnorm runif
#' @importFrom regressoR.functional.models FunctionalModel.par.fix
.make.initial.pop <- function(par, lower, upper, NP, paramCount) {

  # Let's first randomly sample the current population based on the par vector
  pop <- base::matrix(base::sapply(X=par, FUN = function(x) {
    if(x != 0) {
      return(stats::rnorm(n=NP, mean=x, sd=0.1*base::abs(x)))
    } else {
      return(base::exp(stats::rnorm(n=NP, sd=2)) * stats::rnorm(n=NP)); }
  }),
  ncol=paramCount, nrow=NP);

  # Then let's make sure that the initial guess will appear in the population
  pop[1,] <- par;

  # Do we need to check the boundaries?
  if(!(base::is.null(lower) && base::is.null(upper))) {
    # Yes we do
    for(i in 1:NP) {
      pop[i,] <- FunctionalModel.par.fix(pop[i, ], lower, upper, paramCount);
    }
  }

  return(pop);
}
