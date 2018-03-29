#' @include FittedFunctionalModel.R
#' @include defaultFitters.R
#' @include tools.R
#' @include utils.R

#' @title Fit the Given Model Blueprint to the Specified Data
#'
#' @description Apply a set of fitters iteratively to fit the specified model to
#' the given data. First, we generate a starting guess about the
#' parameterization via \code{\link{FunctionalModel.par.estimate}} (or accept it
#' via the parameter \code{par}). From then on, we apply the different
#' function fitters one by one. All the fitters who have not produced the
#' current best solution are applied again, to the now-best guess. However, we
#' do not apply the fitters that have produced that very guess in the next
#' round. (They may get a chance again in a later turn.) Anyway, this
#' procedure is iterated until no improvement can be made anymore. After
#' finishing the fitting, we attempt whether rounding the fitted parameters to
#' integers can improve the fitting quality.
#'
#' @param metric an instance of \code{RegressionQualityMetric}
#' @param model an instance of \code{\link{FunctionalModel}}
#' @param par the initial starting point
#' @param fitters the fitters
#' @return On success, an instance of
#'   \code{\link{FittedFunctionalModel}}. \code{NULL} on failure.
#' @export FunctionalModel.fit
#' @importFrom learnerSelectoR learning.checkQuality
#' @importFrom regressoR.functional.models FunctionalModel.par.check FunctionalModel.par.estimate
#' @examples
#'
#' set.seed(12555)
#' x <- (1:201 - 100) * 0.1;
#' fxpar <- function(x, par) par[1] + x * (par[2] + x * (par[3]));
#' par <- c(4, -3, 2);
#' fx <- function(x) fxpar(x, par);
#' y <- fx(x);
#' model <- regressoR.functional.models::FunctionalModel.quadratic();
#' metric1 <- regressoR.quality::RegressionQualityMetric.default(x, y);
#' res1 <- FunctionalModel.fit(metric1, model);
#' res1@quality
#' # [1] 0
#' res1@par
#' # [1]  4 -3  2
#'
#' xr <- x + 0.1*rnorm(length(x));
#' yr <- y + 0.1*rnorm(length(y));
#' metric2 <- regressoR.quality::RegressionQualityMetric.default(xr, yr);
#' res2 <- FunctionalModel.fit(metric2, model);
#' res2@quality
#' # [1] 0.2439082
#' res2@par
#' # [1]  3.919365 -2.938202  1.998102
FunctionalModel.fit <- function(metric, model, par=NULL,
                                fitters = FunctionalModel.fit.defaultFitters(length(metric@x), model@paramCount)) {
  #  is the input data valid?
  if(is.null(metric) || is.null(model) || is.null(fitters)) { return(NULL); }

  # do we even have any fitter?
  fitterCount <- length(fitters);
  if(fitterCount <= 0L) { return(NULL); }

  # initialize best holders
  bestQuality <- +Inf;
  bestParams  <- NULL;
  bestResult  <- NULL;

  # let's check if there is a valid starting point
  if(FunctionalModel.par.check(model, par)) {
    # ok, there is one, but is it valid?
    q <- metric@quality(model@f, par);
    if(learning.checkQuality(bestQuality)) {
      # yets it is
      bestQuality <- q;
      bestParams  <- par;
    }
  }

  # if we do not have a valid start point, give each algorithm 3 trials
  if(is.null(bestParams)) {
    times <- 3L;
  } else { # if we have a start point, only 1 trial
    times <- 1L;
  }

  improved        <- TRUE;
  fitterQualities <- rep(+Inf, fitterCount);

  # if par==NULL, this cycle will loop at least two times
  while(improved) {
    improved <- FALSE;

    # apply all fitters
    for(i in 1L:fitterCount) {
      # apply a fitter iff it did not YET see the best solution so far
      if((!(is.finite(bestQuality))) || (fitterQualities[i] > bestQuality)) {
        # we apply it then either once or three times
        for(j in 1L:times) {
          # call the fitter
          res <- fitters[[i]](metric=metric, model=model, par=bestParams);
          # check the result
          if(!(is.null(res))) {
            if(res@quality < fitterQualities[i]) {
              # the new best solution seen by the fitter
              fitterQualities[i] <- res@quality;
            }
            # but is this maybe even the best result so far?
            if(is.null(bestResult) || (res@quality < bestResult@quality)) {
              bestResult <- res;
              improved   <- TRUE; # yes, we made an improvement!
            }
          }
        }
      }
    }

    # if we get here but did not see any valid result, we can as well give up
    if(is.null(bestResult)) { break; }

    # if the best result's quality is not better than what we had last time, we
    # can also stop
    if(bestResult@quality >= bestQuality) {
      bestResult <- NULL;
      break;
    }

    # ok, we have an improvement, use the current best as starting point for the
    # next iteration
    bestQuality <- bestResult@quality;
    bestParams  <- bestResult@par;
    times       <- 1L;
  }

  # there was no improvement or even no valid result
  if(is.null(bestResult) &&
     FunctionalModel.par.check(model, bestParams) &&
     learning.checkQuality(bestQuality)) {
    # strange, ok, let's try to build a new solution
    # this could happen if we have a valid starting point which is somehow
    # surrounded by only invalid solutions
    return(FittedFunctionalModel.new(model, bestParams, bestQuality));
  }

  # best result is either something good or NULL
  return(bestResult);
}
