#' @include TransformedFittedFunctionalModel.R
#' @include defaultFitters.R
#' @include fit.R
#'
#' @title Fit the Given Model Blueprint to the Specified Data
#'
#' @description Apply a set of fitters iteratively to fit the specified model to
#'   the given data. First, we generate a starting guess about the
#'   parameterization via \code{\link{par.estimate}} (or accept it via the
#'   parameter \code{par}). From then on, we apply the different function
#'   fitters one by one. All the fitters who have not produced the current best
#'   solution are applied again, to the now-best guess. However, we do not apply
#'   the fitters that have produced that very guess in the next round. (They may
#'   get a chance again in a later turn.) Anyway, this procedure is iterated
#'   until no improvement can be made anymore. After finishing the fitting, we
#'   attempt whether rounding the fitted parameters to integers can improve the
#'   fitting quality.
#'
#' @param metric an instance of
#'   \code{regressoR.quality::RegressionQualityMetric}
#' @param model an instance of \code{\link{FunctionalModel}}
#' @param par the initial starting point
#' @param fitters the fitters
#' @param transformed.data the transformed data for the first fitting step
#' @param transformed.metric the transformed metric for the first fitting step
#' @return On success, an instance of \code{\link{FittedFunctionalModel}}.
#'   \code{NULL} on failure.
#' @export model.fit.transformed
#' @importFrom learnerSelectoR learning.checkQuality
#' @importFrom regressoR.functional.models FunctionalModel.new
#' @examples
#' set.seed(234345L)
#'
#' orig.f.x.par <- function(x, par) exp(par[1] + par[2]*x - par[3]*x*x);
#' orig.par <- c(0.2, -0.3, 0.4);
#' orig.f.x <- function(x) orig.f.x.par(x, orig.par);
#' orig.x <- (-100:100)*0.05;
#' orig.y <- orig.f.x(orig.x);
#' noisy.x <- rnorm(n=length(orig.x), mean=orig.x, sd=0.05);
#' noisy.y <- rnorm(n=length(orig.y), mean=orig.y, sd=0.05*orig.y);
#'
#' transformed.data <- dataTransformeR::TransformedData2D.new(
#'   dataTransformeR::Transformation.normalize(noisy.x),
#'   dataTransformeR::Transformation.log(noisy.y));
#'
#' metric <- regressoR.quality::default(noisy.x, noisy.y);
#' transformed.metric <- regressoR.quality::default(transformed.data@x@data, transformed.data@y@data);
#' model <- regressoR.functional.models::quadratic();
#' result <- model.fit.transformed(metric, model, transformed.data, transformed.metric);
#'
#' result.2 <- learnerSelectoR::learning.Result.finalize(result)
#' plot(noisy.x, noisy.y)
#' lines(noisy.x, result.2@f(noisy.x), col="red")
model.fit.transformed <- function(metric, model, transformed.data=NULL, transformed.metric=NULL,
                                  par=NULL,
                                  fitters = model.fit.defaultFitters(base::length(metric@x), model@paramCount)) {
  if(base::is.null(transformed.data)) {
    if(base::is.null(transformed.metric)) {
      return(model.fit(metric=metric, model=model,par=par, fitters=fitters));
    } else {
      base::stop("Transformed metric must be NULL if transformed data is NULL.");
    }
  } else {
    if(base::is.null(transformed.metric)) {
      base::stop("Transformed metric canot be NULL if transformed data is not NULL.");
    }
  }

  result <- model.fit(metric=transformed.metric, model=model, par=par,
                      fitters=fitters);
  if(base::is.null(result)) {
    return(NULL);
  }
  if(base::identical(transformed.metric, metric)) {
    # This is highly odd, the transformed metric and the metric are the same.
    # OK, then we can quite here as well.
    return(result);
  }

  f <- model@f;
  f.x <- transformed.data@x@transformation@forward;
  f.x.i <- base::identical(f.x, identity);
  f.y <- transformed.data@y@transformation@backward;
  f.y.i <- base::identical(f.y, identity);
  if(f.x.i) {
    if(f.y.i) {
      # if all data is identity transformed, we can stop here
      base::stop("Transformation cannot be identity if metrics differ.");
    } else {
      # x is identity, y is not
      f.n <- function(x, par) f.y(f(x, par));
    }
  } else {
    # x is not identity
    if(f.y.i) {
      # y is identity, x not
      f.n <- function(x, par) f(x, par);
    } else {
      # neither is
      f.n <- function(x, par) f.y(f(f.x(x), par));
    }
  }

  # create a temporary model for the fitting procedure
  model.temp <- regressoR.functional.models::FunctionalModel.new(f=f.n, paramCount = model@paramCount,
                                                                 paramLower=model@paramLower,
                                                                 paramUpper = model@paramUpper);
  # fit the model, starting with the current parameterization
  result.2 <- model.fit(metric, model.temp, par=result@par, fitters = fitters);
  if(base::is.null(result.2)) {
    # if we failed, let's see whether we can just use the original result
    result@quality <- metric@quality(f.n, result@par);
    if(learnerSelectoR::learning.checkQuality(result@quality)) {
      # ok, we can
      return(result);
    }
    # nope, strange
    return(NULL);
  }

  # OK, we have fitted everything, so we can return a new model
  return(TransformedFittedFunctionalModel.new(
            model=model, par=result.2@par, quality=result.2@quality,
            transform.x = transformed.data@x@transformation@forward,
            transform.x.complexity = transformed.data@x@transformation@complexity,
            transform.y = transformed.data@y@transformation@backward,
            transform.y.complexity = transformed.data@y@transformation@complexity));
}
