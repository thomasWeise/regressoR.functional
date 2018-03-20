#' @include TransformedFittedFunctionalModel.R
#' @include defaultFitters.R
#' @include fit.R

#' @title Fit the Given Model Blueprint to the Specified Data
#'
#' @description Apply a set of fitters iteratively to fit the specified model to
#' the given data. First, we generate a starting guess about the
#' parameterization via \code{\link{FunctionalModel.par.estimate}} (or accept it
#' via the parameter \code{par}). From then on, we apply the different function
#' fitters one by one. All the fitters who have not produced the current best
#' solution are applied again, to the now-best guess. However, we do not apply
#' the fitters that have produced that very guess in the next round. (They may
#' get a chance again in a later turn.) Anyway, this procedure is iterated until
#' no improvement can be made anymore. After finishing the fitting, we attempt
#' whether rounding the fitted parameters to integers can improve the fitting
#' quality.
#'
#' @param metric an instance of
#'   \code{regressoR.quality::RegressionQualityMetric}
#' @param model an instance of \code{\link{FunctionalModel}}
#' @param par the initial starting point
#' @param fitters the fitters
#' @param transformation.x the transformation along the \code{x}-axis, or
#'   \code{NULL} if none was applied to the data
#' @param transformation.y the transformation along the \code{y}-axis, or
#'   \code{NULL} if none was applied to the data
#' @param metric.transformed the transformed metric for the first fitting step
#' @return On success, an instance of \code{\link{FittedFunctionalModel}}.
#'   \code{NULL} on failure.
#' @export FunctionalModel.fit.transformed
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
#' metric <- regressoR.quality::RegressionQualityMetric.default(noisy.x, noisy.y);
#' metric.transformed <- regressoR.quality::RegressionQualityMetric.default(transformed.data@x@data,
#'                                                  transformed.data@y@data);
#' model <- regressoR.functional.models::FunctionalModel.quadratic();
#' result <- FunctionalModel.fit.transformed(metric, model,
#'                                 transformed.data@x@transformation,
#'                                 transformed.data@y@transformation,
#'                                 metric.transformed);
#'
#' result.2 <- learnerSelectoR::learning.Result.finalize(result)
#' plot(noisy.x, noisy.y)
#' lines(noisy.x, result.2@f(noisy.x), col="red")
FunctionalModel.fit.transformed <- function(metric, model,
                                  transformation.x=NULL, transformation.y=NULL,
                                  metric.transformed=NULL,
                                  par=NULL,
                                  fitters = FunctionalModel.fit.defaultFitters(base::length(metric@x), model@paramCount)) {

  f.x.i <- base::is.null(transformation.x);
  f.y.i <- base::is.null(transformation.y);
  if(f.x.i && f.y.i) {
    if(base::is.null(metric.transformed)) {
      return(FunctionalModel.fit(metric=metric, model=model,par=par, fitters=fitters));
    } else {
      base::stop("Transformed metric must be NULL if transformations are both NULL.");
    }
  } else {
    if(base::is.null(metric.transformed)) {
      base::stop("Transformed metric canot be NULL if at least one transformation is not NULL.");
    }
  }

  result <- FunctionalModel.fit(metric=metric.transformed, model=model, par=par,
                      fitters=fitters);
  if(base::is.null(result)) {
    return(NULL);
  }
  if(base::identical(metric.transformed, metric)) {
    # This is highly odd, the transformed metric and the metric are the same.
    # OK, then we can quite here as well.
    return(result);
  }

  f <- model@f;

  if(!f.x.i) {
    f.x <- transformation.x@forward;
    f.x.i <- base::identical(f.x, identity);
  }

  if(!f.y.i) {
    f.y <- transformation.y@backward;
    f.y.i <- base::identical(f.y, identity);
  }

  if(f.x.i) {
    if(f.y.i) {
      # if all data is identity transformed, we can stop here
      base::stop("Transformations cannot both be identity if metrics differ.");
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
  result.2 <- FunctionalModel.fit(metric, model.temp, par=result@par, fitters = fitters);
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
            transform.x = transformation.x@forward,
            transform.x.complexity = transformation.x@complexity,
            transform.y = transformation.y@backward,
            transform.y.complexity = transformation.y@complexity));
}
