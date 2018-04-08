library("regressoR.functional")
context("as.character")

test_that("Test as.character", {

  x <- rnorm(10);
  params <- c(5, 7);
  func <- function(x) { params[1] + params[2]*x };
  y <- func(x);

  functionalModel <- regressoR.functional.models::FunctionalModel.linear();
  metric <- regressoR.quality::RegressionQualityMetric.default(x, y)
  start <- functionalModel@estimator(x, y)
  result <- FunctionalModel.fit.nlslm(metric, functionalModel, start);
  expect_true(!is.null(as.character(result)));
})



test_that("Test as.character", {

  data.x <- rnorm(10);
  params <- c(5, 7);
  func <- function(x) { 0.1 + abs(params[1] + params[2]*x) };
  data.y <- func(data.x);

  functionalModel <- regressoR.functional.models::FunctionalModel.linear();
  metric <- regressoR.quality::RegressionQualityMetric.default(data.x, data.y)
  transformation.x <- dataTransformeR::Transformation.normalize(data.x);
  transformation.y <- dataTransformeR::Transformation.log(data.y);
  metric.transformed <- regressoR.quality::RegressionQualityMetric.default(transformation.x@data,
                                                                           transformation.y@data);

  result <- FunctionalModel.fit.transformed(metric, functionalModel,
           transformation.x@transformation, transformation.y@transformation,
           metric.transformed,
           q=0.75,
           fitter=FunctionalModel.fit.nlslm);

  expect_true(!is.null(as.character(result)));
})
