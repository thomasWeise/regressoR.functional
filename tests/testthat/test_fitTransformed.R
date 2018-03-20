library("regressoR.functional")
context("model.fit.transformed")

test_that("Test model.fit.transformed", {
  set.seed(234345L)

  orig.f.x.par <- function(x, par) exp(par[1] + par[2]*x - par[3]*x*x);
  orig.par <- c(0.2, -0.3, 0.4);
  orig.f.x <- function(x) orig.f.x.par(x, orig.par);
  orig.x <- (-100:100)*0.05;
  orig.y <- orig.f.x(orig.x);
  noisy.x <- rnorm(n=length(orig.x), mean=orig.x, sd=0.05);
  noisy.y <- rnorm(n=length(orig.y), mean=orig.y, sd=0.05*orig.y);

  transformed.data <- dataTransformeR::TransformedData2D.new(
    dataTransformeR::Transformation.normalize(noisy.x),
    dataTransformeR::Transformation.log(noisy.y));
  expect_is(transformed.data, "TransformedData2D");

  metric <- regressoR.quality::default(noisy.x, noisy.y);
  expect_is(metric, "RegressionQualityMetric");

  transformed.metric <- regressoR.quality::default(transformed.data@x@data, transformed.data@y@data);
  expect_is(transformed.metric, "RegressionQualityMetric");

  model <- regressoR.functional.models::quadratic();
  expect_is(model, "FunctionalModel");

  result <- model.fit.transformed(metric, model, transformed.data, transformed.metric);
  expect_is(result, "TransformedFittedFunctionalModel");
  expect_lt(result@quality, 0.5);
  validObject(result);
  expect_identical(result@transform.x, transformed.data@x@transformation@forward);
  expect_identical(result@transform.y, transformed.data@y@transformation@backward);
  expect_identical(result@model, model);
  expect_identical(result@size, model@paramCount +
                                transformed.data@x@transformation@complexity +
                                transformed.data@y@transformation@complexity);

  result.2 <- learnerSelectoR::learning.Result.finalize(result)
  expect_is(result.2, "TransformedFittedFunctionalModel");
  expect_lt(result.2@quality, 0.5);
  validObject(result.2);
  expect_identical(result.2@transform.x, transformed.data@x@transformation@forward);
  expect_identical(result.2@transform.y, transformed.data@y@transformation@backward);
  expect_identical(result.2@model, model);
  expect_identical(result.2@size, model@paramCount +
                     transformed.data@x@transformation@complexity +
                     transformed.data@y@transformation@complexity);
})
