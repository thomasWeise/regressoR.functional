library("regressoR.functional")
context("FunctionalModel.fit.de")

test_that("Test FunctionalModel.fit.de", {

  x <- rnorm(10);
  params <- c(5, 7);
  func <- function(x) { params[1] + params[2]*x };
  y <- func(x);

  functionalModel <- regressoR.functional.models::FunctionalModel.linear();
  metric <- regressoR.quality::RegressionQualityMetric.default(x, y)
  start <- functionalModel@estimator(x, y)
  result <- FunctionalModel.fit.de(metric, functionalModel, start);
  expect_identical(is.null(result), FALSE);
  expect_is(result, "FittedFunctionalModel");
  expect_equal(result@par, params);
  expect_equal(result@f(x), y);
  expect_equal(result@quality, 0);
  expect_identical(result@quality, metric@quality(functionalModel@f, result@par));

  yr <- y+0.01*rnorm(length(y))
  metricr <- regressoR.quality::RegressionQualityMetric.default(x, yr);
  startr <- functionalModel@estimator(x, yr);
  result <- FunctionalModel.fit.de(metricr, functionalModel, startr);
  expect_identical(is.null(result), FALSE);
  expect_is(result, "FittedFunctionalModel");
  expect_identical( sum((result@par-params)^2) < 0.1, TRUE);
  expect_identical(result@quality < 0.1, TRUE);
  expect_identical(result@quality, metricr@quality(functionalModel@f, result@par));

  functionalModel <- regressoR.functional.models::FunctionalModel.quadratic();
  startr <- functionalModel@estimator(x, yr);
  result <- FunctionalModel.fit.de(metricr, functionalModel, startr);
  expect_identical(is.null(result), FALSE);
  expect_is(result, "FittedFunctionalModel");
  expect_identical( sum((result@par-c(params,0))^2) < 0.1, TRUE);
  expect_identical(result@quality < 0.1, TRUE);
  expect_identical(result@quality, metricr@quality(functionalModel@f, result@par));
})
