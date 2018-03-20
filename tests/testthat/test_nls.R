library("regressoR.functional")
context("model.fit.nls")

test_that("Test model.fit.nls", {

  x <- rnorm(10);
  params <- c(5, 7);
  func <- function(x) { params[1] + params[2]*x };
  y <- func(x);

  functionalModel <- regressoR.functional.models::linear();
  metric <- regressoR.quality::default(x, y)
  start <- functionalModel@estimator(x, y)
  result <- model.fit.nls(metric, functionalModel, start);
  if(!is.null(result)) {
    expect_is(result, "FittedFunctionalModel");
    expect_equal(result@par, params);
    expect_equal(result@f(x), y);
    expect_equal(result@quality, 0);
    expect_identical(result@quality, metric@quality(functionalModel@f, result@par));
  }

  yr <- y+0.01*rnorm(length(y))
  metricr <- regressoR.quality::default(x, yr);
  startr <- functionalModel@estimator(x, yr);
  result <- model.fit.nls(metricr, functionalModel, startr);
  expect_identical(is.null(result), FALSE);
  expect_is(result, "FittedFunctionalModel");
  expect_identical( sum((result@par-params)^2) < 0.1, TRUE);
  expect_identical(result@quality < 0.1, TRUE);
  expect_identical(result@quality, metricr@quality(functionalModel@f, result@par));

  functionalModel <- regressoR.functional.models::quadratic();
  startr <- functionalModel@estimator(x, yr);
  result <- model.fit.nls(metricr, functionalModel, startr);
  if(!is.null(result)) {
    expect_identical(is.null(result), FALSE);
    expect_is(result, "FittedFunctionalModel");
    expect_identical( sum((result@par-c(params,0))^2) < 0.1, TRUE);
    expect_identical(result@quality < 0.1, TRUE);
    expect_identical(result@quality, metricr@quality(functionalModel@f, result@par));
  }
})
