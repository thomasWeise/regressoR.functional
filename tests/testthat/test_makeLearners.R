library("regressoR.functional")
context("FunctionalModel.makeLearners")

test_that("Test FunctionalModel.makeLearners", {
  learners <- FunctionalModel.makeLearners();
  expect_true(!is.null(learners));
  expect_true(is.list(learners));
  expect_gt(length(learners), 0L);

  x <- rnorm(10);
  params <- c(5, 7);
  func <- function(x) { params[1] + params[2]*x };
  y <- func(x);

  metric <- regressoR.quality::RegressionQualityMetric.default(x, y);

  for(learner in learners) {
    expect_true(is.function(learner));
    result <- learner(metric, NULL, NULL, NULL);
    expect_true(!is.null(result));
    expect_is(result, "FittedFunctionalModel");
    validObject(result);
  }
})
