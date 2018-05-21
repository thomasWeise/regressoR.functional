library("regressoR.functional")
context("FunctionalModel.defaultLearners")

slow.tests <- (is.na(Sys.getenv("TRAVIS", unset=NA)))

test_that("Test FunctionalModel.defaultLearners", {
  learners <- FunctionalModel.defaultLearners();
  expect_true(!is.null(learners));
  expect_true(is.list(learners));
  expect_gt(length(learners), 0L);

  x <- runif(10, min=1, max=10);
  params <- c(5, 7);
  func <- function(x) { params[1] + params[2]*x };
  y <- func(x);

  metric <- regressoR.quality::RegressionQualityMetric.default(x, y);
  full.tests <- TRUE;

  for(learner in learners) {
    expect_true(is.function(learner));
    if(full.tests) {
      result <- learner(metric, NULL, NULL, NULL, 0.75);
      expect_true(!is.null(result));
      expect_is(result, "FittedFunctionalModel");
      validObject(result);
    }
    full.tests <- full.tests && slow.tests;
  }
})



test_that("Test FunctionalModel.monotonousLearners", {
  learners <- FunctionalModel.monotonousLearners();
  expect_true(!is.null(learners));
  expect_true(is.list(learners));
  expect_gt(length(learners), 0L);

  x <- runif(10, min=1, max=10);
  params <- c(5, 7);
  func <- function(x) { params[1] + params[2]*x };
  y <- func(x);

  metric <- regressoR.quality::RegressionQualityMetric.default(x, y);
  full.tests <- TRUE;

  for(learner in learners) {
    expect_true(is.function(learner));
    if(full.tests) {
      result <- learner(metric, NULL, NULL, NULL, 0.75);
      expect_true(!is.null(result));
      expect_is(result, "FittedFunctionalModel");
      validObject(result);
    }
    full.tests <- full.tests && slow.tests;
  }
})
