library("regressoR.functional")
context("TransformedFittedFunctionalModel")

test_that("Test TransformedFittedFunctionalModel constructor", {
  model <- regressoR.functional.models::quadratic();
  par <- c(3, -4, 2)
  f <- function(x) model@f(x, par)
  t.x <- function(x) 3*x+2;
  t.y <- function(x) 0.2*(x-3.1)
  size <- 10L;
  quality <- 2;
  fitted <- new("TransformedFittedFunctionalModel",
                model=model, par=par, quality=quality,
                transform.x=t.x,
                transform.y=t.y,
                f=f,
                size=size);

  validObject(fitted);
  expect_identical(fitted@model, model);
  expect_identical(fitted@par, par);
  expect_identical(fitted@quality, quality);
  expect_identical(fitted@size, size);
  expect_identical(fitted@transform.x, t.x);
  expect_identical(fitted@transform.y, t.y);
})

test_that("Test FittedFunctionalModel constructor with error", {
  model <- regressoR.functional.models::quadratic();
  par <- c(3, -4, 2)
  t.x <- function(x) 3*x+2;
  t.y <- function(x) 0.2*(x-3.1)
  f <- function(x) model@f(x, par)
  size <- 10L;
  quality <- 2;

  expect_error(new("TransformedFittedFunctionalModel", f=f, model=model, quality=quality, par=par, size=length(par)+1));
  expect_error(new("TransformedFittedFunctionalModel", model=model, quality=quality, par=par));
  expect_error(new("TransformedFittedFunctionalModel", f=f, quality=quality, par=par));
  expect_error(new("TransformedFittedFunctionalModel", f=f, model=model, par=par));
  expect_error(new("TransformedFittedFunctionalModel", f=f, model=model, quality=quality));
  expect_error(new("TransformedFittedFunctionalModel", f=f, model=model, quality=-5, par=par));
})

test_that("Test FittedFunctionalModel.new", {
  model <- regressoR.functional.models::quadratic();
  par <- c(3, -4, 2)
  t.x <- function(x) 3*x+2;
  t.y <- function(x) 0.2*(x-3.1)
  quality <- 2;

  fitted <- TransformedFittedFunctionalModel.new(model=model, par=par, quality=quality,
                                                 transform.x=t.x,
                                                 transform.x.complexity=3L,
                                                 transform.y=t.y,
                                                 transform.y.complexity=3L);

  validObject(fitted);
  expect_identical(fitted@model, model);
  expect_identical(fitted@par, par);
  expect_identical(fitted@quality, quality);
  expect_identical(fitted@size, 6L + model@paramCount);
  expect_identical(fitted@transform.x, t.x);
  expect_identical(fitted@transform.y, t.y);
})


test_that("Test TransformedFittedFunctionalModel.new with error", {
  model <- regressoR.functional.models::quadratic();
  par <- c(3, -4, 2)
  t.x <- function(x) 3*x+2;
  t.y <- function(x) 0.2*(x-3.1)
  quality <- 2;

  expect_error(TransformedFittedFunctionalModel.new(quality=quality, par=par));
  expect_error(TransformedFittedFunctionalModel.new(model=model, par=par));
  expect_error(TransformedFittedFunctionalModel.new(model=model, quality=quality));
  expect_error(TransformedFittedFunctionalModel.new(model=model, quality=-3, par=par));
  expect_error(TransformedFittedFunctionalModel.new(model=model, quality=quality, par=c(1,2,+Inf)));
})


test_that("Test learning.Result.finalize", {
  model <- regressoR.functional.models::quadratic();
  par <- c(3, -4, 2)
  t.x <- function(x) 3*x+2;
  t.y <- function(x) 0.2*(x-3.1)
  quality <- 2;

  fitted <- TransformedFittedFunctionalModel.new(model=model, par=par, quality=quality,
                                                 transform.x=t.x,
                                                 transform.x.complexity=3L,
                                                 transform.y=t.y,
                                                 transform.y.complexity=3L);

  validObject(fitted);
  expect_identical(fitted@model, model);
  expect_identical(fitted@par, par);
  expect_identical(fitted@quality, quality);
  expect_identical(fitted@size, 6L + model@paramCount);
  expect_identical(fitted@transform.x, t.x);
  expect_identical(fitted@transform.y, t.y);

  fitted2 <- learning.Result.finalize(fitted);
  expect_identical(fitted2@model, model);
  expect_identical(fitted2@par, par);
  expect_identical(fitted2@quality, quality);
  expect_identical(fitted2@size, 6L + model@paramCount);
  expect_identical(fitted2@transform.x, t.x);
  expect_identical(fitted2@transform.y, t.y);
})