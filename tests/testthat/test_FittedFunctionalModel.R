library("regressoR.functional")
context("FittedFunctionalModel")

test_that("Test FittedFunctionalModel constructor", {
  f <- function(x, par) par[1] + (par[2] * (x + (par[3] * x)));
  functionalModel <- regressoR.functional.models::FunctionalModel.new(f=f, paramCount=3L);
  par <- c(5, 3, 4);
  quality = 7;
  size = length(par);

  fitted <- new("FittedFunctionalModel", f=function(x) f(x, par), model=functionalModel, quality=quality, par=par, size=size);
  validObject(fitted);
  expect_identical(fitted@model, functionalModel)
  expect_identical(fitted@par, par);
  expect_identical(fitted@size, size);
})

test_that("Test FittedFunctionalModel constructorwith error", {
  f <- function(x, par) par[1] + (par[2] * (x + (par[3] * x)));
  functionalModel <- regressoR.functional.models::FunctionalModel.new(f=f, paramCount=3L);
  par <- c(5, 3, 4);
  quality = 7;

  expect_error(new("FittedFunctionalModel", f=function(x) f(x, par), model=functionalModel, quality=quality, par=par, size=length(par)+1));
  expect_error(new("FittedFunctionalModel", model=functionalModel, quality=quality, par=par));
  expect_error(new("FittedFunctionalModel", f=function(x) f(x, par), quality=quality, par=par));
  expect_error(new("FittedFunctionalModel", f=function(x) f(x, par), model=functionalModel, par=par));
  expect_error(new("FittedFunctionalModel", f=function(x) f(x, par), model=functionalModel, quality=quality));
  expect_error(new("FittedFunctionalModel", f=function(x) f(x, par), model=functionalModel, quality=-5, par=par));
})

test_that("Test FittedFunctionalModel.new", {
  f <- function(x, par) par[1] + (par[2] * (x + (par[3] * x)));
  functionalModel <- regressoR.functional.models::FunctionalModel.new(f=f, paramCount=3L);
  par <- c(5, 3, 4);
  quality = 7;

  fitted <- FittedFunctionalModel.new(model=functionalModel, quality=quality, par=par);
  validObject(fitted);
  expect_identical(fitted@model, functionalModel)
})


test_that("Test FittedFunctionalModel.new with error", {
  f <- function(x, par) par[1] + (par[2] * (x + (par[3] * x)));
  functionalModel <- regressoR.functional.models::FunctionalModel.new(f=f, paramCount=3L);
  par <- c(5, 3, 4);
  quality = 7;

  expect_error(FittedFunctionalModel.new(quality=quality, par=par));
  expect_error(FittedFunctionalModel.new(model=functionalModel, par=par));
  expect_error(FittedFunctionalModel.new(model=functionalModel, quality=quality));
  expect_error(FittedFunctionalModel.new(model=functionalModel, quality=-3, par=par));
  expect_error(FittedFunctionalModel.new(model=functionalModel, quality=quality, par=c(1,2,+Inf)));
})


test_that("Test learning.Result.finalize", {
  f <- function(x, par) par[1] + (par[2] * (x + (par[3] * x)));
  functionalModel <- regressoR.functional.models::FunctionalModel.new(f=f, paramCount=3L);
  par <- c(5, 3, 4);
  quality = 7;

  fitted <- FittedFunctionalModel.new(model=functionalModel, quality=quality, par=par);
  expect_identical(fitted@model, functionalModel)
  fitted <- learning.Result.finalize(fitted);
  expect_identical(fitted@model, functionalModel)
  validObject(fitted);
})