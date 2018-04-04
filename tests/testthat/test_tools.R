library("regressoR.functional")
context("fit.tools")

test_that("Test .fix.boundaries with both boundaries fully specified", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  functionalModel <- regressoR.functional.models::FunctionalModel.new(f=f, paramCount=3L, gradient=grad, paramUpper=c(3,4,5), paramLower=c(1,2,3))

  boundaries <- .fix.boundaries(functionalModel);
  expect_length(boundaries$lower, 3L);
  expect_length(boundaries$upper, 3L);
  expect_identical(boundaries$lower, functionalModel@paramLower)
  expect_identical(boundaries$upper, functionalModel@paramUpper)
})

test_that("Test .fix.boundaries with upper boundaries fully specified", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  functionalModel <- regressoR.functional.models::FunctionalModel.new(f=f, paramCount=3L, gradient=grad, paramUpper=c(3,4,5))

  boundaries <- .fix.boundaries(functionalModel);
  expect_null(boundaries$lower);
  expect_length(boundaries$upper, 3L);
  expect_identical(boundaries$upper, functionalModel@paramUpper)
})

test_that("Test .fix.boundaries with lower boundary fully specified", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  functionalModel <- regressoR.functional.models::FunctionalModel.new(f=f, paramCount=3L, gradient=grad, paramLower=c(1,2,3))

  boundaries <- .fix.boundaries(functionalModel);
  expect_length(boundaries$lower, 3L);
  expect_null(boundaries$upper);
  expect_identical(boundaries$lower, functionalModel@paramLower)
})

test_that("Test .fix.boundaries with no boundary specified", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  functionalModel <- regressoR.functional.models::FunctionalModel.new(f=f, paramCount=3L, gradient=grad)

  expect_null(.fix.boundaries(functionalModel));
})


test_that("Test .fix.boundaries with both boundaries partially specified", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  lower <- c(1, NA, NA, 5);
  upper <- c(NA, NA, 4, 10);
  functionalModel <- regressoR.functional.models::FunctionalModel.new(f=f, paramCount=4L, gradient=grad, paramLower=lower, paramUpper=upper)

  boundaries <- .fix.boundaries(functionalModel);
  expect_length(boundaries$lower, 4L);
  expect_length(boundaries$upper, 4L);
  expect_identical(boundaries$lower, c(1, -1e10, 4-1e10, 5));
  expect_identical(boundaries$upper, c(1+1e10, 1e10, 4, 10));
})



test_that("Test .fix.boundaries with upper boundaries partially specified", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  upper <- c(NA, NA, 4, 10);
  functionalModel <- regressoR.functional.models::FunctionalModel.new(f=f, paramCount=4L, gradient=grad, paramUpper=upper)

  boundaries <- .fix.boundaries(functionalModel);
  expect_null(boundaries$lower, 4L);
  expect_identical(boundaries$upper, c(1e10, 1e10, 4, 10));
})


test_that("Test .fix.boundaries with lower boundaries partially specified", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  lower <- c(1, NA, NA, 5);
  functionalModel <- regressoR.functional.models::FunctionalModel.new(f=f, paramCount=4L, gradient=grad, paramLower=lower)

  boundaries <- .fix.boundaries(functionalModel);
  expect_length(boundaries$lower, 4L);
  expect_null(boundaries$upper);
  expect_identical(boundaries$lower, c(1, -1e10, -1e10, 5));
})



test_that("Test .make.initial.pop works correctly", {
  models <- regressoR.functional.models::FunctionalModel.all();
  for(i in 1:50) {
    model <- models[[max(1, min(length(models), as.integer(runif(n=1, min=1, max=length(models)+1))))]];
    n <- model@paramCount;
    NP <- as.integer(runif(n=1, min=2, max=20));

    if(runif(1) > 0.5) {
      lower <- NULL;
    } else {
      lower <- runif(n, min=-100, max=100);
    }
    if(runif(1) > 0.5) {
      upper <- NULL;
    } else {
      if(is.null(lower)) {
        upper <- runif(n=n, min=-100, max=100);
      } else {
        upper <- runif(n=n, min=lower+1e-7, max=lower+200);
      }
    }
    if(!(is.null(lower))) {
      while(runif(1) > 0.5) { lower[as.integer(runif(n)) + 1] <- NA; }
    }
    if(!(is.null(upper))) {
      while(runif(1) > 0.5) { upper[as.integer(runif(n)) + 1] <- NA; }
    }

    model@paramLower <- lower;
    model@paramUpper <- upper;
    pop = .make.initial.pop( runif(n=n, min=-200, max=200),
                             runif(n=n, min=-200, max=200),
                             NP=NP,
                             model=model)

    for(j in 1:NP) {
      x <- pop[j,];
      for(k in 1:n) {
        expect_true(is.finite(x[k]));
        if(!(is.null(lower) || is.na(lower[k]))) {
          expect_true(x[k] >= lower[k]);
        }
        if(!(is.null(upper) || is.na(upper[k]))) {
          expect_true(x[k] <= upper[k]);
        }
      }
    }
  }
})
