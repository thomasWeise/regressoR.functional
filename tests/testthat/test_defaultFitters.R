library("regressoR.functional")
context("FunctionalModel.fit.defaultFitters")

test_that("Test FunctionalModel.fit.defaultFitters plain", {
  list <- FunctionalModel.fit.defaultFitters();
  expect_true(!is.null(list));
  expect_true(length(list) > 0);
  for(index in 1:length(list)) {
    func <- list[[index]];
    expect_true(!is.null(func));
    expect_true(is.function(func));
    expect_identical(names(formals(func)), c("metric", "model", "par"));
    if(index > 1) {
      for(j in 1:(index-1)) {
        expect_false(identical(list[[j]], func));
      }
    }
  }
})




test_that("Test FunctionalModel.fit.defaultFitters combos", {
  params <- c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L);
  sizes  <- c(1L:10L, 2L^(1L:20L), 33L*1L:100L);

  for(param in params) {
    for(size in sizes) {
      list <- FunctionalModel.fit.defaultFitters();
      expect_true(!is.null(list));
      expect_true(length(list) > 0);
      for(index in 1:length(list)) {
        func <- list[[index]];
        expect_true(!is.null(func));
        expect_true(is.function(func));
        expect_identical(names(formals(func)), c("metric", "model", "par"));
        if(index > 1) {
          for(j in 1:(index-1)) {
            expect_false(identical(list[[j]], func));
          }
        }
      }
    }
  }
})