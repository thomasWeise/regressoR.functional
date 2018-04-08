# Compare the Performance of the Different Function Fitters

# This script is used to compare the performance of different functional fitters
# on simple example problems of different scale. Its results will help to
# decide about the default solvers to apply.

# We start this script off by finding some problems which are neither too hard
# nor too easy. For each problem, we first parameterize our model, then sample x
# and y coordinates. We then add noise to the coordinates and check whether all
# solvers can solve the problem and whether the solution quality is in
# reasonable bounds. Once we have sufficiently many such problems of different
# scales, we do the real benchmarking.

library(regressoR.functional);
library(regressoR.functional.models);
library(regressoR.quality);
library(microbenchmark);
library(graphics);
library(MASS);

set.seed(2500234L);
minDataSize  <- 10L;
maxDataSize  <- 1000L;
numProblems  <- 66L;
numRuns      <- 33L;
numTests     <- as.integer(((3*numRuns) / 2) ^ 1.1);

model        <- regressoR.functional.models::FunctionalModel.logistic.1();
model.lower  <- rep(0.01, model@paramCount);
model.upper  <- rep(3, model@paramCount);
x.min        <- 0.01;
x.max        <- 5;

cat("### Compare the Performance of the Different Function Fitters ###\n");

methods.names <- c("nlslm", "minqa", "cmaes", "de", "dfoptim", "nls", "lbfgsb", "pso");
cat("       ", length(methods.names), "  methods: ", paste(methods.names, collapse=", "), "\n", sep="");
cat(" mininum data size: ", minDataSize, "\n", sep="");
cat(" maximum data size: ", maxDataSize, "\n", sep="");
cat("             model: ", model@name, " - ", toString(deparse(body(model@f))), " with ", model@paramCount, " parameters\n", sep="");
cat("number of problems: ", numProblems, "\n", sep="");
cat("  runs per problem: ", numRuns, "\n", sep="");
cat(" tests per problem: ", numTests, "\n", sep="");

# make the set of methods
methods.calls <- lapply(X=methods.names,
                        FUN=function(t) {
                          result <- get(paste(c("FunctionalModel.fit.", t), collapse=""));
                          result <- force(result);
                          return(result);
                        });

# check whether there is any method that cannot solve a given problem (numTests times)
# and that the problem is neither too easy nor too hard.
canSolve <- function(problem) {
  for(method in methods.calls) {
    for(i in 1:numTests) {
      result <- method(metric=problem$metric, model=model, par=problem$par);
      if(is.null(result) ||
        ((!(is.finite(result@quality))) ||
         (result@quality <= 0.1) || (result@quality >= 2))) {
        return(FALSE);
      }
    }
  }
  return(TRUE);
}

# create a solveable problem of the given size
makeProblem <- function(dataSize) {
  cat("new problem of size ", dataSize, sep="", collapse="");
  repeat {
    repeat {
      repeat {
        problem.par <- runif(n=model@paramCount, min=model.lower, max=model.upper);
        if(regressoR.functional.models::FunctionalModel.par.check(model, problem.par)) {
          break;
        }
      }

      repeat {
        problem.x <- runif(n=dataSize, min=x.min, max=x.max);
        if(length(unique(problem.x)) >= dataSize) {
          break;
        }
      }

      problem.f <- function(x) model@f(x, problem.par);
      problem.y <- problem.f(problem.x);

      if(all(is.finite(problem.y))) {
        if(all(problem.y > 0)) {
          if(length(unique(problem.y)) >= length(problem.y)) {
            range <- range(problem.y);
            if(range[2] > range[1]) {
              if( ((range[2] - range[1]) / max(abs(range))) > 0.1) {
                break;
              }
            }
          }
        }
      }
    }

    for(i in 1:50) {
      repeat {
        x.sd <- 2^runif(n=1, min=-6, max=0.5);
        noisy.x <- rnorm(n=length(problem.x), mean=problem.x, sd=x.sd);
        if(all(noisy.x > 0)) {
          if(all(is.finite(problem.f(noisy.x)))) {
            break;
          }
        }
      }
      noisy.x <- force(noisy.x);
      repeat {
        y.sd <- 2^runif(n=1, min=-8, max=0);
        noisy.y <- rnorm(n=length(problem.y), mean=problem.y, sd=y.sd);
        if(all(is.finite(noisy.y))) {
          if(all(noisy.y > 0)) {
            break;
          }
        }
      }
      noisy.y        <- force(noisy.y);
      problem        <- NULL;
      problem$metric <- regressoR.quality::RegressionQualityMetric.default(noisy.x, noisy.y);
      problem$metric <- force(problem$metric);
      problem$par    <- regressoR.functional.models::FunctionalModel.par.estimate(model,
                                                                                  x=problem$metric@x,
                                                                                  y=problem$metric@y);
      problem$par    <- force(problem$par);
      problem        <- force(problem);

      if(canSolve(problem)) {
        col <- rgb(runif(1),runif(1),runif(1));
        #points(noisy.x, noisy.y, col=col, lwd=0.2);
        curve(problem.f, 0.1*x.min, x.max, col=col, add=TRUE, lwd=2);

        cat(": par=c(", paste(problem.par, sep="", collapse=", "),
            "), x.sd=", x.sd, ", y.sd=", y.sd,
            ", est=c(", paste(problem$par, sep="", collapse=", "), ")\n", sep="", collapse="");
        return(problem);
      }
    }
  }
}

# create 'number' problems whose size is between 'minDataSize' and 'maxDataSize', both bounds are inclusive
makeProblems <- function(number) {
  plot(c(0, x.max), c(0, 6), type="n", xlab="", ylab="");
  return(lapply(X=as.integer(minDataSize+((maxDataSize-minDataSize)*(0:(number-1))/(number-1))),
                FUN=makeProblem));
}

# create the buffer to receive results
results <- new.env();

# reset and clear the results buffer
clearResults <- function() {
  for(method in methods.names) {
    assign(x=method, value=NULL, pos=results);
    assign(x=paste0(method,"F"), value=0L, pos=results);
  }
}

# apply the method at the given index to the specified problem and store the results.
# this may be used for benchmarking, as it has a low overhead
applyMethod <- function(method.index, problems) {
  output <- vapply(X=problems,
                  FUN=function(problem) {
                    result <- methods.calls[[method.index]](metric=problem$metric,
                                                            model=model, par=problem$par);
                    if(is.null(result)) { return(+Inf); }
                    return(result@quality);
                  },
                  FUN.VALUE = +Inf);
  if(all(is.finite(output))) {
    assign(x=methods.names[[method.index]],
           value=c(get(x=methods.names[[method.index]], pos=results), output),
           pos=results);
  } else {
    name <- paste0(methods.names[[method.index]], "F");
    assign(x=name, value=(as.integer(sum(!(is.finite(output)))) + get(x=name, pos=results)), pos=results);
  }
}

# make the benchmark calls
makeBenchmarkCalls <- function(problems) {
  problems <- force(problems);
  lapply(X=1:length(methods.names),
         FUN=function(i) {
           i <- force(i);
           result <- bquote(applyMethod(i, problems));
           result <- force(result);
           result[[2]] <- i;
           result[[3]] <- problems;
           result <- force(result);
           return(result);
         });
}

# run the benchmarks: returns a list of time measures and quality matrix PER method
runBechmarks <- function(problems) {
  clearResults();
  calls        <- makeBenchmarkCalls(problems);
  names(calls) <- methods.names;
  micro        <- microbenchmark::microbenchmark(list = calls, times=numRuns);

  times        <- lapply(X=methods.names,
                         FUN=function(name) {
                           t <- micro$time[micro$expr == name];
                           t <- force(t);
                         });
  qualities    <- lapply(X=methods.names,
                         FUN=function(name) {
                           vector <- get(x=name, pos=results);
                           vector <- force(vector);
                           t      <- matrix(vector, nrow=length(problems));
                           t      <- force(t);
                           return(t);
                         });
  times     <- force(times);
  qualities <- force(qualities);
  retval    <- list(times=times, qualities=qualities);
  retval    <- force(retval);
  clearResults();
  return(retval);
}

# evaluate the results
evaluateResults <- function(times, qualities) {
  n            <- length(methods.names);

  times.all    <- c(unlist(times));
  times.orders <- rank(times.all);
  times.min    <- max(1e-20, min(abs(times.all)));

  start        <- 0;
  ranks.time   <- rep(0L, n);
  means.time   <- vapply(X=times, FUN=function(vec) mean(vec / times.min), FUN.VALUE=+Inf);
  for(index in 1:n) {
    end   <- start + length(times[[index]]);
    start <- start + 1L;
    ranks.time[index] <- mean(times.orders[start:end]);
    start <- end;
  }

  rows <- dim(qualities[[1]])[1];
  ranks.quality <- rep(0L, n);
  means.quality <- rep(0L, n);
  for(i in 1:rows) {
    qualities.all   <- c(unlist(lapply(X=1:n, FUN=function(j) qualities[[j]][i,])));
    qualities.order <- rank(qualities.all);
    qualities.min   <- max(1e-20, min(abs(qualities.all)));
    qualities.all   <- (abs(qualities.all - qualities.min) / qualities.min);
    qualities.all   <- force(qualities.all);

    start <- 0L;
    for(index in 1:n) {
      end   <- start + dim(qualities[[index]])[2];
      start <- start + 1L;
      ranks.quality[index] <- ranks.quality[index] + mean(qualities.order[start:end]);
      means.quality[index] <- means.quality[index] + mean(qualities.all[start:end]);
      start <- end;
    }
  }

  failures <- vapply(X=methods.names, FUN=function(n) get(x=paste0(n, "F"), pos=results), FUN.VALUE=-1L);

  printer <- t(matrix(c(
    "        methods:", unlist(methods.names),
    "        quality mean:", means.quality,
    "        quality rank:", ranks.quality,
    "        time mean:", means.time,
    "        time rank:", ranks.time,
    "        failures:", failures), ncol=6));
  write.matrix(printer);

  cat("by quality-mean: ", paste(methods.names[order(means.quality)], collapse=", "), "\n", sep="");
  cat("by quality-rank: ", paste(methods.names[order(ranks.quality)], collapse=", "), "\n", sep="");
  cat("by    time-rank: ", paste(methods.names[order(ranks.time)], collapse=", "), "\n", sep="");
  cat("by    time-mean: ", paste(methods.names[order(means.time)], collapse=", "), "\n", sep="");
}


cat("\n\n######################################################################\n",
    "# Tackling Problems of Range ", minDataSize, "...", maxDataSize, "\n",
    "######################################################################\n",
    sep="");
problems = makeProblems(numProblems);
cat(length(problems), " problems have been created with mean size ",
    mean(vapply(X=problems, FUN=function(x) length(x$metric@x), FUN.VALUE = NaN)),
     "\n...now beginning to benchmark.\n",
    sep="");

values <- runBechmarks(problems);
evaluateResults(values$times, values$qualities);
cat("##########################################################################\n");
