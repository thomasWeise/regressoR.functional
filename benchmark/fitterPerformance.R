# Compare the Performance of the Different Function Fitters

# This script is used to compare the performance of different functional fitters
# on simple example problems of different scale. Its results will help to
# decide about the default solvers to apply.

library(regressoR.functional);
library(microbenchmark);
library(MASS);

set.seed(2500234L);
minDataSize <- 10L;
maxDataSize <- 1000L;
steps       <- 4L;
model       <- regressoR.functional.models::FunctionalModel.gompertz.1();

cat("### Compare the Performance of the Different Function Fitters ###\n");

methods.names <- c("nlslm", "minqa", "cmaes", "de", "dfoptim", "nls", "lbfgsb");
cat("      methods: ", paste(methods.names, collapse=", "), "\n", sep="");
cat("min data size: ", minDataSize, "\n", sep="");
cat("max data size: ", maxDataSize, "\n", sep="");
cat("        steps: ", steps, "\n", sep="");
cat("        model: ", toString(deparse(body(model@f))), " with ", model@paramCount, " parameters\n", sep="");

# make the set of methods
methods.calls <- lapply(X=methods.names,
                        FUN=function(t) {
                          result <- get(paste(c("FunctionalModel.fit.", t), collapse=""));
                          result <- force(result);
                          return(result);
                        });

# check whether there is any method that cannot solve a given problem (twice)
canSolve <- function(metric) all(unlist(lapply(X=1:2, FUN=function(i)
                             vapply(X=methods.calls,
                                    FUN=function(method) (!(is.null(method(metric, model)))),
                                    FUN.VALUE = FALSE))));

# create a solveable problem of the given size
makeProblem <- function(dataSize) {
  repeat {

    repeat {
      repeat {
        problem.par <- runif(n=model@paramCount, min=-3, max=3);
        if(regressoR.functional.models::FunctionalModel.par.check(model, problem.par)) {
          break;
        }
      }

      repeat {
        problem.x      <- runif(n=dataSize, min=-10, max=10);
        if(length(unique(problem.x)) >= dataSize) {
          break;
        }
      }

      problem.f      <- function(x) model@f(x, problem.par);
      problem.y      <- problem.f(problem.x);

      if(length(unique(problem.y)) >= length(problem.y)) {
        range <- range(problem.y);
        if(range[2] > range[1]) {
          if( ((range[2] - range[1]) / max(abs(range))) > 0.1) {
            break;
          }
        }
      }
    }

    noisy.x        <- rnorm(n=length(problem.x), mean=problem.x, sd=0.1);
    noisy.x        <- force(noisy.x);
    noisy.y        <- rnorm(n=length(problem.y), mean=problem.y, sd=0.1);
    noisy.y        <- force(noisy.y);
    problem.metric <- regressoR.quality::RegressionQualityMetric.default(noisy.x, noisy.y);
    problem.metric <- force(problem.metric);
    if(canSolve(problem.metric)) { return(problem.metric); }
  }
}

# create 'number' problems whose size is between 'minDataSize' and 'maxDataSize', both bounds are inclusive
makeProblems <- function(number, minDataSize, maxDataSize) {
  return(lapply(X=1:number,
                FUN=function(x) {
                  size <- -1L;
                  while((size < minDataSize) || (size > maxDataSize)) {
                   size <- as.integer(round(runif(n=1, min=minDataSize, max=(maxDataSize+1L))));
                  }
                  size   <- force(size);
                  result <- makeProblem(size);
                  result <- force(result);
                  return(result);
                }));
}

# create the buffer to receive results
results <- new.env();

# reset and clear the results buffer
clearResults <- function() {
  for(method in methods.names) {
    assign(x=method, value=NULL, pos=results);
  }
}

# apply the method at the given index to the specified problem and store the results.
# this may be used for benchmarking, as it has a low overhead
applyMethod <- function(method.index, problems) {
  output <- vapply(X=problems,
                  FUN=function(problem) {
                    result <- methods.calls[[method.index]](problem, model);
                    if(is.null(result)) { return(+Inf); }
                    return(result@quality);
                  },
                  FUN.VALUE = +Inf);
  assign(x=methods.names[[method.index]],
         value=c(get(x=methods.names[[method.index]], pos=results), output),
         pos=results);
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
  micro        <- microbenchmark::microbenchmark(list = calls, times=2L+as.integer(round(2000L/length(problems))));

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
  retval    <-list(times=times, qualities=qualities);
  retval    <-force(retval);
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

  printer <- t(matrix(c(
    "        methods:", unlist(methods.names),
    "        quality mean:", means.quality,
    "        quality rank:", ranks.quality,
    "        time mean:", means.time,
    "        time rank:", ranks.time), ncol=5));
  write.matrix(printer);

  cat("by quality-mean: ", paste(methods.names[order(means.quality)], collapse=", "), "\n", sep="");
  cat("by quality-rank: ", paste(methods.names[order(ranks.quality)], collapse=", "), "\n", sep="");
  cat("by    time-rank: ", paste(methods.names[order(ranks.time)], collapse=", "), "\n", sep="");
  cat("by    time-mean: ", paste(methods.names[order(means.time)], collapse=", "), "\n", sep="");
}



# create a problem
maxDimension <- minDataSize - 1L;
for(i in 1:steps) {
  minDimension <- as.integer(max(minDataSize, max(1L, maxDimension + 1L)));
  maxDimension <- as.integer(max(minDataSize+1L, min(maxDataSize, as.integer(round(
                  minDataSize + ((i * (maxDataSize - minDataSize)) / steps))))));

  cat("\n\n##########################################################################\n",
      "# Now Tackling Problems of Range ", minDimension, "...", maxDimension, "\n",
      "##########################################################################\n",
      sep="");
  problems = makeProblems(60, minDimension, maxDimension);
  cat(length(problems), " problems have been created with mean size ",
      mean(vapply(X=problems, FUN=function(x) length(x@x), FUN.VALUE = NaN)),
       "\n...now beginning to benchmark.\n",
      sep="");

  values <- runBechmarks(problems);
  evaluateResults(values$times, values$qualities);
  cat("##########################################################################\n");
}
