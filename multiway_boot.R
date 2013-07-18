library(plyr)
library(foreach)

r.double.or.nothing <- function(n) {
  2 * rbinom(n, 1, .5)
}

multiway.boot <- function(
  statistic, R,
  groups = as.matrix(1:N),
  verbose = FALSE,
  RNG = r.double.or.nothing,
  .parallel = FALSE,
  .progress = 'none',
  ...
  ) {
  groups <- apply(groups, 2, function(x) as.numeric(as.factor(x)))
  N.groups <- apply(groups, 2, function(x) max(x))
  N.groupingFactors <- ncol(groups)

  llply(
    1:R,
    function(r) {
      # Observation weights are products of weights for each factor
      w <- foreach(i = 1:N.groupingFactors, .combine = `*`) %do% {
        RNG(N.groups[i])[groups[, i]]
      }

      if (verbose) cat(i, " ")
      statistic(..., weights = w)
    },
    .parallel = .parallel,
    .progress = .progress
    )
}
