library(plyr)

r.double.or.nothing <- function(n) {
  2 * rbinom(n, 1, .5)
}

online.multiway.boot <- function(statistic, R, N,
                               groups = as.matrix(1:N),
                               verbose = FALSE,
                               RNG = r.double.or.nothing,
                               ...) {
  groups.num <- apply(groups, 2, function(x) as.numeric(as.factor(x)))
  N.groups <- apply(groups, 2, function(x) length(unique(x)))
  N.groupingFactors <- ncol(groups)
  
  llply(1:R, function(i) {
    W <- matrix(nrow = 0, ncol = N)
    for (j in 1:N.groupingFactors) {
      W <- rbind(W, RNG(N.groups[j])[groups.num[,j]])
    }
    w <- apply(W, 2, prod)
    if (verbose) cat(i, " ")
    statistic(..., weights = w)
  })
}