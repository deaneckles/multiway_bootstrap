source("multiway_boot.R")

# In this example, we have two grouping factors, subjects and stimuli

# Generate simulated data
N <- 1e4
N.subjects <- 100
N.stimuli <- 20

subjects.re <- rnorm(100)
subject <- rep(1:100, each = 100)
stimuli.re <- rlnorm(N.stimuli)
stimulus <- sample.int(N.stimuli, N, replace = TRUE)

# x has subject, stimulus, and subject-stimulus (error) components
x <- subjects.re[subject] + stimuli.re[stimulus] + rnorm(N)

# point estimate of mean
mean(x)

library(Hmisc) # for wtd.mean

alpha <- .05
q <- c(alpha / 2, 1 - alpha / 2)

# two-way bootstrap
mb.2 <- multiway.boot(
  statistic = wtd.mean,
  R = 500,
  groups = cbind(subject, stimulus),
  .progress = 'text', # can use plyr progress indicators
  x = x
  )
mb.2 <- unlist(mb.2)
sd(mb.2) # bootstrap estimate of standard error of the mean
qnorm(q, mean(x), sd(mb.2)) # normal approximation
quantile(mb.2, q) # percentile bootstrap CI

# setup multicore for demonstration of parallel execution
library(foreach)
library(doMC)

registerDoMC()

# compare with (anti-conservative) one-way version
mb.1 <- multiway.boot(
  statistic = wtd.mean,
  R = 500,
  groups = cbind(subject), # only cluster/block on subject
  .parallel = TRUE, # can use plyr parallel tools
  x = x
  )
mb.1 <- unlist(mb.1)
sd(mb.1)
qnorm(q, mean(x), sd(mb.1))
quantile(mb.1, q)

# compare with CI based on the normal, ignoring both grouping factors
t.test(x, conf.level = 1 - alpha)$conf.int
