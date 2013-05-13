source("multiway_boot.R")

# In this example, we have two grouping factors, subjects and stimuli
N <- 1e4
N.subjects <- 100
subjects.re <- rnorm(100)
subject <- rep(1:100, each = 100)
N.stimuli <- 20
stimuli.re <- rlnorm(N.stimuli)
stimulus <- sample.int(N.stimuli, N, replace = TRUE)

x <- subjects.re[subject] + stimuli.re[stimulus] + rnorm(N)

library(Hmisc) # for wtd.mean


# two-way bootstrap
mb.2 <- multiway.boot(statistic = wtd.mean,
                      R = 500, N = N,
                      groups = cbind(subject, stimulus),
                      x = x)
mb.2 <- unlist(mb.2)
var(mb.2)
quantile(mb.2, c(.025, .975))

## setup multicore
library(foreach)
library(doMC)

registerDoMC()

# compare with (anti-conservative) one-way version
mb.1<- multiway.boot(statistic = wtd.mean,
                     R = 500, N = N,
                     groups = cbind(subject),
                     .progress = 'text',
                     .parallel = TRUE, ## requires foreach and doMC libraries
                     x = x)
mb.1 <- unlist(mb.1)
var(mb.1)
quantile(mb.1, c(.025, .975))

# compare with CI based on the normal, ignoring both grouping factors
t.test(x)$conf.int
