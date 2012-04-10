source("online_multiway_boot.R")

N <- 1e4
subject <- rep(1:100, each = 100)
stimuli <- letters
stimulus <- sample(stimuli, N, replace = TRUE) 

x <- rnorm(N)

library(Hmisc)

mb.1 <- online.multiway.boot(statistic = wtd.mean,
                           R = 500, N = N,
                           groups = cbind(subject, stimulus),
                           x = x)
mb.1 <- unlist(mb.1)
var(mb.1)
quantile(mb.1,
         c(.025, .975))
