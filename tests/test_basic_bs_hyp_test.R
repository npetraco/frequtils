library(frequtils)

data("transactions")

x <- transactions$suspicion.score[transactions$transaction.type=="not.fraud"]
hist(x)

#mu.fix <- 1.5 # big diff
mu.fix <- 2.0 # small.diff
#mu.fix <- 2.4 # big diff
#mu.fix <- 3 # big diff
mu.X   <- mean(x)
mu.X
sd(x)

one.sample.bs.hyp(x, stat.func = mean, val.null = mu.fix,
                  confidence = 0.95,
                  #alternative = "greater")
                  #alternative = "less")
                  alternative = "two.sided")


one.sample.bs.hyp(x, stat.func = mean, val.null = mu.fix,
                  confidence = 0.95,
                  #alternative = "greater")
                  #alternative = "less")
                  alternative = "two.sided")
