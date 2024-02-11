library(frequtils)

data("transactions")

x <- transactions$suspicion.score[transactions$transaction.type=="not.fraud"]
hist(x)

#mu.fix <- 1.5 # big diff
#mu.fix <- 2.0 # small.diff
#mu.fix <- 2.4 # big diff
#mu.fix <- 3 # big diff
mean(x)

one.sample.bs.hyp.for.mean(x,
                     mu.null = 1.6,
                  confidence = 0.95,
                  #alternative = "greater")
                  #alternative = "less")
                  alternative = "two.sided")


data("shedder")
x2 <- shedder[shedder$location == "LU", 3:5]
x2 <- rowMeans(log(x2))
hist(x2)


shedder$location == "LU"
shedder$location

one.sample.bs.hyp.for.mean(x2,
                           mu.null     = -4.7,
                           xlim.marg   = 0.01,
                           confidence  = 0.95,
                           #alternative = "greater")
                           alternative = "less")
                           #alternative = "two.sided")
