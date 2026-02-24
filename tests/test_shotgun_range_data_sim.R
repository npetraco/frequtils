library(bayesutils)

data(sgr)
steve <- as.matrix(sgr[1:6,1:5])  # Stevens data
remin <- as.matrix(sgr[7:12,1:5]) # Remington data

round(colMeans(steve), 1)
round(apply(steve, MARGIN = 2, FUN = sd),1)

round(apply(steve, MARGIN = 2, FUN = median),1)
round(apply(steve, MARGIN = 2, FUN = mad),1)

# Extra options to set for Stan:
options(mc.cores = 4)
rstan_options(auto_write = TRUE)

# Load a Stan model:
stan.code <- paste(readLines("tests/test_shotgun_range_data_sim.stan"),collapse='\n')

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose=T)

# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)

# Data:
X   <- remin
dat <- list(
  "n"            = nrow(X),
  "p"            = ncol(X),
  "X"            = X,
  "mu_hyp"       = c(3,   6.5, 8,  12.5, 13.5), # mean for each dist hyperparam. Should be positive because they are physical areas
  "sigma_hyp"    = c(0.2, 0.6, 1.8, 2.0, 4.0),  # sd for each dist hyperparam
  "sd_sigma_hyp" = 0.1,                         # sd for the sd's of each dist hyperparam.
  "nu_t_fix"     = nrow(X)-1                   # df. for liklihood
)

# Run the model:
fit <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4 )

# Extract:
params.mat <- extract.params(fit, as.matrixQ = T)
#mcmc_areas(params.mat, regex_pars =c("mu", "sigma"), prob = 0.95)
mcmc_areas(params.mat, regex_pars =c("mu"), prob = 0.95)
mcmc_areas(params.mat, regex_pars =c("sigma"), prob = 0.95)
#mcmc_intervals(params.mat, regex_pars =c("mu"), prob = 0.95)
#mcmc_intervals(params.mat, regex_pars =c("sigma"), prob = 0.95)

# Sim data with normal:
library(truncnorm)
x.1 <- rtruncnorm(nrow(params.mat), a = 0, mean = params.mat$mu.1., sd = params.mat$sigma.1.)
x.2 <- rtruncnorm(nrow(params.mat), a = 0, mean = params.mat$mu.2., sd = params.mat$sigma.2.)
x.3 <- rtruncnorm(nrow(params.mat), a = 0, mean = params.mat$mu.3., sd = params.mat$sigma.3.)
x.4 <- rtruncnorm(nrow(params.mat), a = 0, mean = params.mat$mu.4., sd = params.mat$sigma.4.)
x.5 <- rtruncnorm(nrow(params.mat), a = 0, mean = params.mat$mu.5., sd = params.mat$sigma.5.)
hist(x.1)
hist(x.2)
hist(x.3)
hist(x.4)
hist(x.5)

# Sim data with student-T (actual posterior predictive distribution):
library(crch)
x.1t <- rtt(nrow(params.mat), location = params.mat$mu.1., scale = params.mat$sigma.1., df=nrow(X)-1, left = 0)
x.2t <- rtt(nrow(params.mat), location = params.mat$mu.2., scale = params.mat$sigma.2., df=nrow(X)-1, left = 0)
x.3t <- rtt(nrow(params.mat), location = params.mat$mu.3., scale = params.mat$sigma.3., df=nrow(X)-1, left = 0)
x.4t <- rtt(nrow(params.mat), location = params.mat$mu.4., scale = params.mat$sigma.4., df=nrow(X)-1, left = 0)
x.5t <- rtt(nrow(params.mat), location = params.mat$mu.5., scale = params.mat$sigma.5., df=nrow(X)-1, left = 0)
hist(x.1t)
hist(x.2t)
hist(x.3t)
hist(x.4t)
hist(x.5t)

# Grab a sample out of the posterior predictive simulations:
num.samps <- 30
Xsim   <- cbind(x.1, x.2, x.3, x.4, x.5)
Xtsim  <- cbind(x.1t, x.2t, x.3t, x.4t, x.5t)
Xsamp  <- sapply(1:5, function(xx){round(sample(Xsim[,xx], size = num.samps), 2)})
Xtsamp <- sapply(1:5, function(xx){round(sample(Xtsim[,xx], size = num.samps), 2)})

#Xsamp.steve  <- Xsamp
#Xtsamp.steve <- Xtsamp

Xsamp.remington  <- Xsamp
Xtsamp.remington <- Xtsamp



gun.lbl <- gl(n = 2, k = 2*num.samps, labels = c("Stevens", "Remington"))
Xsamp.all  <- rbind(Xsamp.steve, Xsamp.remington)
Xtsamp.all <- rbind(Xtsamp.steve, Xtsamp.remington)

sgr.nsim <- data.frame(Xsamp.all, gun.lbl)
sgr.tsim <- data.frame(Xtsamp.all, gun.lbl)
colnames(sgr.nsim)  <- c("X10.ft", "X20.ft", "X30.ft", "X40.ft", "X50.ft", "gun.lbl")
colnames(sgr.tsim) <- c("X10.ft", "X20.ft", "X30.ft", "X40.ft", "X50.ft", "gun.lbl")

save(sgr.nsim, file="data/sgr.nsim.RData")
save(sgr.tsim, file="data/sgr.tsim.RData")

# Check
data(sgr.nsim)
hist(sgr.nsim[1:30,1])
hist(sgr.nsim[31:60,1])

hist(sgr.nsim[1:30,2])
hist(sgr.nsim[31:60,2])

hist(sgr.nsim[1:30,3])
hist(sgr.nsim[31:60,3])

hist(sgr.nsim[1:30,4])
hist(sgr.nsim[31:60,4])

hist(sgr.nsim[1:30,5])
hist(sgr.nsim[31:60,5])

data(sgr.tsim)
hist(sgr.tsim[1:30,1])
hist(sgr.tsim[31:60,1])

hist(sgr.tsim[1:30,2])
hist(sgr.tsim[31:60,2])

hist(sgr.tsim[1:30,3])
hist(sgr.tsim[31:60,3])

hist(sgr.tsim[1:30,4])
hist(sgr.tsim[31:60,4])

hist(sgr.tsim[1:30,5])
hist(sgr.tsim[31:60,5])
