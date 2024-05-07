x <- c(100,120,140,160,180,200,220,240,260,280,300,320,340,360,380,400)
y <- c(88.8,108.7,129.8,146.2,161.6,179.9,202.4,224.5,245.1,257.7,277.0,298.1,318.8,334.6,355.2,377.0)

plot(x,y)
fit1 <- lm(y ~ x)   # Basic linear regression
plot(x, y, xlab="concentration", ylab="absorbance")
abline(fit1)        # Show best fit line
summary(fit1)       # Shows regression coefs and some diagnostics
anova(fit1)         # Regression ANOVA table. "Total" row is absent

# Residuals analysis:
eps1   <- residuals(fit1)
preds1 <- predict(fit1)
plot(preds1, eps1, main="Hetroscedastic?")

qqnorm(eps1, main="Normal?")
qqline(eps1)

y0   <- 200
b0   <- -6.67
b1   <- 0.953
Mres <- 5.86
SXX  <- sum((x - mean(x))^2)
n    <- length(x)
x0 <- (y0 - b0)/b1
conf <- 0.95
alp <- 1 - conf
tc <- qt(1-alp/2, n-2)
tc

x0.hi <- x0 + tc/b1 * sqrt(Mres*(1 + 1/n + (x0-mean(x))/SXX))
x0.lo <- x0 - tc/b1 * sqrt(Mres*(1 + 1/n + (x0-mean(x))/SXX))

library(EnvStats)
calibrate.list <- calibrate(y ~ x, data=data.frame(x,y), max.order=1)
summary(calibrate.list)

inversePredictCalibrate(calibrate.list, obs.y = y0, 
                        intervals = TRUE, coverage = 0.95, individual = TRUE) 
x0
c(x0.lo,x0.hi)
fit1$model$x
