library(rgl)

X <- cbind(
  c(41.9, 43.4, 43.9, 44.5, 47.3, 47.5, 47.9, 50.2, 52.8, 53.2, 56.7, 57,   63.5, 65.3, 71.1, 77,   77.8),
  c(29.1, 29.3, 29.5, 29.7, 29.9, 30.3, 30.5, 30.7, 30.8, 30.9, 31.5, 31.7, 31.9, 32,   32.1, 32.5, 32.9),
  c(251.3, 251.3, 248.3, 267.5, 273, 276.5, 270.3, 274.9, 285, 290, 297, 302.5, 304.5, 309.3, 321.7, 330.7, 349)
)

colnames(X) <- c("addativeA","addativeB","response")
X <- data.frame(X)

fit <- lm(response ~ addativeA + addativeB, data=X)
summary(fit)

fit2 <- lm(response ~ addativeA + addativeB + addativeA:addativeB, data=X)
summary(fit2)

AIC(fit)
AIC(fit2)


# Plot check
plot3d(X$addativeA, X$addativeB, X$response, radius = 0.1, aspect = c(1,1,1))
planes3d(a = coef(fit)[2], b = coef(fit)[3], c = -1, d = coef(fit)[1], col = "blue", alpha = 0.5)

# Another way to plot the plane of the fit:
f <- function(xx,yy){
  zz <- coef(fit)[1] + coef(fit)[2]*xx + coef(fit)[3]*yy
  return(zz)
}

persp3d(f,
        xlim = c(min(X$addativeA), max(X$addativeA)),
        ylim = c(min(X$addativeB), max(X$addativeB)),
        zlim = c(min(X$response), max(X$response)),
        n = 30, alpha = 0.5, col="red")
points3d(X$addativeA, X$addativeB, X$response, radius = 0.1)


# Fit surface with interaction:
f2 <- function(xx,yy){
  zz <- coef(fit2)[1] + coef(fit)[2]*xx + coef(fit)[3]*yy + coef(fit)[4]*xx*yy
  return(zz)
}

persp3d(f2,
        xlim = c(min(X$addativeA), max(X$addativeA)),
        ylim = c(min(X$addativeB), max(X$addativeB)),
        zlim = c(min(X$response), max(X$response)),
        n = 30, alpha = 0.5, col="red")
points3d(X$addativeA, X$addativeB, X$response, radius = 0.1)
