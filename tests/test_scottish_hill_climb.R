library(rgl)
library(lattice)
library(ggplot2)

Races <-latticeRaces <- read.table("http://stat4ds.rwth-aachen.de/data/ScotsRaces.dat", header=TRUE)

# Plot check
pairs(~timeW + distance + climb, data=Races) # scatterplot matrix (Figure 6.2)
plot3d(Races$distance, Races$climb, Races$timeW)

fit.dc <- lm(timeW ~ distance + climb, data=Races)
summary(fit.dc)

fit.dc2 <- lm(timeW ~ -1 + distance + climb, data=Races)
summary(fit.dc2)

fit.int1 <- lm(timeW ~ distance + climb + distance:climb, data=Races)
summary(fit.int1)

fit.int2 <- lm(timeW ~ -1 + distance + climb + distance:climb, data=Races)
summary(fit.int2) # -1 constrains intercept = 0

# Interaction plots
coplot(timeW~distance|climb, data = Races, panel = panel.smooth)
#interaction.plot(Races$distance, cut(Races$climb,breaks = 2), Races$timeW)

cut(Races$climb,breaks = 4)

dat2 <- data.frame(Races,cut(Races$climb,breaks = 4))
colnames(dat2) <- c(colnames(Races),"grp")
xyplot(timeW ~ distance | grp, data = dat2, type = "p")    # lattice
qplot(x = distance, y = timeW, data = dat2, color = grp) + # ggplot
  geom_smooth(method = "lm") 


# More 3D Plot check
f.dc <- function(xx, yy){
  zz <- coef(fit.dc)[1] + coef(fit.dc)[2]*xx + coef(fit.dc)[3]*yy
  return(zz)
}
f.int <- function(xx, yy){
  zz <- coef(fit.int1)[1] + coef(fit.int1)[2]*xx + coef(fit.int1)[3]*yy + coef(fit.int1)[4]*xx*yy
  return(zz)
}
f.dc2 <- function(xx, yy){
  zz <-  coef(fit.dc2)[1]*xx + coef(fit.dc2)[2]*yy
  return(zz)
}
f.int2 <- function(xx, yy){
  zz <- coef(fit.int2)[1]*xx + coef(fit.int2)[2]*yy + coef(fit.int2)[3]*xx*yy
  return(zz)
}


persp3d(f.int2,
        xlim = c(min(Races$distance), max(Races$distance)),
        ylim = c(min(Races$climb),    max(Races$climb)),
        zlim = c(min(Races$timeW),    max(Races$timeW)),
        n = 100, alpha = 0.5, col="red", aspect=c(1,1,1))
points3d(Races$distance, Races$climb, Races$timeW, radius = 0.1)
