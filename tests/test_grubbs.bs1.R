library(outliers)
library(extraDistr)

grubbs.bs <- function(xdat, rdist.func, rdist.args.list, confidence.level=0.95, num.B=2000) {
  
  G.bs.samp <- array(NA, c(num.B))
  for(i in 1:num.B) {
    
    # Get a parametric BS Null data sample:
    x.bs <- do.call(rdist.func, rdist.args.list)
    #print(x.bs)
    
    # Compute Grubbs statistic on the BS data sample and store
    mu.hat.bs    <- mean(x.bs)
    sdx.hat.bs   <- sd(x.bs)
    G.bs         <- max(abs(x.bs - mu.hat.bs)) / sdx.hat.bs
    G.bs.samp[i] <- G.bs
  }
  
  hist(G.bs.samp)
  
  # Overlay "Critical" BS G-value at level of confidence:
  Gc.bs <- quantile(G.bs.samp, prob=c(confidence.level))
  points(c(Gc.bs), c(0), pch=17)
  
  #Overlay Sample G-value:
  G.samp <- max(abs(x-mean(x)))/sd(x)
  points(c(G.samp), c(0), col="red", pch=16)
  
  # Bootstrap p-value: 
  asl  <- sum(G.bs.samp >= G.samp)/num.B
  asl2 <- (1 + sum(G.bs.samp >= G.samp) )/(num.B+1) # Unbiased form
  print(paste0("Achieved Significance Level: ", asl*100, "%"))
  print(paste0("Achieved Confidence Level:   ", (1-asl)*100, "%"))
  print(paste0("Reject H0 in favor of Ha?    ", asl < (1-confidence.level)))
  #print(paste0("ASL2: ", asl2))
  
}


x <- c(266, 273, 273, 274, 275, 276, 276, 276, 277, 277, 
       278, 278, 278, 279, 279, 280, 281, 282, 282, 284)

x.md <- median(x)
x.s  <- mad(x)
n   <- length(x)

grubbs.bs(x, 
          rdist.func      = rlst, 
          rdist.args.list = list(n = n, df=n-1, mu = x.md, sigma = x.s), 
          num.B           = 2000)

