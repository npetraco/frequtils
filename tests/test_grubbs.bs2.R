library(frequtils)

# x <- c(266, 273, 273, 274, 275, 276, 276, 276, 277, 277,
#        278, 278, 278, 279, 279, 280, 281, 282, 282, 284)
x <- c(200, 273, 273, 274, 275, 276, 276, 276, 277, 277,
       278, 278, 278, 279, 279, 280, 281, 282, 282, 284, 320)


x.md <- median(x)
x.s  <- mad(x)
n    <- length(x)
grubbs.parametric.bs.test(x,
          rdist.func      = rlst,
          rdist.args.list = list(n = n, df=n-1, mu = x.md, sigma = x.s),
          num.B           = 2000)
