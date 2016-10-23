rm(list = ls())
library(MASS)
data("geyser")
# Empirical CDF
waiting <- geyser[,1]
waiting.sort <- sort(waiting)
waiting.rank <- rank(waiting.sort)
N <- length(waiting)
waiting.cdf <- waiting.rank/N
plot(waiting.sort,waiting.cdf, type = "s", main = "Empirical CDF")
segments(waiting.sort[1:(N - 1)], waiting.cdf[1:(N - 1)], waiting.sort[2:N], waiting.cdf[1:(N - 1)])
alpha <- 0.5
band <- sqrt(1/(2*N))*log(2/alpha) 
lower.95 <- waiting.cdf - band
upper.95 <- waiting.cdf + band
lines(waiting.sort,lower.95, type = "s", lty = 2)
lines(waiting.sort,upper.95, type = "s", lty = 2)

# band & alpha
plot(1:10000*0.0001,band <- sqrt(1/(2*N))*log(2/(1:10000*0.0001)), type = "l", main = "Relationship btwn Band and alpha", ylab = "band", xlab = "alpha")

# Poisson 
Error.rate <- function(lambda,n = 10,C = 5) { 
    p = 0
    k = 0
    while (k <= C) {
      p  = p + (n*lambda)^(k)*exp(-n*lambda)/factorial(k)
      k = k + 1
    }
    return(1-p)
}

plot(0:4000*0.0005,Error.rate(lambda = 0:4000*0.0005,C = 5),type = "l")

  


