d = 100
n = 20000
set.seed(1247)
x <- matrix(NA,nrow = n, ncol = d)
y <- vector()
x <- matrix(rnorm(n*d,0,1),ncol = d)
for (i in 1:n) {
  y[i] <- rnorm(1,sum(x[i,]),sqrt(10))
}


blb.ci <- function(alpha=0.05,x,y,s=10,bsize=0.7,r=50,lambda = 0.00001) {
  n = length(y)
  b = round(n^{bsize})
  ci.true <- 0.1
  ci <- matrix(NA,nrow = s,ncol = d)
  ci.length <- function (m) {
    ci.length <-quantile(m,1-alpha/2) - quantile(m,alpha/2)
    return(ci.length[[1]])
  }
  for (i in 1:s)  {
    bsample <- sample(1:n,b,replace = FALSE)
    beta <- matrix(NA,ncol = d,nrow = r)
    for (j in 1:r) {
      nsample <- sample(bsample,n,replace = TRUE)
      beta[j,] <- solve(t(x[nsample,])%*%x[nsample,] + lambda*diag(d))%*%t(x[nsample,])%*%y[nsample]
    }
    ci[i,] <- apply(beta,2,ci.length)
  }
  ci.all <- colMeans(ci)
  relative.error <- abs((mean(ci.all) - ci.true))/ci.true
  return(relative.error)
}

blb.ci(alpha = 0.05,x,y)