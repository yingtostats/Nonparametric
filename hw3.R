#1.11
#Initialization
setwd("C:/Users/user/Desktop/Non-parametric/Datasets/1")
data <- read.table("beenswax.txt",header = TRUE)
dat.hc <- data[,2]
dat.mp <- data[,1]

#ecdf
par(mfrow = c(1,2))
plot(ecdf(dat.mp), main = "Empirical CDF of MeltingPoint Data",xlab = "Melting Point", ylab = "Empirical CDF")
plot(ecdf(dat.hc), main = "Empirical CDF of Hydrocarbon Data",xlab = "Hydrocarbon", ylab = "Empirical CDF")
#histogram
hist(dat.mp, main = "Histogram of MeltingPoint Data",xlab = "Melting Point", ylab = "Frequency")
hist(dat.hc, main = "Histogram of Hydrocarbon Data",xlab = "Hydrocarbon", ylab = "Frequency")
#qqplot
par(mfrow = c(1,2))
qqnorm(dat.mp)
qqline(dat.mp,col = 6, lwd = 1)
qqnorm(dat.hc)
qqline(dat.hc,col = 12, lwd = 1)
#quantile
dat.quantile <- rbind(quantile(dat.mp,c(0.9,0.75,0.5,0.25,0.1)),quantile(dat.hc,c(0.9,0.75,0.5,0.25,0.1)))
rownames(dat.quantile) <- c("MeltingPoint","Hydrocarbon")
dat.quantile
#shapiro-wilk test
data.name(dat.mp)
shapiro.test(dat.mp)
#we conduct the Shapiro-Wilk test to check the normality assumption. The null hypothesis is the data is from Gaussian distribution. The p-value is 0.2579 > 0.05, we can conclude that we accept the $H_{0}$: the data is from Gaussian Distribution under 95% confidence level
shapiro.test(dat.hc)
#we conduct the Shapiro-Wilk test to check the normality assumption. The null hypothesis is the data is from Gaussian distribution. The p-value is 0.3123 > 0.05, we can conclude that we accept the $H_{0}$: the data is from Gaussian Distribution under 95% confidence level

#1.12
#initialization
dat.expr <- readline(prompt = " ")
1 174 263 105 199 141 108 141 2 224 213 103 143 168 341 184 3 260 231 145 113 78 159 125 4 255 291 103 225 164 135 227 5 165 168 144 176 127 239 194 6 237 121 94 144 114 136 155 7 191 137 35 87 96 140 121 8 100 102 133 120 222 134 129 9 115 89 83 100 165 185 79 10 189 433 237 173 168 188 317
dat.expr <- strsplit(dat.expr,split = " ")[[1]]
dat.expr <- matrix(as.integer(dat.expr), ncol = 8, byrow = TRUE)
dat.expr <- as.data.frame(dat.expr)
colnames(dat.expr) <- c("Patient", "Nodrug", "Placebo", "Papv", "Morp", "Amino", "Pento", "Tripel")
dat.expr$Patient <- c("BG","JF","BS","SI","BW","TS","GM","SS","MU","OS")
(dat.expr)

#survival plots
surv.dat <- function(data)  {
  data.sort <- sort(data)
  rank <- rank(data.sort)
  n <- length(data)
  cdf <- rank/n
  return(c(1 - cdf,data.sort))
  }
surv <- apply(dat.expr[,2:8],2,surv.dat)

plot(0,0, type = "s", main = "Empirical Survival Function", xlab = "Itch Time", ylab = "empirical Survival Function", xlim = c(75,450), ylim = c(0,1))
for (i in 1:7) {
    lines(surv[11:20,i],surv[1:10,i], type = "s", col = colors(distinct = TRUE)[(i - 1)*10],lty = i*5,lwd = 2)
  }
legend(350,0.8,legend = colnames(surv), col = colors(distinct = TRUE)[(1:7 - 1)*10] , lty = (1:7)*5, lwd = 2)  


