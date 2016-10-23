#### Author: Ying Zhang
#### ID: 2013202023
#### Course: Non-parametric Statistics

rm(list = ls())

# A.5
getzero <- function(equation,lower,upper) {
    stopifnot(is.function(equation) || equation(lower)*equation(upper) < 0 )
    while ( upper - lower > 0.00000001) {
        mid <- (lower+upper) / 2
        f.mid <- equation(mid)
        if(f.mid == 0) return(mid)
        else if (f.mid * equation(upper) < 0 ) lower <- mid
        else (upper <- mid)
    }
    return((upper + lower) / 2)
}

equation <- function(x) return(2*x^3 - 4*x^2 + 3*x - 6)
getzero(equation,-10,10)

# A.7
enigma <- function(word) {
    stopifnot(is.character(word))
    word.split <- strsplit(word,split="")
    CCB.initial <- c(letters,LETTERS)
    CCB.transcode <- c(LETTERS[c(14:26,1:13)],letters[c(14:26,1:13)])
    word.new <- CCB.transcode[match(word.split[[1]],CCB.initial)]
    word.new <- paste(word.new,collapse = "")
    return(word.new)
}

enigma("People")

# A.9
find <- function(n,m) {
    num.initial <- 1:n
    num.list <- cbind(num.initial,num.initial)
    leng <- length(num.list[,1])
    while (leng > 2) {
        startpoint <- num.list[leng,2] + 1
        num.list <- num.list[num.list[,2] %% m != 0, ]
        leng <- length(num.list[,1])
        num.list[,2] <- startpoint:(startpoint + leng - 1)
    }
    startpoint <- num.list[leng,2] + 1
    num.list <- num.list[num.list[,2] %% m != 0, ]
    return(num.list[1][[1]])
}

find(13,3)

# A.11
basket <- read.table("basket.txt",header = TRUE)
dat.V <- as.matrix(basket[,2:6])
A.matrix <- matrix(grepl("A",dat.V),ncol = 5)
A.num <- rowSums(A.matrix)
B.matrix <- matrix(grepl("B",dat.V),ncol = 5)
B.num <- rowSums(B.matrix)
row.num <- cbind(basket[,1],A.num,B.num)

row.num[row.num[,2] > 0 & row.num[,3] > 0,1]

row.num[row.num[,2] > 0 & row.num[,3] > 2,1]

# Protein
setwd("C:\\Users\\user\\Desktop\\Non-parametric\\")
protein <- read.csv("protein.csv")
dat <- protein[,2:4]
getpara <- function (data, dim) {
    cbind <- cbind(data[,c(dim)],data[,-c(dim)])
    mu <- colMeans(data)
    Sigma <- cov(data)
    dim <- length(dim)
    mu.1 <- mu[1:dim]
    mu.2 <- mu[-(1:dim)]
    sigma.11 <- as.matrix(Sigma[1:dim,1:dim])
    sigma.12 <- t(as.matrix(Sigma[1:dim,-(1:dim)],nrow = dim))
    sigma.22 <- as.matrix(Sigma[-(1:dim),-(1:dim)])
    return(list(mu.1,mu.2,sigma.11,sigma.12,sigma.22))   
}

## Conditional mean for V1
con <- getpara(dat,1)
con.mean.1 <- con[[1]] + con[[4]]%*%solve(con[[5]])%*%t(dat[,2:3] - con[[2]])
con.mean.1 <- as.vector(con.mean.1)
con.mean.1 <- data.frame(protein[,1],con.mean.1)
names(con.mean.1) <- c("country", "con.meanV1")
con.mean.1

## Conditional Variance for V1
con.var.1 <- con[[3]] - con[[4]]%*%solve(con[[5]])%*%t(con[[4]])
con.var.1

## Conditional mean for V23
con <- getpara(dat,2:3)
con.mean.23 <- con[[1]] + t(con[[4]])%*%solve(con[[5]])%*%t(dat[,1] - con[[2]])
con.mean.23 <- t(con.mean.23)
con.mean.23 <- data.frame(protein[,1],con.mean.23)
names(con.mean.23) <- c("country", "con.meanV2","con.meanV3")
con.mean.23

## Conditional Variance for V1
con.var.23 <- con[[3]] - t(con[[4]])%*%solve(con[[5]])%*%(con[[4]])

# Euclid Algorithm
gcd <- function(a,b) {
    if (a < b) {
        c <- a
        a <- b
        b <- c
    }
    while ( b > 0 ) {
        c <- a
        a <- b
        b <- c %% b
    }
    return(a)
}

gcd(978549243672,849866359923)

