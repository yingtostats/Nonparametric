

```R
rm(list = ls())
```

# 课本作业

### 使用二分法求零点 
* 输入方程式的函数形式、区间下限、区间上限
* 规定算法收敛的标准是区间长度小于等于$10^{-12}$
* 循环二分法运算直至收敛


```R
# A.5
getzero <- function(equation,lower,upper) {

    stopifnot(is.function(equation)||equation(lower)*equation(upper) < 0 )
    
    while ( upper - lower > 0.000000000001) { #while函数循环算法直至收敛
        
        mid <- (lower+upper) / 2 
        f.mid <- equation(mid) 
        
        if(f.mid == 0) return(mid) 
        else if (f.mid * equation(upper) < 0 ) lower <- mid
        else (upper <- mid)
    
    }
            
    return((upper + lower) / 2)
}
```


```R
equation <- function(x) return(2*x^3 - 4*x^2 + 3*x - 6)
```


```R
getzero(equation,-10,10)
```


2.00000000000017


### CCB编码 
* 制作一个编码对应表：原始单词表 & 编码单词表; 
* strsplit函数把输入单词拆成字母；
* match函数输出对应到原始单词表的行数；
* 输出编码单词表对应行数的字母；
* paste函数把字母合成词


```R
# A.7

enigma <- function(word) {
    stopifnot(is.character(word))
    
    word.split <- strsplit(word,split="") # 返回的是一个list！
    
    CCB.initial <- c(letters,LETTERS) 
    CCB.transcode <- c(LETTERS[c(14:26,1:13)],letters[c(14:26,1:13)])
    
    word.new <- CCB.transcode[match(word.split[[1]],CCB.initial)]
    word.new <- paste(word.new,collapse = "")
    
    return(word.new)
}
```


```R
enigma("People")
```


'cRBCYR'


### 13个人围成一圈，找出最后的人
* 制作一个表，第一列是第一轮大家起始位置，第二列是大家报的号
* 报1,2,3报到3的退出 等价于 报1,2,3,4,5...然后整除3余数为0的退出
* 在表的行数>2的时候作循环
    * 记住最后一行的人报的数
    * 筛选出整除3余数不为0的人
    * 更新行数
    * 更新第二列大家继续报的号
* 循环结束后只剩下两个人，继续报号筛选步骤，得到最后一人，取其第一列即为起始位置


```R
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
```


```R
find(13,3)
```


13


### 寻找含有A与B的行数
* grepl函数用来寻找含有"A"的格子
    * 数据是"A"的则返回TRUE
    * 数据不是"A"的则返回FALSE
* rowSums计算矩阵的行和，TRUE = 1 FALSE = 0, 则输出为此行含有"A"的个数
* 同理对B计算出每行含有"B"的个数
* 按题目筛选，输出满足条件的行数


```R
# A.11
basket <- read.table("basket.txt",header = TRUE)
dat.V <- as.matrix(basket[,2:6])

A <- matrix(grepl("A",dat.V),ncol = 5) #grepl返回的是一个向量
A.num <- rowSums(A)

B <- matrix(grepl("B",dat.V),ncol = 5)
B.num <- rowSums(B)

row.num <- cbind(basket[,1],A.num,B.num)
```


```R
row.num[row.num[,2] > 0 & row.num[,3] > 0,1]
```


<ol class=list-inline>
	<li>1</li>
	<li>2</li>
	<li>4</li>
	<li>5</li>
	<li>6</li>
	<li>7</li>
	<li>9</li>
	<li>10</li>
	<li>12</li>
	<li>13</li>
	<li>14</li>
	<li>17</li>
</ol>




```R
row.num[row.num[,2] > 0 & row.num[,3] > 2,1]
```


<ol class=list-inline>
	<li>4</li>
	<li>5</li>
	<li>12</li>
</ol>



# 补充题

## 条件期望与条件方差
* 写了一个函数，输入数据和维数，输出$\mu_{1}$,$\mu_{2}$,...,$\Sigma_{22}$
* 按照公式计算出相应的条件期望和条件方差
* 矩阵相关函数
    * 求矩阵维数 dim()
    * 求矩阵列和 colMeans()
    * 求协方差矩阵 cov(data)
    * 矩阵转置函数 t()
    * 将数据转换成矩阵形式 as.matrix(...) 
    * 矩阵求逆函数 solve()
    * 矩阵乘法 %*% (维数必须对应可乘)


```R
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
```


```R
## Conditional mean for V1
con <- getpara(dat,1)

con.mean.1 <- con[[1]] + con[[4]]%*%solve(con[[5]])%*%t(dat[,2:3] - con[[2]])
con.mean.1 <- as.vector(con.mean.1)
con.mean.1 <- data.frame(protein[,1],con.mean.1)
names(con.mean.1) <- c("country", "con.meanV1")

con.mean.1
```


<table>
<thead><tr><th></th><th scope=col>country</th><th scope=col>con.meanV1</th></tr></thead>
<tbody>
	<tr><th scope=row>1</th><td>Albania         </td><td>6.02063365891832</td></tr>
	<tr><th scope=row>2</th><td>Austria          </td><td>-2.19090001321767</td></tr>
	<tr><th scope=row>3</th><td>Belgium         </td><td>12.1739516069855</td></tr>
	<tr><th scope=row>4</th><td>Bulgaria         </td><td>-6.16304348628991</td></tr>
	<tr><th scope=row>5</th><td>Czechoslovakia  </td><td>8.41752573006964</td></tr>
	<tr><th scope=row>6</th><td>Denmark          </td><td>-2.63298727250171</td></tr>
	<tr><th scope=row>7</th><td>EGermany        </td><td>10.5057587610195</td></tr>
	<tr><th scope=row>8</th><td>Finland         </td><td>-3.1942433103345</td></tr>
	<tr><th scope=row>9</th><td>France          </td><td>10.0767851689874</td></tr>
	<tr><th scope=row>10</th><td>Greece           </td><td>-2.36670372097745</td></tr>
	<tr><th scope=row>11</th><td>Hungary         </td><td>8.34663136032471</td></tr>
	<tr><th scope=row>12</th><td>Ireland            </td><td>0.00396101567014995</td></tr>
	<tr><th scope=row>13</th><td>Italy           </td><td>10.6082064987538</td></tr>
	<tr><th scope=row>14</th><td>Netherlands      </td><td>-3.73935119399387</td></tr>
	<tr><th scope=row>15</th><td>Norway          </td><td>10.2543075366702</td></tr>
	<tr><th scope=row>16</th><td>Poland           </td><td>-4.83620882179675</td></tr>
	<tr><th scope=row>17</th><td>Portugal        </td><td>6.74154525033747</td></tr>
	<tr><th scope=row>18</th><td>Romania          </td><td>-6.49489537407345</td></tr>
	<tr><th scope=row>19</th><td>Spain           </td><td>11.6126955691527</td></tr>
	<tr><th scope=row>20</th><td>Sweden          </td><td>-2.1813937195284</td></tr>
	<tr><th scope=row>21</th><td>Switzerland     </td><td>9.53700331881365</td></tr>
	<tr><th scope=row>22</th><td>UK              </td><td>1.33612171364896</td></tr>
	<tr><th scope=row>23</th><td>USSR            </td><td>8.85182535558748</td></tr>
	<tr><th scope=row>24</th><td>WGermany         </td><td>-2.20401368046953</td></tr>
	<tr><th scope=row>25</th><td>Yugoslavia     </td><td>6.5777094365475</td></tr>
</tbody>
</table>




```R
## Conditional Variance for V1
con.var.1 <- con[[3]] - con[[4]]%*%solve(con[[5]])%*%t(con[[4]])
con.var.1
```


<table>
<tbody>
	<tr><td>6.555405</td></tr>
</tbody>
</table>




```R
## Conditional mean for V23
con <- getpara(dat,2:3)

con.mean.23 <- con[[1]] + t(con[[4]])%*%solve(con[[5]])%*%t(dat[,1] - con[[2]])
con.mean.23 <- t(con.mean.23)
con.mean.23 <- data.frame(protein[,1],con.mean.23)
names(con.mean.23) <- c("country", "con.meanV2","con.meanV3")

con.mean.23
```


<table>
<thead><tr><th></th><th scope=col>country</th><th scope=col>con.meanV2</th><th scope=col>con.meanV3</th></tr></thead>
<tbody>
	<tr><th scope=row>1</th><td>Albania         </td><td>22.3922435418446</td><td>22.5868648590948</td></tr>
	<tr><th scope=row>2</th><td>Austria         </td><td>20.2876801345004</td><td>20.1260834756618</td></tr>
	<tr><th scope=row>3</th><td>Belgium         </td><td>28.3551731959863</td><td>29.5590787788215</td></tr>
	<tr><th scope=row>4</th><td>Bulgaria        </td><td>18.3584970111016</td><td>17.8703672075149</td></tr>
	<tr><th scope=row>5</th><td>Czechoslovakia  </td><td>21.6907224060632</td><td>21.7666043979505</td></tr>
	<tr><th scope=row>6</th><td>Denmark         </td><td>23.2691449615713</td><td>23.6121904355252</td></tr>
	<tr><th scope=row>7</th><td>EGermany        </td><td>19.4107787147737</td><td>19.1007578992314</td></tr>
	<tr><th scope=row>8</th><td>Finland         </td><td>21.3399618381725</td><td>21.3564741673783</td></tr>
	<tr><th scope=row>9</th><td>France          </td><td>36.2472859735269</td><td>38.7870089666951</td></tr>
	<tr><th scope=row>10</th><td>Greece          </td><td>22.5676238257899</td><td>22.7919299743809</td></tr>
	<tr><th scope=row>11</th><td>Hungary         </td><td>13.973989912468 </td><td>12.7437393253629</td></tr>
	<tr><th scope=row>12</th><td>Ireland         </td><td>29.0566943317677</td><td>30.3793392399658</td></tr>
	<tr><th scope=row>13</th><td>Italy           </td><td>20.4630604184458</td><td>20.3311485909479</td></tr>
	<tr><th scope=row>14</th><td>Netherlands     </td><td>21.3399618381725</td><td>21.3564741673783</td></tr>
	<tr><th scope=row>15</th><td>Norway          </td><td>21.1645815542272</td><td>21.1514090520922</td></tr>
	<tr><th scope=row>16</th><td>Poland          </td><td>16.7800744555935</td><td>16.0247811699402</td></tr>
	<tr><th scope=row>17</th><td>Portugal        </td><td>15.5524124679761</td><td>14.5893253629377</td></tr>
	<tr><th scope=row>18</th><td>Romania         </td><td>15.5524124679761</td><td>14.5893253629377</td></tr>
	<tr><th scope=row>19</th><td>Spain           </td><td>17.1308350234842</td><td>16.4349114005124</td></tr>
	<tr><th scope=row>20</th><td>Sweden          </td><td>22.0414829739539</td><td>22.1767346285226</td></tr>
	<tr><th scope=row>21</th><td>Switzerland     </td><td>27.653652060205 </td><td>28.7388183176772</td></tr>
	<tr><th scope=row>22</th><td>UK              </td><td>35.1950042698548</td><td>37.5566182749786</td></tr>
	<tr><th scope=row>23</th><td>USSR            </td><td>20.9892012702818</td><td>20.9463439368062</td></tr>
	<tr><th scope=row>24</th><td>WGermany        </td><td>24.6721872331341</td><td>25.2527113578138</td></tr>
	<tr><th scope=row>25</th><td>Yugoslavia      </td><td>12.3955673569599</td><td>10.8981532877882</td></tr>
</tbody>
</table>




```R
## Conditional Variance for V23
con.var.23 <- con[[3]] - t(con[[4]])%*%solve(con[[5]])%*%(con[[4]])
con.var.23
```


<table>
<thead><tr><th></th><th scope=col>RedMeat</th><th scope=col>WhiteMeat</th></tr></thead>
<tbody>
	<tr><th scope=row>RedMeat</th><td> 7.361024</td><td>-2.600407</td></tr>
	<tr><th scope=row>WhiteMeat</th><td>-2.600407</td><td> 8.393695</td></tr>
</tbody>
</table>



### Euclid Algorithm
* 输入a,b
* 如果a < b则调换其值 s.t. a > b
* 在b > 0的情况下，按照算法循环，gcd(a,b) = gcd(b,a%b)
* 当b = 0时，输出a即为最大公因数


```R
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
```


92463

