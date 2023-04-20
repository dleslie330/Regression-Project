file <- read.csv("/Users/BenFavorite/Documents/RegressionDataSet/Real Estate.txt", header = TRUE, sep = ",")
file <- file[!apply(is.na(file) | file == "", 1, all),]
 
	 
	x1 <- file[1:nrow(file),2]
	x2 <- file[1:nrow(file),3]
	x3 <- file[1:nrow(file),4]
	x4 <- file[1:nrow(file),5]
	x5 <- file[1:nrow(file),6]
	x6 <- file[1:nrow(file),7]
	 
y <- file[1:nrow(file),8]
 

n <- length(y)
const <- rep(1,n)
 
X <- cbind(const,x1,x2,x3,x4,x5,x6)
p <- dim(X)[2]
k <- p-1
XpX <- t(X)%*%X
invXpX <- solve(XpX)
Xpy <- t(X)%*%y

BetaHat<- invXpX%*%Xpy
yhat <- X%*%BetaHat

I <- diag(n)
J <- const%*%t(const)
H <- X%*%invXpX%*%t(X)

hii <- diag(H)

# Anova
SST <- t(y)%*%(I-J/n)%*%y
SSR <- t(y)%*%(H-J/n)%*%y
SSE <- t(y)%*%(I-H)%*%y

MSR <- SSR/k
MSE <- SSE/(n-k-1)

R2 <- SSR/SST
R2A <- 1-(n-1)/(n-k-1)*(1-R2)

S2 <- c(MSE)

s <- sqrt(S2)

sebeta <- sqrt(S2*diag(invXpX))
tval <- BetaHat/sebeta
pvalT <- 2*(1 - pt(abs(tval), n-k-1))	

F0 <- MSR/MSE
PvalF <- 1 - pf(F0,k,n-k-1)

df1 <- data.frame(Source=c("regression","Error","Total"), df=c(k, n-k-1, n-1), MS=round(c(MSR, MSE, 0),3), FO=round(c(F0, 0, 0),3), pvalue=c(PvalF, 0, 0))
 
print(df1)

#remove insignificant regressor and dates
file <- read.csv("/Users/BenFavorite/Documents/RegressionDataSet/Real Estate.txt", header = TRUE, sep = ",")
file <- file[!apply(is.na(file) | file == "", 1, all),]


X <- cbind(const,x2,x3,x4,x5)
p <- dim(X)[2]
k <- p-1
XpX <- t(X)%*%X
invXpX <- solve(XpX)
Xpy <- t(X)%*%y

BetaHat<- invXpX%*%Xpy
yhat <- X%*%BetaHat

I <- diag(n)
J <- const%*%t(const)
H <- X%*%invXpX%*%t(X)

hii <- diag(H)

# Anova
SST <- t(y)%*%(I-J/n)%*%y
SSR <- t(y)%*%(H-J/n)%*%y
SSE <- t(y)%*%(I-H)%*%y

MSR <- SSR/k
MSE <- SSE/(n-k-1)

R2 <- SSR/SST
R2A <- 1-(n-1)/(n-k-1)*(1-R2)

S2 <- c(MSE)

s <- sqrt(S2)

sebeta <- sqrt(S2*diag(invXpX))
tval <- BetaHat/sebeta
pvalT <- 2*(1 - pt(abs(tval), n-k-1))	

F0 <- MSR/MSE
PvalF <- 1 - pf(F0,k,n-k-1)

df1 <- data.frame(Source=c("regression","Error","Total"), df=c(k, n-k-1, n-1), MS=round(c(MSR, MSE, 0),3), FO=round(c(F0, 0, 0),3), pvalue=c(PvalF, 0, 0))
 
print(df1)


df2 <- data.frame(Parameter=c("Intercept","house Age", "Distance to nearest MRT station", "Number of convenience stores", "Latitude"), Estimate=BetaHat, s.e.=sebeta, Tvalue = tval, pvalue = pvalT)
 
print(df2)

e <- y-yhat
e_std <- e/s
e_stu <- e/(s*sqrt(1-hii))
e_press <- e/(1-hii)

df3 <- data.frame(y_ihat=yhat, Studentized_residuals = e_stu)



plot(df3,pch = 20, cex=1.5, col='steelblue')


r_i <- e/(s*sqrt(1-hii))
r_i2 <- t(r_i)%*%r_i
d_i <- as.vector(r_i2/p)*(hii/(1-hii))

s_i <- sqrt(1/(n-p-1))*(sqrt(n-p)*s - e*sqrt(1/(1-hii)))
s_i2 <-(1/(n-p-1))*((n-p)*S2 - e/(1-hii))
t_i <- e/(s_i*sqrt(1-hii))


DFFITS_i <- sqrt(hii/(1-hii))*t_i
COVRATIO_i <- (s_i2/S2)^p *(1/(1-hii))


print(d_i) #408, 392, 360, 329
print(DFFITS_i) #271
print(COVRATIO_i)




#remove i = 329

x2 <- c(file[1:35,3],file[37:nrow(file),3])
x3 <- c(file[1:35,4],file[37:nrow(file),4])
x4 <- c(file[1:35,5],file[37:nrow(file),5])
x5 <- c(file[1:35,6],file[37:nrow(file),6])


y <- c( file[1:35,8],file[37:nrow(file),8])

 n <- length(y)
 const <- rep(1,n)
X <- cbind(const,x2,x3,x4,x5)
p <- dim(X)[2]
k <- p-1
XpX <- t(X)%*%X
invXpX <- solve(XpX)
Xpy <- t(X)%*%y

BetaHat<- invXpX%*%Xpy
yhat <- X%*%BetaHat

I <- diag(n)
J <- const%*%t(const)
H <- X%*%invXpX%*%t(X)

hii <- diag(H)

# Anova
SST <- t(y)%*%(I-J/n)%*%y
SSR <- t(y)%*%(H-J/n)%*%y
SSE <- t(y)%*%(I-H)%*%y

MSR <- SSR/k
MSE <- SSE/(n-k-1)

R2 <- SSR/SST
R2A <- 1-(n-1)/(n-k-1)*(1-R2)

S2 <- c(MSE)

s <- sqrt(S2)

sebeta <- sqrt(S2*diag(invXpX))
tval <- BetaHat/sebeta
pvalT <- 2*(1 - pt(abs(tval), n-k-1))	

F0 <- MSR/MSE
PvalF <- 1 - pf(F0,k,n-k-1)

e <- y-yhat
e_std <- e/s
e_stu <- e/(s*sqrt(1-hii))
e_press <- e/(1-hii)


r_i <- e/(s*sqrt(1-hii))
r_i2 <- t(r_i)%*%r_i
d_i <- as.vector(r_i2/p)*(hii/(1-hii))

s_i <- sqrt(1/(n-p-1))*(sqrt(n-p)*s - e*sqrt(1/(1-hii)))
s_i2 <-(1/(n-p-1))*((n-p)*S2 - e/(1-hii))
t_i <- e/(s_i*sqrt(1-hii))


DFFITS_i <- sqrt(hii/(1-hii))*t_i
COVRATIO_i <- (s_i2/S2)^p *(1/(1-hii))


print(d_i) #408, 392, 360, 329
print(DFFITS_i) #271
print(COVRATIO_i)




df5 <- data.frame(Source=c("regression","Error","Total"), df=c(k, n-k-1, n-1), MS=round(c(MSR, MSE, 0),3), FO=round(c(F0, 0, 0),3), pvalue=c(PvalF, 0, 0))
 
print(df5)

df6 <- data.frame(y_ihat=yhat, Studentized_residuals = e_stu)

plot(df6,pch = 20, cex=1.5, col='steelblue')


#remove 

x1 <- c(file[1:270,2],file[272:nrow(file),2])
x2 <- c(file[1:270,3],file[272:nrow(file),3])
x3 <- c(file[1:270,4],file[272:nrow(file),4])
x4 <- c(file[1:270,5],file[272:nrow(file),5])
x5 <- c(file[1:270,6],file[272:nrow(file),6])


y <- c( file[1:270,8],file[272:nrow(file),8])

 n <- length(y)
 const <- rep(1,n)
X <- cbind(const,x2,x3,x4,x5)
p <- dim(X)[2]
k <- p-1
XpX <- t(X)%*%X
invXpX <- solve(XpX)
Xpy <- t(X)%*%y

BetaHat<- invXpX%*%Xpy
yhat <- X%*%BetaHat

I <- diag(n)
J <- const%*%t(const)
H <- X%*%invXpX%*%t(X)

hii <- diag(H)

# Anova
SST <- t(y)%*%(I-J/n)%*%y
SSR <- t(y)%*%(H-J/n)%*%y
SSE <- t(y)%*%(I-H)%*%y

MSR <- SSR/k
MSE <- SSE/(n-k-1)

R2 <- SSR/SST
R2A <- 1-(n-1)/(n-k-1)*(1-R2)

S2 <- c(MSE)

s <- sqrt(S2)

sebeta <- sqrt(S2*diag(invXpX))
tval <- BetaHat/sebeta
pvalT <- 2*(1 - pt(abs(tval), n-k-1))	

F0 <- MSR/MSE
PvalF <- 1 - pf(F0,k,n-k-1)

e <- y-yhat
e_std <- e/s
e_stu <- e/(s*sqrt(1-hii))
e_press <- e/(1-hii)


df7 <- data.frame(Source=c("regression","Error","Total"), df=c(k, n-k-1, n-1), MS=round(c(MSR, MSE, 0),3), FO=round(c(F0, 0, 0),3), pvalue=c(PvalF, 0, 0))
 
print(df5)

df8 <- data.frame(y_ihat=yhat, Studentized_residuals = e_stu)

plot(df8,pch = 20, cex=1.5, col='steelblue')


# remove both

x2 <- c(file[1:35,3],file[37:270,3],file[272:nrow(file),3])
x3 <- c(file[1:35,4],file[37:270,4],file[272:nrow(file),4])
x4 <- c(file[1:35,5],file[37:270,5],file[272:nrow(file),5])
x5 <- c(file[1:35,6],file[37:270,6],file[272:nrow(file),6])

y <- c(file[1:35,8],file[37:270,8],file[272:nrow(file),8])

 n <- length(y)
 const <- rep(1,n)
X <- cbind(const,x2,x3,x4,x5)
p <- dim(X)[2]
k <- p-1
XpX <- t(X)%*%X
invXpX <- solve(XpX)
Xpy <- t(X)%*%y

BetaHat<- invXpX%*%Xpy
yhat <- X%*%BetaHat

I <- diag(n)
J <- const%*%t(const)
H <- X%*%invXpX%*%t(X)

hii <- diag(H)

# Anova
SST <- t(y)%*%(I-J/n)%*%y
SSR <- t(y)%*%(H-J/n)%*%y
SSE <- t(y)%*%(I-H)%*%y

MSR <- SSR/k
MSE <- SSE/(n-k-1)

R2 <- SSR/SST
R2A <- 1-(n-1)/(n-k-1)*(1-R2)

S2 <- c(MSE)

s <- sqrt(S2)

sebeta <- sqrt(S2*diag(invXpX))
tval <- BetaHat/sebeta
pvalT <- 2*(1 - pt(abs(tval), n-k-1))	

F0 <- MSR/MSE
PvalF <- 1 - pf(F0,k,n-k-1)

e <- y-yhat
e_std <- e/s
e_stu <- e/(s*sqrt(1-hii))
e_press <- e/(1-hii)


df9 <- data.frame(Source=c("regression","Error","Total"), df=c(k, n-k-1, n-1), MS=round(c(MSR, MSE, 0),3), FO=round(c(F0, 0, 0),3), pvalue=c(PvalF, 0, 0))
 
print(df9)

df10 <- data.frame(y_ihat=yhat, Studentized_residuals = e_stu)

plot(df10,pch = 20, cex=1.5, col='steelblue')


library(MASS)
boxcox(lm(y~X))

lambda <- 0.25

# 6. Refit the model


ylambda <- (1/lambda)*(y^(lambda)-1)
Xpylambda <- t(X)%*%ylambda

BetaHatLambda<- invXpX%*%Xpylambda
yhatlambda <- X%*%BetaHatLambda

I <- diag(n)
J <- const%*%t(const)
H <- X%*%invXpX%*%t(X)

hii <- diag(H)

# Anova
SST <- t(ylambda)%*%(I-J/n)%*%ylambda
SSR <- t(ylambda)%*%(H-J/n)%*%ylambda
SSE <- t(ylambda)%*%(I-H)%*%ylambda

MSR <- SSR/k
MSE <- SSE/(n-k-1)

R2 <- SSR/SST
R2A <- 1-(n-1)/(n-k-1)*(1-R2)

S2 <- c(MSE)

s <- sqrt(S2)

e <- ylambda-yhatlambda
e_std <- e/s
e_stu <- e/(s*sqrt(1-hii))
e_press <- e/(1-hii)

# 7. plot the residual
df4 <- data.frame(y_ihat=yhatlambda, Studentized_residuals = e_stu)

plot(df4,pch = 20, cex=1.5, col='steelblue')


#doesn't work





