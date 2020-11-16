#Here is the example to test the code:
getwd()
forestfires<- read.csv("forestfires.csv")
y<-forestfires$Y
x<-forestfires[,c("X","DC")]
alpha<-0.05

my_lm(y,x,0.05)


# The first part of the code is from section 6.4 of the book
# linear regression model
my_lm = function(y, x, alpha) {
  
# Make sure data formats are appropriate
y <- as.vector(y)
x <- as.matrix(x)
  
# Define parameters
n <- length(y)
p <- dim(x)[2]
df <- n - p
  
# Estimate beta through Eq. (6.1)
beta.hat <- solve(t(x)%*%x)%*%t(x)%*%y
  
# Estimate of the residual variance (sigma2) from Eq. (6.3)
# Compute residuals(n*1)
resid <- y - x%*%as.matrix(beta.hat) 
sigma2.hat <- (1/df)*t(resid)%*%resid
sigma2.hat<-as.numeric(sigma2.hat)
# Estimate of the variance of the estimated beta from Eq. (6.2)
var.beta <- sigma2.hat*solve(t(x)%*%x)
var.beta<-diag(var.beta)
###################################################### 
## Estimate of the confidence interval based on alpha
  quant <- 1 - alpha/2
  ci.beta <- cbind(beta.hat - qnorm(p = quant)*sqrt(var.beta), beta.hat + 
                 qnorm(p = quant)*sqrt(var.beta))
  df <- as.data.frame(ci.beta)
  colnames(df) <- c("low", "high")
  
######################################################
#plot
##residual vs fitted values
y.hat <- x%*%beta.hat
fig1<-plot(y.hat, resid,
main="residual vs fitted values",
xlab="fitted values",
ylab="residual")

##qq-plot of residuals
nr<- length(resid)
zpercent<-1/nr
z<-qnorm(p = zpercent*(1:nr))
fig2<-plot(z,resid,
main="qq-plot of residuals",
xlab="theoretical quantiles",
ylab="sample quantiles")

##Histogram (or density) of residuals
densi<-density(resid)
fig3<-plot(densi,
main="Histogram (or density) of residuals",
xlab="residual",
ylab="density")

######################################################
#Mean Square Prediction Error (MSPE)
MSPE<- (1/n)*t(resid)%*%resid

######################################################
#F-test
yaverage<-mean(y)
ybar<-rep(yaverage, n)
SSM<-t(y.hat-ybar)%*%(y.hat-ybar)
resid<-as.vector(resid)
SSE<-t(resid)%*%resid
MSM<-SSM/(p-1)
MSE<-SSE/(n-p)
Fstar<-MSM/MSE
Probofftest<-pf(Fstar, p-1, n-p, lower.tail = FALSE)

# Return all estimated values
return(list( ci = ci.beta,fig1,fig2,fig3,MSPE=MSPE,
             Ftest=Fstar,Probability=Probofftest))

}

