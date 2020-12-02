

#' @title Linear Regression Model
#' @description Perform a linear regression based on a user input alpha value for the confidence intervals and
#' gives the user a choice beteen using the asymptotic or bootstrap approach.
#' The package returns the mean square prediction error (MSPE), an F-test, and creates plots for the residuals,
#' residual vs. fitted values, and a histogram of the residuals.
#' @param y The \code{vector} dependent variable
#' @param x The \code{matrix} independent variable
#' @param alpha The \code{numeric} significance devlevel input by the user to estimate the confidence intervals
#' @param approach The \code{character} allows the user to choose bootstrap or asymptotic approach
#' @return A \code{list} which returns the following attributes:
#' \describe{
#'      \item{ci.beta}{The confidence intervals}
#'      \item{sigma2.hat}{The standard deviation}
#'      \item{var.beta}{Variance}
#'      \item{MSPE}{The mean square prediction error}
#'      \item{Fstar}{An F Test}
#'      \item{Proboftest}{The probability or p-value of test}
#' }
#' @importFrom graphics plot
#' @export
#' @examples
#' n=20
#' my_lm (y= rbinom(n, size = 15,prob=0.5), x=cbind(rbinom(n, size = 22,prob=0.5), rnorm(n, 42, 65),rnorm(n, 20, 20)), alpha = 0.01, approach =  "bootstrap")
#' n=30
#' my_lm (y= rnorm(n, 15, 20), x=cbind(rbinom (n, size = 45,prob=0.1), rnorm(n, 85, 22)), alpha = 0.10, approach = "asymptotic")

my_lm = function(y, x, alpha, approach) {

  y <- as.vector(y)
  x <- as.matrix(x)
  n <- length(y)
  p <- dim(x)[2]
  df <- n - p

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
if (approach=="bootstrap")
{
  smp<-cbind(x,y)
  beta.star<-NULL
  for(i in 1:200){
    # nonparametric bootstrap
    bootdata=smp[sample(nrow(smp), size = n, replace = TRUE),]
    x<-bootdata[,1:p]
    y<-bootdata[,p+1]
    beta <- t(solve(t(x)%*%x)%*%t(x)%*%y)
    beta.star<-rbind(beta,beta.star)
  }
  quant <- 1 - alpha/2
  ci.beta<-apply(as.matrix(beta.star), 2, function(x){mean(x)+c(-qnorm(p = quant),qnorm(p = quant))*sd(x)/sqrt(length(x))})
  ci.beta<-t(ci.beta)
  ci.beta <- as.data.frame(ci.beta)
  colnames(ci.beta) <- c("low", "high")
}else{
  quant <- 1 - alpha/2
  ci.beta <- cbind(beta.hat - qnorm(p = quant)*sqrt(var.beta), beta.hat +
                     qnorm(p = quant)*sqrt(var.beta))
  ci.beta <- as.data.frame(ci.beta)
  colnames(ci.beta) <- c("low", "high")
}
######################################################
#plot
##residual vs fitted values
y.hat <- x%*%beta.hat
fig1<-plot(y.hat, resid,
main="Residual vs Fitted Values",
xlab="Fitted values",
ylab="Residuals")

##qq-plot of residuals
nr<- length(resid)
zpercent<-1/nr
z<-qnorm(p = zpercent*(1:nr))
fig2<-plot(z,resid,
main="qq-plot of residuals",
xlab="Theoretical Quantiles",
ylab="Sample Quantiles")

##Histogram (or density) of residuals
densi<-density(resid)
fig3<-plot(densi,
main="Histogram (density) of Residuals",
xlab="Residuals",
ylab="Density")

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
return(list(  ci = ci.beta,
             MSPE=MSPE,Ftest=Fstar,
             Probability=Probofftest))
}


