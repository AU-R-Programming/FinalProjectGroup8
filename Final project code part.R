#Here is the example to test the code:
forestfires<- read.csv("/Users/LynnVonHagen/Desktop/forestfires.csv")
y<-forestfires$Y
x<-forestfires[,c("X","DC")]
alpha<-0.05

my_lm(y,x,0.05)

#' @title Linear Regression Model
#' @description Perform a linear regression based on a user input alpha value for the confidence intervals and
#' gives the user a choice beteen using the asymptotic or bootstrap approach.
#' The package returns the mean square prediction error (MSPE), an F-test, and creates plots for the residuals,
#' residual vs. fitted values, and a histogram of the residuals.
#' @param y The /code {vector} dependent variable
#' @param x The /code {vector} independent variable
#' @param 0.05 The /code {numeric} significance level input by the user to estimate the confidence intervals
#' @return A /code {list}  which returns the following attributes:
#' \describe {
#'      \{ci}{Confidence intervals}
#'      \{MSPE}{The mean square prediction error}
#'      \{Ftest}{An F Test}
#'      \{Probability}{The probability of test}
#'      }
#' @importFrom ggplot2
#' @export
#' @examples
#' my_lm (y= c(18, 45, 22), x=??, alpha=.01)
#' my_lm (y= c(458, 32, 99) x=??, alpha=.10)

# The first part of the code is from section 6.4 of the book

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
return(list( ci = ci.beta,MSPE=MSPE,
             Ftest=Fstar,Probability=Probofftest))

}
roxygen2::roxygenize

