% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Final_Project_Code.R
\name{my_lm}
\alias{my_lm}
\title{Linear Regression Model}
\usage{
my_lm(y, x, alpha, approach)
}
\arguments{
\item{y}{The \code{vector} dependent variable}

\item{x}{The \code{matrix} independent variable}

\item{alpha}{The \code{numeric} significance level input by the user to estimate the confidence intervals}

\item{approach}{The \code{character} allows the user to choose bootstrap or asymptotic approach}
}
\value{
A \code{list} which returns the following attributes:
\describe{
     \item{ci.beta}{The confidence intervals}
     \item{sigma2.hat}{The standard deviation}
     \item{var.beta}{Variance}
     \item{MSPE}{The mean square prediction error}
     \item{Fstar}{An F Test}
     \item{Proboftest}{The probability or p-value of test}
}
}
\description{
Perform a linear regression based on a user input alpha value for the confidence intervals and
gives the user a choice beteen using the asymptotic or bootstrap approach.
The package returns the mean square prediction error (MSPE), an F-test, and creates plots for the residuals,
residual vs. fitted values, and a histogram of the residuals.
}
\examples{
n=20
my_lm (y= rbinom(n, size = 15,prob=0.5), x=cbind(rbinom(n, size = 22,prob=0.5), rnorm(n, 0, 1),rnorm(n, 20, 20)), alpha = 0.01, approach =  "bootstrap")
n=30
my_lm (y= rnorm(n, 15, 20), x=cbind(rbinom (n, size = 45,prob=0.1), rnorm(n, 0, 1)), alpha = 0.10, approach = "asymptotic")
}
