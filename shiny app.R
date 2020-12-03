library(shiny)
library(datasets)
library(ggplot2)
# Define UI for miles per gallon application
ui<-shinyUI(pageWithSidebar(

  # Application title
  headerPanel("A linear Function Using the mtcars Dataset From R"),

  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    selectInput("var", "Plot:",
                list("resid vs fitted" = "y.hat",
                     "qq-plot" = "z"
                    ))

  ),

  mainPanel(
    plotOutput("mpgPlot")
  )
))

mpgData <- mtcars
y<-mpgData$mpg
x<-mpgData[,c("cyl","disp","hp","drat","wt","qsec","gear")]
y <- as.vector(y)
x <- as.matrix(x)
n <- length(y)
p <- dim(x)[2]
df <- n - p
beta.hat <- solve(t(x)%*%x)%*%t(x)%*%y
resid <- y - x%*%as.matrix(beta.hat)
mpgData$resid <- y - x%*%as.matrix(beta.hat)
sigma2.hat <- (1/df)*t(resid)%*%resid
sigma2.hat<-as.numeric(sigma2.hat)
# Estimate of the variance of the estimated beta from Eq. (6.2)
var.beta <- sigma2.hat*solve(t(x)%*%x)
var.beta<-diag(var.beta)
y.hat <- x%*%beta.hat
mpgData$y.hat <- x%*%beta.hat
nr<- length(resid)
zpercent<-1/nr
z<-qnorm(p = zpercent*(1:nr))
mpgData$z<-qnorm(p = zpercent*(1:nr))
densi<-density(resid)


# Define server logic required to plot various variables against mpg
server<-shinyServer(function(input, output) {

  output$mpgPlot <- renderPlot({

if(input$var=="z"){

     ggplot(mpgData, aes(z,resid))+
       geom_point(
        data = mpgData,
        aes(z,resid),
        size = 5, colour = "black")
}else{

  ggplot(mpgData, aes(y.hat,resid))+
    geom_point(
      data = mpgData,
      aes(y.hat,resid),
      size = 5, colour = "black")
               }

  })
})
shinyApp(ui = ui, server = server)
