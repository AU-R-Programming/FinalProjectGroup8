#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(gridExtra)
library(datasets)
library(ggplot2)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Example of my_lm function using mtcars data from R"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(

        sidebarPanel(
            sliderInput("alpha", "Value for alpha: ", min = 0, max = 1, value = .5),
            selectInput("approach", "Select approach (asymptotic or bootstrap): ",list("bootstrap",
                        "asymptotic")),
            actionButton("button", "Compute confidence intervals")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
             tabPanel("Confidence Intervals", tableOutput("table")),
             tabPanel("Resid vs Fitted", plotOutput("mpgPlot")),
             tabPanel("qq Plot", plotOutput("mpgQq"))
            )

    )
)
)

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



# Define server logic required to draw a histogram
server <- function(input, output) {

    a = eventReactive(input$button, {
        my_lm(y = mpgData$mpg, x = mpgData[,c("cyl","disp","hp","drat","wt","qsec","gear")],
              alpha = input$alpha, approach = input$approach)



    })


    output$table = renderTable(a()$ci, rownames = TRUE,
                               colnames = TRUE)

       output$mpgPlot = renderPlot({

           ggplot(mpgData, aes(z,resid))+
               geom_point(
                   data = mpgData,
                   aes(z,resid),
                   size = 5, colour = "black")+
               ggtitle("Residual vs Fitted values")

       })

       output$mpgQq = renderPlot({

           ggplot(mpgData, aes(y.hat,resid))+
               geom_point(
                   data = mpgData,
                   aes(y.hat,resid),
                   size = 5, colour = "black")+
               ggtitle("qq-plot of Residuals")

       })

}

# Run the application
shinyApp(ui = ui, server = server)
