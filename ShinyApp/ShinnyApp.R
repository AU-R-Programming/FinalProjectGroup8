#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Example of my_lm function using mtcars data from R"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput("alpha", "Value for alpha: ", 0.5, min = 0, max = 1),
            textInput("approach", "Select approach (asymptotic or bootstrap): ", "asymptotic"),
            actionButton("button", "Compute linear regression")
        ),


        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("mpgPlot")
        )
    )
)

n = 517
mpgData <- mtcars



# Define server logic required to draw a histogram
server <- function(input, output) {

    a <- eventReactive(input$button, {
            my_lm(y = mpgData$mpg, x = mpgData[,c("cyl","disp","hp","drat","wt","qsec","gear")],
                  alpha = input$alpha, approach = input$approach)
    })

       output$mpgPlot = renderPlot({
           plot(unlist(a()))
       })
}

# Run the application
shinyApp(ui = ui, server = server)
