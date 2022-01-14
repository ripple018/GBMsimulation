
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Geometric Brownian Motion Simulation"),

    # Sidebar with two slider inputs for mean and standard deviation 
    sidebarLayout(
        sidebarPanel(
            sliderInput("mu",
                        "Mean:",
                        min = 0,
                        max = 0.5,
                        value = 0.05),

            sliderInput("std",
                        "Standard Deviation:",
                        min = 0,
                        max = 1,
                        value = 0.2)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("hg")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    gbm <- function(S0,mu,sig,timespan=1,interval,path){
        
        dt = timespan / interval
        paths = matrix(0,path,interval+1)
        paths[,1] = S0
        
        for (t in 2:(interval+1)) {
            paths[,t] = paths[,t-1] * exp((mu-sig^2/2)*dt+sig*rnorm(path)*sqrt(dt))
        }
        
        return(t(paths))
    }

    output$hg <- renderPlot({
        # generate paths
        pathMatrix <- gbm(S0=100,mu=input$mu,sig=input$std,interval=250,path=1000)

        # draw the histogram with the specified number of bins
        hist(pathMatrix[251,],main="Histogram of Value at 250th day",xlab="Value")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
