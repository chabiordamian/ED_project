library(shiny)
require(ggplot2)
require(dplyr)
require(readr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("2011 Canadian National Election Study, With Attitude Toward Abortion"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    ),
    #show region selection
    selectInput(inputId = 1,label ="Region", data("AB", "BC", "MB", "NB", "NL", "NS", "ON", "PE", "QC", "SK"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    data <- read_csv("https://vincentarelbundock.github.io/Rdatasets/csv/carData/CES11.csv")
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
