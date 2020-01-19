library(shiny)
require(ggplot2)
require(dplyr)
require(readr)


data <- read_csv("CES11.csv")
data <- subset(data, select=-c(X1, id, weight, population))
data [data == 'ON'] <- "Ontario"
data [data == 'QC'] <- "Quebec"
data [data == 'NS'] <- "Nova Scotia"
data [data == 'NB'] <- "New Brunswick"
data [data == 'MB'] <- "Manitoba"
data [data == 'BC'] <- "British Columbia"
data [data == 'PE'] <- "Prince Edward Island"
data [data == 'SK'] <- "Saskatchewan"
data [data == 'AB'] <- "Alberta"
data [data == 'NL'] <- "Newfoundland and Labrador"
data[] <- lapply(data, factor)
data2 <- data
data2 <- subset(data, select=-c(abortion))
data2$overall <- data$abortion
data2 <- data2[,c(6,1,2,3,4,5)]

library(maps)
library(mapproj)
library(mapdata)
library(rgeos)
library(maptools)
library(sp)
library(raster)
library(rgdal)
library(leaflet)
library(leaflet.extras)

#can1<-getData('GADM', country="CAN", level=1) # provinces
#plot(can1)

ui <- fluidPage(
    titlePanel("2011 Canadian National Election Study, With Attitude Toward Abortion"),
    sidebarLayout(
        sidebarPanel(
            #selectInput("inCategory", "Choose category:", choices = colnames(data2))
            selectInput("inProvince", "Province:", choices = data$province)
        ),
        mainPanel(
           leafletOutput(outputId = "mymap"),
           tableOutput("provinceTable")
        )
    ),
)

server <- function(input, output) {
    #Create Table
    output$provinceTable <- renderTable({
        provinceFilter <- subset(data, data$province == input$inProvince)
    })
    
    #create the map
    output$mymap <- renderLeaflet({leaflet() %>%
                                      addTiles(
                                          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png") %>%
                                      setView(lng = -93.85, lat = 37.45, zoom = 4)
    })
}

shinyApp(ui = ui, server = server)
