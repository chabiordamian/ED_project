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

sum <- table(data2$overall)[1] + table(data2$overall)[2]
sum <- as.numeric(sum)
lbls <- paste(names(table(data2$overall)), (round(table(data2$overall)/sum*100, digits = 2)), "%")
pie(table(data2$overall), labels = lbls, main ="Should abortion be banned?")


provinces_json <- geojsonio::geojson_read("canada.geojson", what = "sp")

bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
pal <- colorBin("YlOrRd", domain = provinces_json$cartodb_id, bins = bins)

if (!file.exists("./src/ref/ne_50m_admin_1_states_provinces_lakes/ne_50m_admin_1_states_provinces_lakes.dbf")){
    download.file(file.path('http://www.naturalearthdata.com/http/',
                            'www.naturalearthdata.com/download/50m/cultural',
                            'ne_50m_admin_1_states_provinces_lakes.zip'), 
                  f <- tempfile())
    unzip(f, exdir = "./src/ref/ne_50m_admin_1_states_provinces_lakes")
    rm(f)
}

region <- readOGR("./src/ref/ne_50m_admin_1_states_provinces_lakes", 'ne_50m_admin_1_states_provinces_lakes', encoding='UTF-8')

ui <- fluidPage(
    titlePanel("2011 Canadian National Election Study, With Attitude Toward Abortion"),
    sidebarLayout(
        sidebarPanel(
            selectInput("inCategory", "Choose category:", choices = colnames(data2)),
            selectInput("inProvince", "Province:", choices = data$province)
        ),
        mainPanel(
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("overallChart")),
                        tabPanel("Map", leafletOutput(outputId = "mymap")),
                        tabPanel("Table", tableOutput("provinceTable")))
        )
    ),
)

server <- function(input, output) {
    
    output$provinceTable <- renderTable({
        provinceFilter <- subset(data, data$province == input$inProvince)
    })
    


    #create the map
    output$mymap <- renderLeaflet({leaflet() %>%
            addTiles() %>% 
            setView(lng = -113.71, lat=60,585,  zoom = 2)
    })

    # update the map markers and view on location selectInput changes
    observeEvent(input$inProvince,
                 if (!is.null(input$inProvince)){
                        output$mymap <- NULL
                        output$mymap <- renderLeaflet({leaflet()%>% addTiles() %>% 
                             setView(lng = -113.71, lat = 60,585,  zoom = 2) %>% 
                             addPolygons(data = subset(region, name %in% c(input$inProvince)), 
                                         fillColor = topo.colors(10, alpha = NULL), weight = 1)})
                 }
    )
    
    
    observeEvent(input$inCategory, 
        if(input$inCategory == 'overall'){
            output$overallChart <- renderPlot({
                pie(table(data2$overall), labels = lbls, main =paste0(input$inCategory,"\n Should abortion be banned?"))
            })
        }
    )
}

shinyApp(ui = ui, server = server)
