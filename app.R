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

#pie(table(data2$overall), labels = lbls, main ="Should abortion be banned?")
lbls <- paste(names(table(data2$overall)), (round(table(data2$overall)/sum*100, digits = 2)), "%")
lbls2 <- paste(names(table(data2$province)), (round(table(data2$province)/sum*100, digits = 2)), "%")
lbls3 <- paste(names(table(data2$gender)), (round(table(data2$gender)/sum*100, digits = 2)), "%")
lbls4 <- paste(names(table(data2$importance)), (round(table(data2$importance)/sum*100, digits = 2)), "%")
lbls5 <- paste(names(table(data2$education)), (round(table(data2$education)/sum*100, digits = 2)), "%")
lbls6 <- paste(names(table(data2$urban)), (round(table(data2$urban)/sum*100, digits = 2)), "%")

levels((data2$importance))

if (!file.exists("./src/ref/ne_50m_admin_1_states_provinces_lakes/ne_50m_admin_1_states_provinces_lakes.dbf")){
    download.file(file.path('http://www.naturalearthdata.com/http/',
                            'www.naturalearthdata.com/download/50m/cultural',
                            'ne_50m_admin_1_states_provinces_lakes.zip'), 
                  f <- tempfile())
    unzip(f, exdir = "./src/ref/ne_50m_admin_1_states_provinces_lakes")
    rm(f)
}

region <- readOGR("./src/ref/ne_50m_admin_1_states_provinces_lakes", 'ne_50m_admin_1_states_provinces_lakes', encoding='UTF-8')
levels(region$name)[72] <- "Quebec"
levels(region$name)[72]

ui <- fluidPage(
    titlePanel("2011 Canadian National Election Study, With Attitude Toward Abortion"),
    sidebarLayout(
        sidebarPanel(
            selectInput("inCategory", "Choose a category for pie chart:", choices = colnames(data2)),
            selectInput("inGroup", "Choose a group for pie chart:", choices = NULL),
            selectInput("inProvince", "Province for Map and Table:", choices = data$province)
        ),
        mainPanel(
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Pie chart results", plotOutput("overallChart")),
                        tabPanel("Map of Provinces", leafletOutput(outputId = "mymap")),
                        tabPanel("Results for particular province", tableOutput("provinceTable")))
        )
    )
)

server <- function(input, output, session) {
    
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
                             setView(lng = subset(region, name == input$inProvince, select =longitude)$longitude,
                                     lat = subset(region, name == input$inProvince, select =latitude)$latitude, zoom =4) %>% 
                             addPolygons(data = subset(region, name %in% c(input$inProvince)), 
                                         fillColor = topo.colors(10, alpha = NULL), weight = 1)})
                 }
    )
    
    
    observeEvent(input$inCategory, 
        if(input$inCategory == 'overall'){
            output$overallChart <- renderPlot({
                pie(table(data2$overall), labels = lbls, main =paste0("Category: ",input$inCategory,"\n Should abortion be banned?"))
            })
        }
        else if(input$inCategory == 'province'){
            output$overallChart <- renderPlot({
                pie(table(data2$province), labels = lbls2, main =paste0("Category: ",input$inCategory,"\n Participants per province"))
            })
            observe({
            updateSelectInput(session, "inGroup",
                              choices = c("NONE", levels((data2$province))),
                              selected = "NONE"
                              )
            })
            observeEvent(input$inGroup,
                         if(input$inGroup == 'Alberta'){
                             output$overallChart <- renderPlot({
                                 pie(table(data2$overall), labels = lbls, main =paste0("Category: ",input$inCategory,"\n Should abortion be banned?"))
                             })
                         }
            )
            
        }
        else if(input$inCategory == 'gender'){
            output$overallChart <- renderPlot({
                pie(table(data2$gender), labels = lbls3, main =paste0("Category: ",input$inCategory,"\n Participants per gender"))
            })
        }
        else if(input$inCategory == 'importance'){
            output$overallChart <- renderPlot({
                pie(table(data2$importance), labels = lbls4, main =paste0("Category: ",input$inCategory,"\n Is the subject of abortion important?"))
            })
        }
        else if(input$inCategory == 'education'){
            output$overallChart <- renderPlot({
                pie(table(data2$education), labels = lbls5, main =paste0("Category: ",input$inCategory,"\n Participants per education"))
            })
        }
        else if(input$inCategory == 'urban'){
            output$overallChart <- renderPlot({
                pie(table(data2$urban), labels = lbls6, main =paste0("Category: area\n Participants per area"))
            })
        }
    )
}

shinyApp(ui = ui, server = server)
