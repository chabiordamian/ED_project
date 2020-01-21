library(shiny)
require(ggplot2)
require(dplyr)
require(readr)

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

sum <- table(data2$overall)[1] + table(data2$overall)[2]
sum <- as.numeric(sum)

#pie(table(data2$overall), labels = lbls, main ="Should abortion be banned?")
lbls <- paste(names(table(data2$overall)), (round(table(data2$overall)/sum*100, digits = 2)), "%")
lbls2 <- paste(names(table(data2$province)), (round(table(data2$province)/sum*100, digits = 2)), "%")
lbls3 <- paste(names(table(data2$gender)), (round(table(data2$gender)/sum*100, digits = 2)), "%")
lbls4 <- paste(names(table(data2$importance)), (round(table(data2$importance)/sum*100, digits = 2)), "%")
lbls5 <- paste(names(table(data2$education)), (round(table(data2$education)/sum*100, digits = 2)), "%")
lbls6 <- paste(names(table(data2$urban)), (round(table(data2$urban)/sum*100, digits = 2)), "%")

gender_data <- subset(data2, select=c(overall, gender))
female = gender_data %>% filter(., gender == 'Female') 
female <- table(female$overall)
female_lbls <- paste(names(female), "\n", round(female/sum(female)*100, digits = 2), "%", sep="")

male = gender_data %>% filter(., gender == 'Male') 
male <- table(male$overall)
male_lbls <- paste(names(male), "\n", round(male/sum(male)*100, digits = 2), "%", sep="")

importance_data <- subset(data2, select=c(overall, importance))
not = importance_data %>% filter(., importance == 'not') 
not <- table(not$overall)
not_lbls <- paste(names(not), "\n", round(not/sum(not)*100, digits = 2), "%", sep="")

notvery = importance_data %>% filter(., importance == 'notvery') 
notvery <- table(notvery$overall)
notvery_lbls <- paste(names(notvery), "\n", round(notvery/sum(notvery)*100, digits = 2), "%", sep="")

somewhat = importance_data %>% filter(., importance == 'somewhat') 
somewhat <- table(somewhat$overall)
somewhat_lbls <- paste(names(somewhat), "\n", round(somewhat/sum(somewhat)*100, digits = 2), "%", sep="")

very = importance_data %>% filter(., importance == 'very') 
very <- table(very$overall)
very_lbls <- paste(names(very), "\n", round(very/sum(very)*100, digits = 2), "%", sep="")

urban_data <- subset(data2, select=c(overall, urban))

rural = urban_data %>% filter(., urban == 'rural') 
rural <- table(rural$overall)
rural_lbls <- paste(names(rural), "\n", round(rural/sum(rural)*100, digits = 2), "%", sep="")

urban = urban_data %>% filter(., urban == 'urban') 
urban <- table(urban$overall)
urban_lbls <- paste(names(urban), "\n", round(urban/sum(urban)*100, digits = 2), "%", sep="")

education_data <- subset(data2, select=c(overall, education))

college = education_data %>% filter(., education == 'college') 
college <- table(college$overall)
college_lbls <- paste(names(college), "\n", round(college/sum(college)*100, digits = 2), "%", sep="")

higher = education_data %>% filter(., education == 'higher') 
higher <- table(higher$overall)
higher_lbls <- paste(names(higher), "\n", round(higher/sum(higher)*100, digits = 2), "%", sep="")

bachelors = education_data %>% filter(., education == 'bachelors') 
bachelors <- table(bachelors$overall)
bachelors_lbls <- paste(names(bachelors), "\n", round(bachelors/sum(bachelors)*100, digits = 2), "%", sep="")

HS = education_data %>% filter(., education == 'HS') 
HS <- table(HS$overall)
HS_lbls <- paste(names(HS), "\n", round(HS/sum(HS)*100, digits = 2), "%", sep="")

somePS = education_data %>% filter(., education == 'somePS') 
somePS <- table(somePS$overall)
somePS_lbls <- paste(names(somePS), "\n", round(somePS/sum(somePS)*100, digits = 2), "%", sep="")

lessHS = education_data %>% filter(., education == 'lessHS') 
lessHS <- table(lessHS$overall)
lessHS_lbls <- paste(names(lessHS), "\n", round(lessHS/sum(lessHS)*100, digits = 2), "%", sep="")

province_data <- subset(data2, select=c(overall, province))
Alberta = province_data %>% filter(., province == 'Alberta') 
Alberta <- table(Alberta$overall)
Alberta_lbls <- paste(names(Alberta), "\n", round(Alberta/sum(Alberta)*100, digits = 2), "%", sep="")

British_Columbia = province_data %>% filter(., province == 'British Columbia') 
British_Columbia <- table(British_Columbia$overall)
British_Columbia_lbls <- paste(names(British_Columbia), "\n", round(British_Columbia/sum(British_Columbia)*100, digits = 2), "%", sep="")

Manitoba = province_data %>% filter(., province == 'Manitoba') 
Manitoba <- table(Manitoba$overall)
Manitoba_lbls <- paste(names(Manitoba), "\n", round(Manitoba/sum(Manitoba)*100, digits = 2), "%", sep="")

Newfoundland_and_Labrador = province_data %>% filter(., province == 'Newfoundland and Labrador') 
Newfoundland_and_Labrador <- table(Newfoundland_and_Labrador$overall)
Newfoundland_and_Labrador_lbls <- paste(names(Newfoundland_and_Labrador), "\n", round(Newfoundland_and_Labrador/sum(Newfoundland_and_Labrador)*100, digits = 2), "%", sep="")

Ontario = province_data %>% filter(., province == 'Ontario') 
Ontario <- table(Ontario$overall)
Ontario_lbls <- paste(names(Ontario), "\n", round(Ontario/sum(Ontario)*100, digits = 2), "%", sep="")

Prince_Edward_Island = province_data %>% filter(., province == 'Prince Edward Island') 
Prince_Edward_Island <- table(Prince_Edward_Island$overall)
Prince_Edward_Island_lbls <- paste(names(Prince_Edward_Island), "\n", round(Prince_Edward_Island/sum(Prince_Edward_Island)*100, digits = 2), "%", sep="")

New_Brunswick = province_data %>% filter(., province == 'New Brunswick') 
New_Brunswick <- table(New_Brunswick$overall)
New_Brunswick_lbls <- paste(names(New_Brunswick), "\n", round(New_Brunswick/sum(New_Brunswick)*100, digits = 2), "%", sep="")

Nova_Scotia = province_data %>% filter(., province == 'Nova Scotia') 
Nova_Scotia <- table(Nova_Scotia$overall)
Nova_Scotia_lbls <- paste(names(Nova_Scotia), "\n", round(Nova_Scotia/sum(Nova_Scotia)*100, digits = 2), "%", sep="")

Quebec = province_data %>% filter(., province == 'Quebec') 
Quebec <- table(Quebec$overall)
Quebec_lbls <- paste(names(Quebec), "\n", round(Quebec/sum(Quebec)*100, digits = 2), "%", sep="")

Saskatchewan = province_data %>% filter(., province == 'Saskatchewan') 
Saskatchewan <- table(Saskatchewan$overall)
Saskatchewan_lbls <- paste(names(Saskatchewan), "\n", round(Saskatchewan/sum(Saskatchewan)*100, digits = 2), "%", sep="")

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
                     observe({
                         updateSelectInput(session, "inGroup",
                                           choices = c("NONE", levels((data2$province))),
                                           selected = "NONE"
                         )
                     })
                     observeEvent(input$inGroup,
                                  if(input$inGroup == 'Alberta'){
                                      output$overallChart <- renderPlot({
                                          pie(Alberta, labels = Alberta_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                      })
                                  }
                                  else if(input$inGroup == 'NONE'){    
                                      output$overallChart <- renderPlot({
                                          pie(table(data2$province), labels = lbls2, main =paste0("Category: ",input$inCategory,"\n Participants per province"))
                                      })
                                  }
                                  else if(input$inGroup == 'British Columbia'){    
                                      output$overallChart <- renderPlot({
                                          pie(British_Columbia, labels = British_Columbia_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                          
                                      })
                                  }
                                  else if(input$inGroup == 'Newfoundland and Labrador'){    
                                      output$overallChart <- renderPlot({
                                          pie(Newfoundland_and_Labrador, labels = Newfoundland_and_Labrador_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                          
                                      })
                                  }
                                  else if(input$inGroup == 'Prince Edward Island'){    
                                      output$overallChart <- renderPlot({
                                          pie(Prince_Edward_Island, labels = Prince_Edward_Island_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                          
                                      })
                                  }
                                  else if(input$inGroup == 'New Brunswick'){    
                                      output$overallChart <- renderPlot({
                                          pie(New_Brunswick, labels = New_Brunswick_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                          
                                      })
                                  }
                                  else if(input$inGroup == 'Manitoba'){    
                                      output$overallChart <- renderPlot({
                                          pie(Manitoba, labels = Manitoba_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                          
                                      })
                                  }
                                  else if(input$inGroup == 'Nova Scotia'){    
                                      output$overallChart <- renderPlot({
                                          pie(Nova_Scotia, labels = Nova_Scotia_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                          
                                      })
                                  }
                                  else if(input$inGroup == 'Quebec'){    
                                      output$overallChart <- renderPlot({
                                          pie(Quebec, labels = Quebec_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                          
                                      })
                                  }
                                  else if(input$inGroup == 'Ontario'){    
                                      output$overallChart <- renderPlot({
                                          pie(Ontario, labels = Ontario_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                          
                                      })
                                  }
                                  else if(input$inGroup == 'Saskatchewan'){    
                                      output$overallChart <- renderPlot({
                                          pie(Saskatchewan, labels = Saskatchewan_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                          
                                      })
                                  }
                     )
                     
                 }
                 else if(input$inCategory == 'gender'){
                     observe({
                         updateSelectInput(session, "inGroup",
                                           choices = c("NONE", levels((data2$gender))),
                                           selected = "NONE"
                         )
                     })
                     observeEvent(input$inGroup,
                                  if(input$inGroup == 'NONE'){
                                      output$overallChart <- renderPlot({
                                          pie(table(data2$gender), labels = lbls3, main =paste0("Category: ",input$inCategory,"\n Participants per gender"))
                                      })
                                  }
                                  else if(input$inGroup == 'Female'){
                                      output$overallChart <- renderPlot({
                                          pie(female, labels = female_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                      })
                                  }
                                  else if(input$inGroup == 'Male'){
                                      output$overallChart <- renderPlot({
                                          pie(male, labels = male_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                      })
                                  }
                     )
                 }
                 else if(input$inCategory == 'importance'){
                     observe({
                         updateSelectInput(session, "inGroup",
                                           choices = c("NONE", levels((data2$importance))),
                                           selected = "NONE"
                         )
                     })
                     observeEvent(input$inGroup,
                                  if(input$inGroup == 'NONE'){
                                      output$overallChart <- renderPlot({
                                          pie(table(data2$importance), labels = lbls4, main =paste0("Category: ",input$inCategory,"\n Is the subject of abortion important?"))
                                      })
                                  }
                                  else if(input$inGroup == 'not'){
                                      output$overallChart <- renderPlot({
                                          pie(not, labels = not_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                       })
                                  }
                                  else if(input$inGroup == 'notvery'){
                                      output$overallChart <- renderPlot({
                                          pie(notvery, labels = notvery_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                      })
                                  }
                                  else if(input$inGroup == 'somewhat'){
                                      output$overallChart <- renderPlot({
                                          pie(somewhat, labels = somewhat_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                      })
                                  }
                                  else if(input$inGroup == 'very'){
                                      output$overallChart <- renderPlot({
                                          pie(very, labels = very_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                    })
                                  }
                     )
                 }
                 else if(input$inCategory == 'education'){
                     observe({
                         updateSelectInput(session, "inGroup",
                                           choices = c("NONE", levels((data2$education))),
                                           selected = "NONE"
                         )
                     })
                     observeEvent(input$inGroup,
                                  if(input$inGroup == 'NONE'){
                                      output$overallChart <- renderPlot({
                                          pie(table(data2$education), labels = lbls5, main =paste0("Category: ",input$inCategory,"\n Participants per education"))
                                      })
                                  }
                                  else if(input$inGroup == 'bachelors'){
                                      output$overallChart <- renderPlot({
                                          pie(bachelors, labels = bachelors_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                          
                                      })
                                  }
                                  else if(input$inGroup == 'somePS'){
                                      output$overallChart <- renderPlot({
                                          pie(somePS, labels = somePS_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                          
                                      })
                                  }
                                  else if(input$inGroup == 'lessHS'){
                                      output$overallChart <- renderPlot({
                                          pie(lessHS, labels = lessHS_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                          
                                      })
                                  }
                                  else if(input$inGroup == 'HS'){
                                      output$overallChart <- renderPlot({
                                          pie(HS, labels = HS_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                          
                                      })
                                  }
                                  else if(input$inGroup == 'higher'){
                                      output$overallChart <- renderPlot({
                                          pie(higher, labels = higher_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                       })
                                  }
                                  else if(input$inGroup == 'college'){
                                      output$overallChart <- renderPlot({
                                          pie(college, labels = college_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                      })
                                  }
                     )
                     
                 }
                 else if(input$inCategory == 'urban'){
                     observe({
                         updateSelectInput(session, "inGroup",
                                           choices = c("NONE", levels((data2$urban))),
                                           selected = "NONE"
                         )
                     })
                     observeEvent(input$inGroup,
                                  if(input$inGroup == 'NONE'){
                                      output$overallChart <- renderPlot({
                                          pie(table(data2$urban), labels = lbls6, main =paste0("Category: area\n Participants per area"))
                                      })
                                  }
                                  else if(input$inGroup == 'urban'){
                                      output$overallChart <- renderPlot({
                                          pie(urban, labels = urban_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                      })
                                  }
                                  else if(input$inGroup == 'rural'){
                                      output$overallChart <- renderPlot({
                                          pie(rural, labels = rural_lbls, main =paste0("Category: ",input$inCategory,"\n", "Group: ",input$inGroup,"\n", "Should abortion be banned?"))
                                      })
                                  }
                     )
                 }
    )
}

shinyApp(ui = ui, server = server)