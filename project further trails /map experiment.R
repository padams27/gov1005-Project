#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)
library(gt)
library(broom)
library(readxl)
library(janitor)
library(sf)
library(rgdal)
library(jsonlite)
library(httr)
library(leaflet)
library(spdplyr)


# load in all relevant libraries and the two RDS files needed

demographics <- readRDS("demographics.RDS")
ukMap <- readRDS("ukMap.RDS")

ui <- navbarPage("Analysing Brexit",
                 theme = shinytheme("flatly"),
                 
    
                
                tabPanel("UK Visualisation",
                         tabPanel("UK Plots",
                                 br(),
                                 mainPanel(
                                     leafletOutput("map", width = "100%", height = "100%"),
                                     absolutePanel(top = 10, right = 10,
                                     h4("About"),
                                     p("This overlayed map plot helps visualise how the variables are distributed around the UK."),
                                     
                                     helpText("Choose a varibale to view over the map"),
                                     selectInput(inputId = "map",
                                                 label = "Variable:",
                                                 choices = c("Percent Leave" = "pct_leave",
                                                             "Age: Over 50" = "over50",
                                                             "Age: Under 50" = "under50",
                                                             "Education",
                                                             "Unemployment" = "Unemployed"),
                                                 selected = "Percent Leave")),
              
                            ))))
                 


server <- function(input, output, session) {
    

    
    # the map visual is done similar to the above gt with an if
    # for each variable. it seemed easier and less stressful even 
    # if it may be more lines of code. I chose to put the full leaflet 
    # map in instead of .png even though it takes a while to load because 
    # this way you can see individual stats for each region by 
    # scrolling over
    
    # for some reason when I publish the app this leaflet plot crashes 
    # the app so I'm putting in screenshots instead
    
    output$map <- renderLeaflet({
        
        leaflet(ukMap) %>% 
            addTiles() %>% 
            setView(lng = 53.50, lat = 2.30, zoom = 12)
        
    })
            
        
        
    observe({
        
        pal <- colorNumeric("viridis", NULL)
        
        leafletProxy("map", data = ukMap) %>%
            clearShapes() %>%
            addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                         label = labels, fillColor = ~ pal(input$map)) 
            
    })
 

}

# Run the application 
shinyApp(ui = ui, server = server)
