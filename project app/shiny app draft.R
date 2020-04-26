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




demographics <- readRDS("demographics.RDS")
ukMap <- readRDS("ukMap.RDS")

ui <- navbarPage("Analysing Brexit",
                 theme = shinytheme("flatly"),
                 
                 #### ABOUT
                 
                 tabPanel("About",
                          
                          imageOutput("brexit", width = "100%", height = "100%"),
                          br(),
                          h2("How do the 2016 referendum results correlate with other national statistics?", align = "center"),
                          br(),
                          div(),
                          
                          
                          br(),
                          
                          fluidRow(column(2), column(8,
                                                     
                                        h4(strong("Introduction")),          
                                                     
                                        #text to introduce project
                                                     
                                        p("The aim of my project is to explore the relationship between the 2016 UK 'Brexit' referendum data and other data points 
                                                        relating the region where the votes were cast. The point of this is to try and explore any correlation
                                                        between the regional vote leave percentage, and a selection of data points taken from the 2011 national 
                                                        census."),
                                                        
                                                        p("The controversial vote led to a few stereotypes about the supposed voter base chosing to leave. The media 
                                                        and other sources portrayed those voting to leave as older, less educated citizens. It was generally 
                                                        thought that unemployment and the urban / rural divide were also contibuting factors."),
                                                    
                                                        p("By analysing the data I have I hope to see if there are significant correlations and whether or not these 
                                                        stereotypes have a strong basis."),
                                                     
                                        br(),
                                                     
                                                     
                          ))),
                 
                 
             #### DATA
             
                 tabPanel("Data Analysis",
                          tabPanel("Graphics",
                                      
                            br(),
                            
                            sidebarPanel(
                              h4("About"),
                              p("These plots show a brief overview of the relationships between 
                                the vote leave percentage (by local authority) and other regional
                                data points from the 2011 national census."),
                              
                              helpText("Choose a varibale to view corellation plot"),
                              selectInput(inputId = "demographic",
                                          label = "Variable:",
                                          choices = c("Age: Over 50" = "over50",
                                                      "Age: Under 50" = "under50",
                                                      "Population Density" = "Density",
                                                      "Education",
                                                      "Unemployment" = "Unemployed"),
                                          selected = "Age: Over 50")),
                                   
                            mainPanel(
                                tabsetPanel(id = "tabsMain",
                                            tabPanel("Plots",
                                                     br(),
                                                     plotOutput("demographics"),
                                                     br(),
                                                     p("The above plots show the broad correlation between the two data points.
                                                       There is not necessarily any causation, this is just observational and 
                                                       open to interpretation."),
                                                     br(),
                                                     p("Correlation coefficeints:"),
                                                     br(),
                                                     p("Over 50:  0.38"),
                                                     p("Under 50:  -0.46"),
                                                     p("Population Density:  -0.407"),
                                                     p("Unemployment"),
                                                     p("Education:  -0.72"),
                                            ),
                                            tabPanel("Models",
                                                     br(),
                                                     gt_output("model"),
                                                     br(),
                                                     br(),
                                                     p(paste("The above model shows the regression coefficients for the given single 
                                                             variable to the vote leave percentage. An increse in one point of the 
                                                             variable will have an avergae treatment affect of displayed coefficient 
                                                             on the slope of the regression from the intercept.")),
                                
                          ))))),
                 
                tabPanel("National Values",
                         tabPanel("UK Plots",
                                 h4("Put screenshots of spatial data here"),
                                 sidebarPanel(
                                     h4("About"),
                                     p("These plots show a brief overview of the relationships between 
                                the vote leave percentage (by local authority) and other regional
                                data points from the 2011 national census."),
                                     
                                     helpText("Choose a varibale to view corellation plot"),
                                     selectInput(inputId = "map",
                                                 label = "Variable:",
                                                 choices = c("Age: Over 50" = "over50",
                                                             "Age: Under 50" = "under50",
                                                             "Population Density" = "Density",
                                                             "Education",
                                                             "Unemployment" = "Unemployed"),
                                                 selected = "Age: Over 50")),
                                         
                                mainPanel(
                                    leafletOutput("ukMap")

                            ))),
                 
            #### FOOTNOTES
                 
                 
                 #tab to explain where I got my data 
                 
                 tabPanel("Footnotes",
                          
                          
                          h4("References"),
                          br(),
                          
                          p(paste("I obtained my data from the UK goverment and their Brexit and 2011 national census data. 
                            Both are online and for public use:"), a(href = "https://www.ons.gov.uk/", "Office for National Statistics")),
                          br(),
                          h4("Data variables"),
                          
                          p("For my Brexit data point I chose the vote leave percentage, as it was the winning vote."),
                          p("Education data is the percentage of the population with two A-levels or higher."),
                          p("Age data points are the proportion either over 50 years old, or between voter age
                            and 50 (15-50, not strictly voter age but more inclusive)."),
                          p("'Density' is taken as People per hectare."),
                          p("Unemployment refers to the proportion of 'economically active: unemplyed' persons."),
                          br(),
                          
                          h4("About me"),
                          p(paste("My name is Paddy Adams and I'm a Sophmore student-athlete at Harvard studying 
                            Integrative Biology. You can access the source code for the project 
                            at my"), a(href = "https://github.com/padams27/gov1005-Project", "Github.")),
                          

                          ))


server <- function(input, output, session) {
    
    
    
    output$brexit <- renderImage({
        
        list(src = "brexit.jpg",
             height = 300,
             width = 700,
             style = "display: block; margin-left: auto; margin-right: auto;")},
        deleteFile = FALSE
    )

    
    #### DATA
    
    
    output$demographics <- renderPlot({
        
        
            if(input$demographic == "over50") {
                x_value <- demographics$over50
                x_lab <- "Population Over 50"
                demographic_title <- "Proportion of Population Over 50 and Percent Leave"
            } 
            else if(input$demographic == "under50") {
                x_value <- demographics$under50
                x_lab <- "Population Under 50"
                demographic_title <- "Proportion of Population Under 50 and Percent Leave"
            } 
            else if(input$demographic == "Education") {
                x_value <- demographics$Education
                x_lab <- "Higher Education"
                demographic_title <- "Proportion of Regional Population With A-Levels or Above and Percent Leave"
            }
            else if(input$demographic == "Density") {
                x_value <- demographics$Density
                x_lab <- "Density (Persons per Hectare)"
                demographic_title <- "Population Density and Percent Leave"
            }
            else{
                x_value <- demographics$Unemployed
                x_lab <- "Unemployment"
                demographic_title <- "Proportion of Population Unemployed and Percent Leave"
            }
        
        ggplot(demographics, aes(x_value, pct_leave)) +
            geom_point(color = "lightblue") +
            geom_smooth(se = F, color = "darkgreen") +
            scale_x_continuous(labels = function(x) paste0(x, "%")) +
            scale_y_continuous(labels = function(y) paste0(y, "%")) +
            labs(y = "Percentage vote leave",
                 x = x_lab,
                 title = demographic_title) +
            theme_classic()
        
    })

    output$model <- render_gt({
        
        if(input$demographic == "over50") {
            demographics %>% 
                lm(pct_leave ~ over50, .) %>% 
                tidy(conf.int = T) %>% 
                select(-c(std.error, p.value, statistic)) %>% 
                mutate(term = c("Intercept", "Over 50")) %>% 
                gt() %>% 
                tab_header(title = "Effect of the Variable on Leave vote") %>% 
                cols_label(term = "",
                           estimate = "Coefficient",
                           conf.low = "5th percentile",
                           conf.high = "95th percentile") %>%
                tab_spanner(label = "Confidence Interval",
                            columns = 3:4) %>% 
                fmt_number(columns = 2:4, decimals = 2)
        } 
        else if(input$demographic == "under50") {
            demographics %>% 
                lm(pct_leave ~ under50, .) %>% 
                tidy(conf.int = T) %>% 
                select(-c(std.error, p.value, statistic)) %>% 
                mutate(term = c("Intercept", "Under 50")) %>% 
                gt() %>% 
                tab_header(title = "Effect of the Variable on Leave vote") %>% 
                cols_label(term = "",
                           estimate = "Coefficient",
                           conf.low = "5th percentile",
                           conf.high = "95th percentile") %>%
                tab_spanner(label = "Confidence Interval",
                            columns = 3:4) %>% 
                fmt_number(columns = 2:4, decimals = 2)
        } 
        else if(input$demographic == "Education") {
            demographics %>% 
                lm(pct_leave ~ Education, .) %>% 
                tidy(conf.int = T) %>% 
                select(-c(std.error, p.value, statistic)) %>% 
                mutate(term = c("Intercept", "Higher Education")) %>% 
                gt() %>% 
                tab_header(title = "Effect of the Variable on Leave vote") %>% 
                cols_label(term = "",
                           estimate = "Coefficient",
                           conf.low = "5th percentile",
                           conf.high = "95th percentile") %>%
                tab_spanner(label = "Confidence Interval",
                            columns = 3:4) %>% 
                fmt_number(columns = 2:4, decimals = 2)  
        }
        else if(input$demographic == "Density") {
            demographics %>% 
                lm(pct_leave ~ Density, .) %>% 
                tidy(conf.int = T) %>% 
                select(-c(std.error, p.value, statistic)) %>% 
                mutate(term = c("Intercept", "Population Density")) %>% 
                gt() %>% 
                tab_header(title = "Effect of the Variable on Leave vote") %>% 
                cols_label(term = "",
                           estimate = "Coefficient",
                           conf.low = "5th percentile",
                           conf.high = "95th percentile") %>%
                tab_spanner(label = "Confidence Interval",
                            columns = 3:4) %>% 
                fmt_number(columns = 2:4, decimals = 2)
        }
        else{input$Unemployed
            demographics %>% 
                lm(pct_leave ~ Unemployed, .) %>% 
                tidy(conf.int = T) %>% 
                select(-c(std.error, p.value, statistic)) %>% 
                mutate(term = c("Intercept", "Unemployment")) %>% 
                gt() %>% 
                tab_header(title = "Effect of the Variable on Leave vote") %>% 
                cols_label(term = "",
                           estimate = "Coefficient",
                           conf.low = "5th percentile",
                           conf.high = "95th percentile") %>%
                tab_spanner(label = "Confidence Interval",
                            columns = 3:4) %>% 
                fmt_number(columns = 2:4, decimals = 2)
        }
    })  
    
    output$ukMap({
        
        if(input$map == "over50"){
            
            ## Set color palette, other options available
            pal <- colorNumeric("viridis", NULL)
            
            #set so tiles are labelled 
            labels <- paste(ukMap$area, ukMap$over50, sep = ":  ")
            
            ## Plot UK referendum data
            leaflet(ukMap) %>%
                addTiles() %>%
                addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                            label = labels, fillColor = ~ pal(over50)) %>%
                addLegend(pal = pal, values = ~ over50, opacity = 1.0)
        }
        else if(input$map == "under50"){
            
            pal <- colorNumeric("viridis", NULL)
 
            labels <- paste(ukMap$area, ukMap$under50, sep = ":  ")

            leaflet(ukMap) %>%
                addTiles() %>%
                addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                            label = labels, fillColor = ~ pal(under50)) %>%
                addLegend(pal = pal, values = ~ under50, opacity = 1.0)
        }
        else if(input$map == "Education"){
            
            pal <- colorNumeric("viridis", NULL)
 
            labels <- paste(ukMap$area, ukMap$Education, sep = ":  ")
            
            leaflet(ukMap) %>%
                addTiles() %>%
                addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                            label = labels, fillColor = ~ pal(Education)) %>%
                addLegend(pal = pal, values = ~ Education, opacity = 1.0)            
        }
        else if(input$map == "Density"){
            
            pal <- colorNumeric("viridis", NULL)
            
            labels <- paste(ukMap$area, ukMap$Density, sep = ":  ")
            
            leaflet(ukMap) %>%
                addTiles() %>%
                addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                            label = labels, fillColor = ~ pal(Density)) %>%
                addLegend(pal = pal, values = ~ Density, opacity = 1.0) 
        }
        else{input$Unemployed

            pal <- colorNumeric("viridis", NULL)
        
            labels <- paste(ukMap$area, ukMap$Density, sep = ":  ")
            
            leaflet(ukMap) %>%
                addTiles() %>%
                addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                            label = labels, fillColor = ~ pal(Unemployed)) %>%
                addLegend(pal = pal, values = ~ Unemployed, opacity = 1.0)             
            }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
