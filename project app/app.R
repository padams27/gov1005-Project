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
                 
        # ABOUT tab introducing project
                 
                 tabPanel("About",
                          
                          imageOutput("brexit", width = "100%", height = "100%"),
                          br(),
                          h2("How do the 2016 referendum results correlate with other national statistics?", align = "center"),
                          br(),
                          div(),
                          br(),
                          fluidRow(column(2), column(8,
                                        h4(strong("Introduction")),          
                                        p("The aim of my project is to explore the relationship between the 2016 UK 'Brexit' referendum data and other data points 
                                                        relating the region where the votes were cast. The point of this is to try and explore any correlation
                                                        between the regional vote leave percentage, and a selection of data points taken from the 2011 national 
                                                        census."),
                                                        
                                                        p("The controversial vote led to a few stereotypes about the supposed voter base chosing to leave. The media 
                                                        and other sources portrayed those voting to leave as older, less educated citizens. It was generally 
                                                        thought that unemployment and the urban / rural divide were also contibuting factors."),
                                                    
                                                        p("By analysing the data I have I hope to see if there are significant correlations and whether or not these 
                                                        stereotypes have a strong basis."),
                                                     
                                                     
                          ))),
                 
        # PLOT tab with corellations and regression analysis
             
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
                                
        # main panel has two subset tabs for plot or gt tables
                                
                                tabsetPanel(id = "tabsMain",
                                            tabPanel("Plots",
                                                     br(),
                                                     plotOutput("demographics"),
                                                     br(),
                                                     br(),
                                            ),
                                            tabPanel("Models",
                                                     br(),
                                                     gt_output("model"),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     
                                ))),

            h4(strong("Analysis")),
            br(),
            h4("Plots"),
            p("The graphs show the where each region lies when relating the dependent and 
              independent variables. This rough guide shows how a change in demographic proportions 
              of the voting populaiton in that region potentially influences overal percent 
              leave vote. Although not causational, we can see that some of the stereotypes 
              around the populaiton voting to leave Europe do have some grounding."),
            p("There is a strong negative correlation between education and 'vote leave' with a coeffcient
              of -0.72, suggesting that the more educated an individual is the more likely it is 
              that they would vote to remain. Being from a rural populaiton seems to also make one 
              less likely to vote to leave, unlike an increase in age. So far this seems to 
              fulfill the stereotypes. Unemployment, however, which is more often associated
              with vote leave doesn't seem to have much correlation at all"),
        
            h4("Models"),
            p("To further analyse this data I have ran some regression models. 
                The above models show the regression coefficients for the given single 
                variable to the vote leave percentage. In these models, an increse in one point of the 
                variable will have an avergae treatment affect of displayed coefficient 
                on the slope of the regression from the intercept."),
            p("Again some of the variables have much more obvious relationships with the 
              vote leave percentage. Overall it would seem that most of the 'stereotypes' 
              have a reasonable grounding, but not all. The idea that being rural makes you more
              likely to vote leave seems false and there may be many reasons behind that."),
        )),
              
        # MAP visuals using leaflet    
                
                tabPanel("UK Visualisation",
                         tabPanel("UK Plots",
                                 br(),
                                 sidebarPanel(
                                     h4("About"),
                                     p("This overlayed map plot helps visualise how the variables are distributed around the UK."),
                                     
                                     helpText("Choose a varibale to view over the map"),
                                     selectInput(inputId = "map",
                                                 label = "Variable:",
                                                 choices = c("Percent Leave" = "pct_leave",
                                                             "Age: Over 50" = "over50",
                                                             "Age: Under 50" = "under50",
                                                             #"Population Density" = "Density",
                                                             "Education",
                                                             "Unemployment" = "Unemployed"),
                                                 selected = "Percent Leave")),
                                         
                                mainPanel(
                                    #p("Disclaimer: This takes a few seconds to load."),
                                    br(),
                                    imageOutput("ukMap")

                            ))),
                 
        # I have commented out the two lines above because they arent needed since I
        # got rid of leaflet output
        
        # FOOTNOTES and references 

                 tabPanel("Footnotes",
                          
                          
                          h4("References"),
                          br(),
                          p(paste("I obtained all my data from the UK goverment and their Brexit and 2011 national census data. The 
                          shapefiles I used for my mapping data are also from the same source and they are all free and easy to 
                          download:"), a(href = "https://www.ons.gov.uk/", "Office for National Statistics")),
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
                            Integrative Biology. My email is padams@college.harvard.edu. You can access the source
                            code for the project at my"), a(href = "https://github.com/padams27/gov1005-Project", "Github.")),
                          

                          ))


server <- function(input, output, session) {
    
    # load in image for the fist page     
    
    output$brexit <- renderImage({
        
        list(src = "graphics/brexit.jpg",
             height = 300,
             width = 700,
             style = "display: block; margin-left: auto; margin-right: auto;")},
        deleteFile = FALSE
    )


    # if / if else loop takes the ui input and selects relevant columns to take values from 
    # allowing for the use of a drop down selection. by creating these variables I can then
    # use them further down
    
    output$demographics <- renderPlot({
        
        
            if(input$demographic == "over50") {
                x_value <- demographics$over50
                x_lab <- "Population Over 50"
                demographic_title <- "Proportion of Population Over 50 and Percent Leave"
                cor <- "Correlation coefficient: 0.38"
            } 
            else if(input$demographic == "under50") {
                x_value <- demographics$under50
                x_lab <- "Population Under 50"
                demographic_title <- "Proportion of Population Under 50 and Percent Leave"
                cor <- "Correlation coefficient: -0.46"
            } 
            else if(input$demographic == "Education") {
                x_value <- demographics$Education
                x_lab <- "Higher Education"
                demographic_title <- "Proportion of Regional Population With A-Levels or Above and Percent Leave"
                cor <- "Correlation coefficient: -0.72"
            }
            else if(input$demographic == "Density") {
                x_value <- demographics$Density
                x_lab <- "Density (Persons per Hectare)"
                demographic_title <- "Population Density and Percent Leave"
                cor <- "Correlation coefficient: -0.407"
            }
            else{
                x_value <- demographics$Unemployed
                x_lab <- "Unemployment"
                demographic_title <- "Proportion of Population Unemployed and Percent Leave"
                cor <- "Correlation coefficient: 0.087"
            }
        
        # ggplot using my created variables
        
        ggplot(demographics, aes(x_value, pct_leave)) +
            geom_point(color = "lightblue") +
            geom_smooth(se = F, color = "darkgreen") +
            scale_x_continuous(labels = function(x) paste0(x, "%")) +
            scale_y_continuous(labels = function(y) paste0(y, "%")) +
            labs(y = "Percentage vote leave",
                 x = x_lab,
                 title = demographic_title,
                 subtitle = cor) +
            theme_classic()
        
    })

    # for the gt regression tables I wanted more flexibilty for changing 
    # titles etc so each plot is in its own if clasue to make changes 
    # between them
    
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
    
    # the map visual is done similar to the above gt with an if
    # for each variable. it seemed easier and less stressful even 
    # if it may be more lines of code. I chose to put the full leaflet 
    # map in instead of .png even though it takes a while to load because 
    # this way you can see individual stats for each region by 
    # scrolling over
    
    # for some reason when I publish the app this leaflet plot crashes 
    # the app so I'm putting in screenshots instead
    
#    output$ukMap <- renderLeaflet({
#        
#        
#        if(input$map == "over50"){
#            
#            pal <- colorNumeric("viridis", NULL)
#           
#            labels <- paste(ukMap$area, ukMap$over50, sep = ":  ")
#
#            leaflet(ukMap) %>%
#                addTiles() %>%
#                addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
#                            label = labels, fillColor = ~ pal(over50)) %>%
#                addLegend(pal = pal, values = ~ over50, opacity = 1.0)
#        }
#        else if(input$map == "under50"){
#            
#            pal <- colorNumeric("viridis", NULL)
# 
#            labels <- paste(ukMap$area, ukMap$under50, sep = ":  ")
#
#            leaflet(ukMap) %>%
#                addTiles() %>%
#                addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
#                            label = labels, fillColor = ~ pal(under50)) %>%
#                addLegend(pal = pal, values = ~ under50, opacity = 1.0)
#        }
#        else if(input$map == "Education"){
#            
#            pal <- colorNumeric("viridis", NULL)
# 
#            labels <- paste(ukMap$area, ukMap$Education, sep = ":  ")
#            
#            leaflet(ukMap) %>%
#                addTiles() %>%
#                addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
#                            label = labels, fillColor = ~ pal(Education)) %>%
#                addLegend(pal = pal, values = ~ Education, opacity = 1.0)            
#        }
#        else if(input$map == "Density"){
#            
#            pal <- colorNumeric("viridis", NULL)
#            
#            labels <- paste(ukMap$area, ukMap$Density, sep = ":  ")
#            
#            leaflet(ukMap) %>%
#                addTiles() %>%
#                addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
#                            label = labels, fillColor = ~ pal(Density)) %>%
#                addLegend(pal = pal, values = ~ Density, opacity = 1.0) 
#        }
#        else if(input$map == "pct_leave"){
#            
#            pal <- colorNumeric("viridis", NULL)
#            
#            labels <- paste(ukMap$area, ukMap$pct_leave, sep = ":  ")
#            
#            leaflet(ukMap) %>%
#                addTiles() %>%
#                addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
#                            label = labels, fillColor = ~ pal(pct_leave)) %>%
#                addLegend(pal = pal, values = ~ pct_leave, opacity = 1.0) 
#        }
#        else{input$Unemployed
#
#            pal <- colorNumeric("viridis", NULL)
#       
#            labels <- paste(ukMap$area, ukMap$Density, sep = ":  ")
#            
#            leaflet(ukMap) %>%
#                addTiles() %>%
#                addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
#                            label = labels, fillColor = ~ pal(Unemployed)) %>%
#                addLegend(pal = pal, values = ~ Unemployed, opacity = 1.0)             
#            }
#    })
 
    
    # map screenshots instead...
    
    
    output$ukMap <- renderImage({
        
        if(input$map == "over50"){ 
            list(src = "graphics/over50.png",
                 height = 500,
                 width = 700,
                 style = "display: block; margin-left: auto; margin-right: auto;")
        }
        else if(input$map == "under50"){ 
            list(src = "graphics/under50.png",
                 height = 500,
                 width = 700,
                 style = "display: block; margin-left: auto; margin-right: auto;")
        }
        else if(input$map == "Education"){ 
            list(src = "graphics/education.png",
                 height = 500,
                 width = 700,
                 style = "display: block; margin-left: auto; margin-right: auto;")
        }
        else if(input$map == "pct_leave"){ 
            list(src = "graphics/pct_leave.png",
                 height = 500,
                 width = 700,
                 style = "display: block; margin-left: auto; margin-right: auto;")
        }
        else if(input$map == "Unemployed"){ 
            list(src = "graphics/unemployed.png",
                 height = 500,
                 width = 700,
                 style = "display: block; margin-left: auto; margin-right: auto;")
        }
        
        
        
    },    
         deleteFile = FALSE
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
