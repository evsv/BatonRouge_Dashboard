library(shiny)
library(readr)

ui <- fluidPage(
  
  titlePanel("Period Overview"),
  
  hr(),
  
  fluidRow(
    
    #NOTE: CURRENTLY HARDCODED, MAKE PROGRAMMATIC ACCORDING TO DATA
    column(2, align = "center",
           
           wellPanel(
             dateRangeInput("initialDateRange", "Analysis Period:",
                            start = "2017-09-13",
                            end = "2017-09-20",
                            format = "yyyy-mm-dd"),
             
             actionButton(inputId = "initialFilterBtn", label = "Filter")
             )
           ),
    
    column(8, align = "center",
           plotOutput("overviewPlot")
           )
    
  ),
  
  hr(),
  
  fluidRow(
    
    column(2, align = "center",
           
           wellPanel(
             selectInput("crimeSelector", "Select crime to deep dive into:",
                         choices = c("ASSAULT", "BATTERY", "BUSINESS ROBBERY",
                                     "CRIMINAL DAMAGE TO PROPERTY", "FIREARM", "HOMICIDE",
                                     "INDIVIDUAL ROBBERY", "JUVENILE", "NARCOTICS",
                                     "NON-RESIDENTIAL BURGLARY", "NUISANCE", "OTHER",
                                     "RESIDENTIAL BURGLARY", "SEXUAL ASSAULT", "THEFT",
                                      "VEHICLE BURGLARY", "VICE")
                         ),
             
             actionButton(inputId = "crimeFilterBtn", label = "Select Crime")
           )
          ),
    
    column(5, align = "center",
           plotOutput("trendPlot")
           ),
    column(5, align = "center", 
           plotOutput("mapPlot")
           )
    
  )
  
)