library(shiny)
library(dplyr)
library(lubridate)
library(hms)
source('.\\Scripts\\general_helpers.R')

server <- function(input, output){
  
  dataSet <- read_csv(file = ".\\Data\\brdata_cleaned.csv")
  dataSet <- mutate(dataSet, offense_date = ymd(offense_date),
                      offense_time = as.hms(offense_time))
  
  currentWeekData <- eventReactive(input$initialFilterBtn, 
                                   {
                                     FilterData(dataSet, startDate = as.Date(input$initialDateRange[1], format = "%Y-%m-%d"), 
                                                endDate = as.Date(input$initialDateRange[2], format = "%Y-%m-%d"))
                                   })
  
  previousWeekData <- eventReactive(input$initialFilterBtn, 
                                    {
                                      FilterData(dataSet, startDate = as.Date(input$initialDateRange[1], format = "%Y-%m-%d") - 7, 
                                                 endDate = as.Date(input$initialDateRange[2], format = "%Y-%m-%d") - 7)
                                    })
  
  output$testText1 <- renderText(as.character(nrow(currentWeekData())))
  output$testText2 <- renderText(as.character(nrow(previousWeekData())))
  
}