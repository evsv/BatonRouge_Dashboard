library(shiny)
library(dplyr)
library(lubridate)
library(hms)
source('.\\Scripts\\general_helpers.R')
source('.\\Scripts\\overview_view.R')

server <- function(input, output){
  
  #Read the dataset containing the crime data
  dataSet <- read_csv(file = ".\\Data\\brdata_cleaned.csv")
  dataSet <- mutate(dataSet, offense_date = ymd(offense_date),
                      offense_time = as.hms(offense_time))
  
  #Filter data according the date range input
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
  
  #Create the crime comparison overview
  overviewData <- eventReactive(input$initialFilterBtn, 
                                {
                                  OverviewGraphDataGen(currentWeekData(), previousWeekData())
                                })
  
  overviewGraph <- eventReactive(input$initialFilterBtn,
                                 {
                                   OverviewGraphGen(overviewData())
                                 })
  
  output$overviewPlot <- renderPlot(overviewGraph())
  
  #Filter data according to specific crime deep dive
  
  
  output$testText1 <- renderText(as.character(nrow(currentWeekData())))
  output$testText2 <- renderText(as.character(nrow(previousWeekData())))
  output$testText3 <- renderText(as.character(nrow(overviewData())))
}