library(shiny)
library(dplyr)
library(lubridate)
library(hms)
source('.\\Scripts\\general_helpers.R')
source('.\\Scripts\\overview_view.R')

server <- function(input, output){
  
  # APP LEVEL DEPENDENCIES
  
  #Read the dataset containing the crime data, load the Baton Rouge map
  dataSet <- read_csv(file = ".\\Data\\brdata_cleaned.csv")
  dataSet <- mutate(dataSet, offense_date = ymd(offense_date),
                      offense_time = as.hms(offense_time))
  
  brMap <- qmap(location = "baton rouge", zoom = 12)
  
  # INITIAL OVERVIEW SECTION
  
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
  
  output$testText1 <- renderText(as.character(nrow(currentWeekData())))
  output$testText2 <- renderText(as.character(nrow(previousWeekData())))
  output$testText3 <- renderText(as.character(nrow(overviewData())))  
  
  # CRIME DEEP DIVE SECTION
  
  #Filter data according to specific crime deep dive
  currentWkCrime <- eventReactive(input$crimeFilterBtn,
                                  {
                                    FilterData(currentWeekData(), offenseCategory = input$crimeSelector)
                                  })
  
  previousWkCrime <- eventReactive(input$crimeFilterBtn,
                                   {
                                    FilterData(previousWeekData(), offenseCategory = input$crimeSelector) 
                                   })
  
  
  #Create the trend comparision graph
  trendData <- eventReactive(input$crimeFilterBtn,
                             {
                               TrendGraphDataGen(currentWkCrime(), previousWkCrime())
                             })
  
  trendGraph <- eventReactive(input$crimeFilterBtn,
                              {
                                TrendGraphGen(trendData())
                              })
  output$trendPlot <- renderPlot(trendGraph())
  
  #Create the heatmap graph
  heatMap <- eventReactive(input$crimeFilterBtn,
                           {
                             MapGraphGen(currentWkCrime(), brMap)
                           })
  output$mapPlot <- renderPlot(heatMap())
  
  
}