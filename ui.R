library(shiny)
library(readr)

ui <- fluidPage(
  
  titlePanel("Period Overview"),
  
  hr(),
  
  fluidRow(
    
    #NOTE: CURRENTLY HARDCODED, MAKE PROGRAMMATIC ACCORDING TO DATA
    column(4,
           
           wellPanel(
             dateRangeInput("initialDateRange", "Analysis Period:",
                            start = "2011-01-01",
                            end = "2017-09-20",
                            format = "yyyy-mm-dd"),
             
             actionButton(inputId = "initialFilterBtn", label = "Filter")
             )
           ),
    
    column(8, 
           textOutput("testText1"),
           textOutput("testText2")
           )
    
  ),
  
  hr(),
  
  fluidRow(
    
    column(4, h1("Input PlaceHolder")),
    
    column(8, h1("Output Placeholder"))
    
  )
  
)