#===================================================================#
# THIS SCRIPT CONTAINS METHODS USED FOR THE OVERVIEW VIEW
#===================================================================#

library(dplyr)
library(ggplot2)

#-------------------------------------------------------------------#
# ValidFilter()
#
# data: dataframe. The dataframe on which filters are to be applied
# startYear: numeric. The first year to be included
# endYear: numeric. The final year to be included
# startMonth: numeric. The first month from the startYear to be 
#             included
# endMonth: numeric. The final month from the endYear to be included
# startDay: numeric. The first day from the startMonth of startYear
#           to be included
# endDay: numeric. The last day from the endMonth of endYear to be 
#         included
# zipCodes: char vector. Vector of zipcodes to be considered. Defaults
#           to all, in which case all zip codes are included
# filtertype: char. For which filter value is the valid range of 
#             filters being extracted
#
# Returns: acceptedRange, a vector of acceptable values for the 
#          
#-------------------------------------------------------------------#
ValidFilter <- function(data, startYear, endYear = c(), startMonth = c(), endMonth = c(),
                startDay = c(), endDay = c(), zipCodes = c(), filterType = c()){
  
  acceptedRange <- c()
  
  if(filterType == "endYear"){
    
    data <- data %>%
            select(offense_date) %>%
            filter(year(offense_date) >= startYear) %>%
            mutate(year = year(offense_date)) %>%
            distinct(year)
    
  } else if(filterType == "startMonth"){
    
      data <- data %>%
              select(offense_date) %>%
              filter(year(offense_date) >= startYear, year(offense_date) <= endYear) %>%
              mutate(month = month(offense_date)) %>%
              distinct(month)
      
  } else if(filterType == "endMonth"){
    
      data <- data %>%
              select(offense_date) %>%
              filter(year(offense_date) >= startYear, year(offense_date) <= endYear,
                     month(offense_date) >= startMonth) %>%
              mutate(month = month(offense_date)) %>%
              distinct(month)
      
  } else if(filterType == "startDay"){
    
      data <- data %>%
              select(offense_date) %>%
              filter(year(offense_date) >= startYear, year(offense_date) <= endYear,
                     month(offense_date) >= startMonth, month(offense_date) <= endMonth) %>%
              mutate(day = day(offense_date)) %>%
              distinct(day)   
      
  } else if(filterType == "endDay"){
    
      data <- data %>%
        select(offense_date) %>%
        filter(year(offense_date) >= startYear, year(offense_date) <= endYear,
               month(offense_date) >= startMonth, month(offense_date) <= endMonth,
               day(offense_date) >= startDay) %>%
        mutate(day = day(offense_date)) %>%
        distinct(day)   
      
  } else if(filterType == "zip"){
    
    data <- data %>%
      select(offense_date, zip) %>%
      filter(year(offense_date) >= startYear, year(offense_date) <= endYear,
             month(offense_date) >= startMonth, month(offense_date) <= endMonth,
             day(offense_date) >= startDay, day(offense_date) <= endDay) %>%
      distinct(zip)  
    
    data <- rbind(data, "all")
  } else if(filterType == "attempt"){
    
    data <- data %>%
      select(offense_date, zip, committed) %>%
      filter(year(offense_date) >= startYear, year(offense_date) <= endYear,
             month(offense_date) >= startMonth, month(offense_date) <= endMonth,
             day(offense_date) >= startDay, day(offense_date) <= endDay, zip %in% zipCodes) %>%
      distinct(committed)  
    
    data <- rbind(data, "both")
  }
  
  acceptedRange <- data[[1]]
  return(acceptedRange)
  
}

#-------------------------------------------------------------------#
# CrimeIncidentSummarizer()
#
# data: dataframe. The dataframe containing the crime data for to be
#       summarised
#
# Returns: data, a dataframe with the crime data summarized 
#          
#-------------------------------------------------------------------#
CrimeIncidentSummarizer <- function(data, period){
  
  data <- data %>%
          group_by(crime) %>%
          summarise(number_of_incidents = n())
  
  return(data)
  
}

#-------------------------------------------------------------------#
# OverviewGraphDataGen()
#
# currentData: dataframe. The dataframe containing the crime data for 
#              the analysis period
# previousData: dataframe. The dataframe containing the crime data for 
#              the week preceding the analysis period
#
# Returns: finalData, a dataframe which contains the consolidated 
#          crime data for the current and previous week 
#          
#-------------------------------------------------------------------#
OverviewGraphDataGen <- function(currentData, previousData){
  
  currentData <- CrimeIncidentSummarizer(currentData, "current")
  currentData <- mutate(currentData, period = "Current Week")
  
  previousData <- CrimeIncidentSummarizer(previousData, "previous")
  previousData <- mutate(previousData, period = "Previous Week")
  
  finalData <- rbind(currentData, previousData)
  #write.csv(finalData, "GraphData.csv")
  return(finalData)
  
}

#-------------------------------------------------------------------#
# OverviewGraphGen()
#
# data: dataframe. The dataframe containing the crime data for both 
#       the current and previous window
#
# Returns: overviewPlot, a ggplot object which contains the graph to
#          to be rendered
#          
#-------------------------------------------------------------------#
OverviewGraphGen <- function(data){
  
  overviewPlot <- ggplot(data) +
                  geom_bar(mapping = aes(x = crime, y = number_of_incidents, fill = period),
                           position = "dodge", stat = "identity")
  
  return(overviewPlot)
  
}

#-------------------------------------------------------------------#
# TrendGraphDataGen()
#
# currentData: dataframe. The dataframe containing the crime data for 
#              the analysis period
# previousData: dataframe. The dataframe containing the crime data for 
#              the week preceding the analysis period
#
# Returns: finalData, a dataframe which contains the consolidated 
#          crime data for the current and previous week 
#          
#-------------------------------------------------------------------#
TrendGraphDataGen <- function(currentData, previousData){
  
  currentData <- currentData %>%
                 group_by(rank_day) %>%
                 summarise(number_of_incidents = n()) %>%
                 mutate(period = "Current Week")
  
  previousData <- previousData %>%
                  group_by(rank_day) %>%
                  summarise(number_of_incidents = n()) %>%
                  mutate(period = "Previous Week")
  
  finalData <- rbind(currentData, previousData)
  #write.csv(finalData, "trendData.csv")
  return(finalData)
}

#-------------------------------------------------------------------#
# TrendGraphGen()
#
# data: dataframe. The dataframe containing the crime trend data for 
#        both the current and previous window
#
# Returns: trendPlot, a ggplot object which contains the graph to
#          to be rendered
#          
#-------------------------------------------------------------------#
TrendGraphGen <- function(data){
  
  trendPlot <- ggplot(data) +
               geom_line(mapping = aes(x = rank_day, y = number_of_incidents, 
                                       colour = period))
  
  return(trendPlot)
  
}

#-------------------------------------------------------------------#
# MapGraphGen()
#
# data: dataframe. The dataframe containing the spatial crime data 
#       for the selected crime and window
# map: ggmap object. The map on which the crime data needs to be 
#      plotted
#
# Returns: mapPlot, a ggplot + ggmap object which contains the map
#          to be rendered
#          
#-------------------------------------------------------------------#
MapGraphGen <- function(data, map){
  
  mapGraph <- map +
              stat_density2d(mapping = aes(x = long, y = lat, alpha = ..level.., fill = ..level..), 
                             size = 2, bins = 4, data = data, geom = "polygon") +
              scale_fill_gradient(low = "green", high = "red") + 
              guides(alpha = FALSE)
  
  return(mapGraph)
  
}

