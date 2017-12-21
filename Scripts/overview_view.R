#===================================================================#
# THIS SCRIPT CONTAINS METHODS USED FOR THE OVERVIEW VIEW
#===================================================================#


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




