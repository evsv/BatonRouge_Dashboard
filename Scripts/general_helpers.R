#===================================================================#
# THIS SCRIPT CONTAINS GENERAL PURPOSE HELPER METHODS USED ACROSS THE
# INDIVIDUAL VIEWS AS WELL AS AT THE DATA QUALITY AND EDA LEVEL
#===================================================================#

library(RCurl)
library(RJSONIO)


#-------------------------------------------------------------------#
# LatLon2Zip()
#
# lat: numeric. Latitude of the location to be converted
# lon: numeric. Longitude of the location to be converted
#
# NOTE: Function currently breaks mid operation, due to possible 
#       connection break as it iterates over each row
#-------------------------------------------------------------------#

LatLon2Zip <- function(lat, lon) {
  url <- sprintf("http://nominatim.openstreetmap.org/reverse?format=json&lat=%f&lon=%f&zoom=18&addressdetails=1", lat, lon)
  result_full <- lapply(url, fromJSON)
  result_zip <- lapply(result_full, function(x) x[["address"]][["postcode"]])
  return(result_zip)
}

#-------------------------------------------------------------------#
# FilterData()
#
# data: dataframe. The dataframe on which filtering is to be applied
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
# attempt: char. Filter to select crimes which are either attempted
#          or committed. Defaults to both, in which case all crimes
#          are included
#
# Returns: filteredData, a dataframe.  
#-------------------------------------------------------------------#

FilterData <- function(data, startDate = "all", endDate = "all", 
                       zipCodes = "all", attempt = "both", offenseCategory = "all"){
  
  filteredData <- data
  
  if(startDate != "all")
    filteredData <- filter(filteredData, startDate <= offense_date)
    
  if(endDate != "all")
    filteredData <- filter(filteredData, endDate >= offense_date)
  
  if(zipCodes != "all")
    filteredData <- filter(filteredData, zip %in% zipCodes)
  
  if(attempt != "both")
    filteredData <- filter(filteredData, committed = attempt)
  
  if(offenseCategory != "all")
    filteredData <- filter(filteredData, crime %in% offenseCategory)
  
  filteredData <- arrange(filteredData, offense_date, offense_time)
  return(filteredData)
}

# TESTING SCRIPTS, TO BE MOVED TO A SEPARATE TESTING FILE
#test1 <- FilterData(brRawData, 2016, 2017, 1, 3, 1, 31)
#test2 <- FilterData(brRawData, 2016, 2016, 1, 3, 1, 31)
#test3 <- FilterData(brRawData, 2016, 2016, 1, 1, 1, 31)
#test4 <- FilterData(brRawData, 2016, 2016, 1, 1, 1, 31, zipCodes = 70816)
#test5 <- FilterData(brRawData, 2016, 2016, 1, 1, 1, 31, zipCodes = c(70816, 70805))
