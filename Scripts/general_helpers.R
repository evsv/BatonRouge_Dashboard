#===================================================================#
# THIS SCRIPT CONTAINS GENERAL PURPOSE HELPER METHODS USED ACROSS THE
# INDIVIDUAL VIEWS AS WELL AS AT THE DATA QUALITY AND EDA LEVEL
#===================================================================#

library(RCurl)
library(RJSONIO)


#-------------------------------------------------------------------#
# latlon2zip()
#
# lat: numeric. Latitude of the location to be converted
# lon: numeric. Longitude of the location to be converted
#
# NOTE: Function currently breaks mid operation, due to possible 
#       connection break as it iterates over each row
#-------------------------------------------------------------------#

latlon2zip <- function(lat, lon) {
  url <- sprintf("http://nominatim.openstreetmap.org/reverse?format=json&lat=%f&lon=%f&zoom=18&addressdetails=1", lat, lon)
  result_full <- lapply(url, fromJSON)
  result_zip <- lapply(result_full, function(x) x[["address"]][["postcode"]])
  return(result_zip)
}




