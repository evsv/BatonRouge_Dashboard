# 1.1. SETTING UP LIBRARIES
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(hms)
library(ggmap)

# 1.2. READING THE FLAT FILE

brRawData <- read_csv(file = ".\\Data\\Baton_Rouge_Crime_Incidents.csv")

# 1.3. UNDERSTANDING THE DATASET

View(head(brRawData, 5))
brRawData

distinct(brRawData, CITY)
distinct(brRawData, STATE)

head(brRawData$GEOLOCATION)
head(brRawData$`COMPLETE DISTRICT`)
head(brRawData$ADDRESS)

# 1.4. INITIAL CORRECTIONS TO THE DATASET

## 1.4.1. CHANGING COLUMN NAMES TO LOWERCASE, AND REPLACING SPACES
## WITH UNDERSCORES

names(brRawData) <- tolower(names(brRawData))
names(brRawData) <- gsub(" ", x = names(brRawData), replacement = "_")

## 1.4.2. FORMATTING THE TIME STRING CORRECTLY

#CHECKING FOR AND REMOVING THE WRONGLY ENTERED VALUES
numOfCharacters <- sum(is.na(as.numeric(brRawData$offense_time)))
characterValues <- brRawData[is.na(as.numeric(brRawData$offense_time)), "offense_time"]
characterValues <- characterValues[!is.na(characterValues$offense_time), "offense_time"]

brRawData <- filter(brRawData, !(is.na(as.numeric(brRawData$offense_time)) &
                               !is.na(brRawData$offense_time)))

#CHECKING IF REMOVAL WAS PERFORMED CORRECTLY             
characterValues <- brRawData[is.na(as.numeric(brRawData$offense_time)), "offense_time"]
characterValues <- characterValues[!is.na(characterValues$offense_time), "offense_time"]

rm(numOfCharacters, characterValues)

#CHECKING IF ALL VALUES ARE IN HHMM FORM
differentFormat <- filter(brRawData, nchar(offense_time) != 4) 

#REMOVING THE MIS-FORMATTED VALUE
brRawData <- filter(brRawData, !(nchar(offense_time) != 4) |
                               is.na(offense_time))

rm(differentFormat)

#APPENDING THE SECONDS VALUE
brRawData <- mutate(brRawData, offense_time = if_else(!is.na(offense_time), 
                                                      paste0(offense_time, ":00"),
                                                      offense_time))

#FORMATTING THE TIME CHARACTER PROPERLY
str_sub(brRawData$offense_time, 3, 2) <- ":"

## 1.4.3. CONVERTING DATE AND TIME INTO DATE-TIME OBJECTS

brRawData <- mutate(brRawData, offense_date = mdy(offense_date),
                    offense_time = as.hms(offense_time))

# 1.5. ExTRACTING PRIMA-FACIE USEFUL FEATURES

## 1.5.1. EXTRACTING DAY OF WEEK

brRawData <- mutate(brRawData, day_of_week = wday(offense_date, label = TRUE))
 

## 1.5.2. EXTRACTING THE LATLONG

brRawData <- mutate(brRawData, 
                    raw_lat_long = regmatches(geolocation, gregexpr("(?<=\\().*?(?=\\))", geolocation, perl=T))) %>%
             mutate(lat = as.numeric(str_sub(raw_lat_long, 1, regexpr(",", raw_lat_long)-1)),
                    long = as.numeric(str_sub(raw_lat_long, regexpr(" ", raw_lat_long), nchar(raw_lat_long)))) %>%
             select(-raw_lat_long)

# 1.6. WRITING OUTPUT DATA FILE
write.csv(brRawData, "brdata_cleaned.csv")
