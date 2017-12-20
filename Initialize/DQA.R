# 2.1. DATA QUALITY ANALYSIS

## 2.1.1. FILE NUMBER ANALYSIS

fileNumberCheck <- brRawData %>%
                   group_by(file_number) %>%
                   mutate(num_of_cases = n()) %>%
                   arrange(desc(num_of_cases), file_number, offense_date, offense_time)

fileNumberCheckNoMissing <- brRawData %>%
                            filter(!is.na(file_number)) %>%
                            group_by(file_number) %>%
                            mutate(num_of_cases = n()) %>%
                            arrange(desc(num_of_cases), file_number, offense_date, offense_time)  

rm(fileNumberCheck, fileNumberCheckNoMissing)

## 2.1.2. DATE ANALYSIS

dateTimeData <- brRawData %>%
                select(offense_date, offense_time, day_of_week) %>%
                mutate(offense_year = year(offense_date))

table(dateTimeData$offense_year)
sum(is.na(dateTimeData$offense_year))

## 2.1.3. TIME ANALYSIS

sum(is.na(dateTimeData$offense_time))
nrow(filter(dateTimeData, offense_time == as.hms(00:00:00)))
View(filter(brRawData, offense_time == as.hms(00:00:00)))

## 2.1.4. CRIME ANALYSIS

sum(is.na(brRawData$offense))
sum(is.na(brRawData$offense_desc))
sum(is.na(brRawData$crime))
sum(is.na(brRawData$committed))

distinct(brRawData, crime)
distinct(brRawData, offense)
distinct(brRawData, offense_desc)

# 2.1.5. LOCATION ANALYSIS

sum(is.na(brRawData$zip))
sum(is.na(brRawData$lat))
sum(is.na(brRawData$long))
sum(is.na(brRawData$address))

sum((is.na(brRawData$address) | is.na(brRawData$offense_time)))
View(filter(brRawData, is.na(address), is.na(offense_time)))

# FILLING OUT MISSING ZIP CODES FROM LATLON DATA
# CURRENTLY UNABLE TO DO THIS, DUE TO API CALL LIMITS, CONNECTION 
# BREAKS, ETC