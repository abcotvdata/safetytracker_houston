library(tidyverse)
library(sf)

# Give the input file name to the function.
# result <- fromJSON(file = "E:\\example.json")


# This is for extracting the last 30 days of crime data that Houston PD moves daily
# We will grab this data on a rolling basis daily, to ensure robust overlap with HPD's monthly file
# The monthly file that HPD vets and posts on its web site comes out midway through the month
# That data will replace this temporary data every month
# This will fill the gap temporarily and aid w/auditing


# THE SOURCE OF LAST 30 DAYS LAYERS DATA IS HERE
# They can be queried one by one; we need 0, 1 and 2. Those are:
# 0 is Group A offenses against people (murder, sexual assault, etc.)
# 1 is Group A offenses against property (car theft, burglary, etc.)
# 2 is lesser offenses against society (drugs, prostitution, etc.)
# https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer
# https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/0

# When we run these queries, because there's a limit of 2,000 records 
# We need to sort the records in descending order by date before extracting
# or we will lose newer records we need; there's enough margin because of our
# frequency of downloading to cover us because we're only then 'losing' oldest records

# This is the phrasing to use in the query tool to generate json api call
# USER_RMSOccurrenceDate DESC, OBJECTID

# proper query to order with newest date descending order
# https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/0/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC%2C+OBJECTID&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson
# use this in order by field in arcgis 
# USER_RMSOccurrenceDate DESC, OBJECTID

# Download the Group A People file and open into a sf/df
# Rename yesterday's version and archive it for redundancy
file.rename("data/source/recent/houston_30day_people.json", "data/source/archive/houston_30day_people.json")
# Now download today's version
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/0/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC%2C+OBJECTID&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=json",
              "data/source/archive/houston_30day_people.json")
# Read in newly downloaded file
houston_30day_people <- st_read("data/source/archive/houston_30day_people.json")

# Download the Group A Property file and open into a sf/df
# Rename yesterday's version and archive it for redundancy
file.rename("data/source/recent/houston_30day_property.json", "data/source/archive/houston_30day_property.json")
# Download today's version
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/1/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC%2C+OBJECTID&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
              "data/source/archive/houston_30day_property.json")
# Read in newly downloaded file
houston_30day_property <- st_read("data/source/archive/houston_30day_property.json")

# Download the Group A Society file and open into a sf/df
# Rename yesterday's version and archive it for redundancy
file.rename("data/source/recent/houston_30day_society.json", "data/source/archive/houston_30day_society.json")
# Download today's version
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/2/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC%2C+OBJECTID&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
              "data/source/archive/houston_30day_society.json")
# Read in newly downloaded file
houston_30day_society <- st_read("data/source/archive/houston_30day_society.json")

# Merge the three files into one and delete the separates
# Rename yesterday's version and archive it for redundancy
file.rename("data/source/recent/houston_30day_full.csv", 
            paste0("data/source/archive/houston_30day_full",Sys.Date(),".csv"))

# Merge
houston_30day <- bind_rows(houston_30day_people,
                           houston_30day_property,
                           houston_30day_society)
# Clean up
rm(houston_30day_people, houston_30day_property, houston_30day_society)

# Time fields are in this esri data are ms from a UTC origin
# This is my saved function to convert the milliseconds from UTC 
ms_to_date = function(ms, t0="1970-01-01", timezone) {
  sec = ms / 1000
  as.POSIXct(sec, origin=t0, tz=timezone)
}

# Convert occurrence date fields from esri ms since utc origin to dates in Central tz
houston_30day$date <- ms_to_date(as.numeric(houston_30day$USER_RMSOccurrenceDate), timezone="America/Chicago")
houston_30day$date <- substr(houston_30day$date,1,10)

# Save today's version of clean, combined HPD recent file for redundancy
write_csv(houston_30day,"data/source/archive/houston_30day_full.csv")


# OPEN WORK IS FIGURING OUT ROLLING SAVE 
# SO THAT WE GET ALL INCIDENTS FROM THE MOST RECENT DAYS
# OBJECTID is unique
# USERINCIDENT is the event, and can have a unique record id for each offense listed
# We want this file to only serve as a temporary accounting of crime since the max date
# of the most recent officially monthly file posted on HPD crime stats web site

#We want to import the file every day
#We want to then add any new records to the main file and save a backup
#We want to then slice off records that are newer than the max record in houston crime
#and append/bind

# Let's get that on a server Monday morning so we can let it run for days and make sure it is doing what we want

houston_30day_old <- read_csv("data/source/archive/houston_30day_full2022-09-19.csv", 
                                         col_types = cols(OBJECTID = col_integer(), 
                                                          USER_RMSOccurrenceDate = col_character(), 
                                                          USER_Incident = col_character(), 
                                                          newdate = col_date(format = "%Y-%m-%d")))

newrecs_houston_30day <- anti_join(houston_30day,
                                   houston_30day_old,
                                   by=c("USER_Incident","USER_RMSOccurrenceDate"))

incidentnumbercheck_old <- houston_30day_old %>% group_by(USER_Incident) %>% summarise(count=n())
incidentnumbercheck_new <- houston_30day %>% group_by(USER_Incident) %>% summarise(count=n())
incidentnumbercheck <- full_join(incidentnumbercheck_new,incidentnumbercheck_old,by="USER_Incident")
names(incidentnumbercheck) <- c("incident_number","count_new","count_old")
incidentnumbercheck$check <- incidentnumbercheck$count_new-incidentnumbercheck$count_old
  
# Adapt the houston_30day file to match the style of 2019-22 dataframes
houston_30day$offense_count <- 1
houston_30day$city <- "HOUSTON"
houston_30day$longitude <- NA
houston_30day$latitude <- NA
houston_30day <- houston_30day %>% 
  select(4,20,3,5,6,21,8,9,11,12,13,14,22,15,23,24)

# Save clean, combined HPD recent file for redundant backup purposes
write_csv(houston_30day,"data/source/latest/houston_30day.csv")





# Read in the 2022 file, renaming to cols to standardize across all years
houston_30day_new <- read_csv("data/source/latest/houston_30day.csv", 
                        col_types = list("c", "date", "d", 
                                      "c", "c", "d", "c", 
                                      "c", "c", "c", "c", "c", 
                                      "c", "c","d","d"))
names(houston_30day_new) <- c("incident", "date", "hour", 
                      "nibrs_class", "offense_type", "offense_count", "beat", 
                      "premise", "street_no", "street_name", "street_type", "street_suffix", 
                      "city", "zip","longitude","latitude")

houston_30day_new <- houston_30day_new %>% filter(as.Date(date)>max(as.Date(houston22$date)))


