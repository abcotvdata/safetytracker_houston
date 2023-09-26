library(tidyverse)
library(sf)
# library(jsonlite)

### PIPELINE TO IMPORT HOUSTON PD'S MOST RECENT 30 DAYS OF CRIME INCIDENTS 
# Houston PD updates these json feeds daily; we will capture on a rolling basis, de-dupe and append
# This will fill the gap from the HPD monthly crime file posted around the 3rd week of every month

# SOURCE: https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer
# They can be queried one by one; we need 0, 1 and 2 and do not need 3 and 4 (for this tracker).
# But here is the full list of all of them:
# 0 is Group A offenses against people (murder, sexual assault, etc.)
# 1 is Group A offenses against property (car theft, burglary, etc.)
# 2 is Group A offenses against society (drugs, prostitution, etc.)
# 3 is lesser Group B offenses we will gather for completeness
# 4 is 'non-crime' incidents we will gather for completeness (e.g., police list justifiable homicide here)

# There's a max output of 2,000 records for each download
# So we're going to manually stream by section, with a rolling offset, to capture all records

# STEP 1: For safety/redundancy, set double backup of our two-day archive
file.copy("data/source/recent/houston_recent_prior.rds", # archives 2 days ago
          "data/source/archive/houston_recent_prior.rds", overwrite = TRUE)
file.copy("data/source/recent/houston_recent_new.rds", # archives yesterday
          "data/source/archive/houston_recent_new.rds", overwrite = TRUE)

# STEP 2: Set the pieces of the urls we need to build in the loop function later to download
urlfront_people <- "https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/0/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset="
urlfront_property <- "https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/1/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset="
urlfront_society <- "https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/2/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset="
urlend <- "&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson"

# STEP 3: Get the data

# Create an empty dataframe to store data
houston_recent_new <- data.frame()

# Set sequences for people, property and society data stream loops
# Setting separately because each one has a slightly different volume of records
people_offsets <- c(0, 2000, 4000, 6000, 8000)
property_offsets <- c(0, 2000, 4000, 6000, 8000, 10000, 12000)
society_offsets <- c(0, 2000)

# Function to build url list and append data
for(i in people_offsets) {
  # Build full url
  temp_url <- paste0(urlfront_people, i, urlend)
  # Download and read the JSON file into a dataframe
  temp_df <- st_read(temp_url)
  # Append to df
  houston_recent_new <- bind_rows(houston_recent_new, temp_df)
}

# Function to build url list and append data
for(i in property_offsets) {
  # Build full url
  temp_url <- paste0(urlfront_property, i, urlend)
  # Download and read the JSON file into a dataframe
  temp_df <- st_read(temp_url)
  # Append to df
  houston_recent_new <- bind_rows(houston_recent_new, temp_df)
}

# Function to build url list and append data
for(i in society_offsets) {
  # Build full url
  temp_url <- paste0(urlfront_society, i, urlend)
  # Download and read the JSON file into a dataframe
  temp_df <- st_read(temp_url)
  # Append to df
  df <- bind_rows(houston_recent_new, temp_df)
}


# Time fields are in this esri data are ms from a UTC origin
# This is my saved function to convert the milliseconds from UTC 
ms_to_date = function(ms, t0="1970-01-01", timezone) {
  sec = ms / 1000
  as.POSIXct(sec, origin=t0, tz=timezone)
}

# Convert occurrence date fields from esri ms since utc origin to dates in Central tz
houston_recent_new$date <- ms_to_date(as.numeric(houston_recent_new$USER_RMSOccurrenceDate), timezone="GMT")
houston_recent_new$date <- as.Date(substr(houston_recent_new$date,1,10))

# Adapt the houston_30day file to match the style of 2019-22 dataframes
# adding fields present in annual and monthly dfs
houston_recent_new$offense_count <- 1
houston_recent_new$city <- "HOUSTON"
houston_recent_new$longitude <- NA
houston_recent_new$latitude <- NA
# reordering and renaming cols to match
houston_recent_new <- houston_recent_new %>% 
  select(4,20,3,5,6,21,8,9,11,12,13,14,22,15,23,24)
names(houston_recent_new) <- c("incident", "date", "hour", 
                               "nibrs_class", "offense_type", "offense_count", "beat", 
                               "premise", "street_no", "street_name", "street_type", "street_suffix", 
                               "city", "zip","longitude","latitude")
houston_recent_new$hour <- as.numeric(houston_recent_new$hour)

### OPEN WORK HERE: Package to convert OLC to latitude and longitude

# Copy yesterday's to recent_prior in two places: archive and rds store
file.copy("scripts/rds/houston_recent_new.rds", # dupe yesterday file
          "scripts/rds/houston_recent_prior.rds", overwrite = TRUE)
file.copy("scripts/rds/houston_recent_new.rds", # dupe yesterday file
          "data/source/recent/houston_recent_prior.rds", overwrite = TRUE)

# Load prior day df
houston_recent_prior <- readRDS("scripts/rds/houston_recent_prior.rds")

# Temp fix dupes; occassional as needed during manual maintenance
# houston_recent_prior <- houston_recent_prior %>% arrange(desc(date))
# houston_recent_prior <- houston_recent_prior[!duplicated(houston_recent_prior[, c("incident", "offense_type", "beat","zip")]), ]
# saveRDS(houston_recent_prior,"scripts/rds/houston_recent_prior.rds")

# Merge prior day and today and then de-dupe
houston_recent_new <- bind_rows(houston_recent_prior,
                                houston_recent_new)
houston_recent_new <- unique(houston_recent_new)
houston_recent_new$beat <- sub(pattern='^0+([1-9])', replacement='\\1', houston_recent_new$beat)
# trim to maintain only the last 60 days
houston_recent_new <- houston_recent_new %>% filter(houston_recent_new$date > max(houston_recent_new$date)-60)

# Save copies of newly-processed houston_recent_new in 3 places for redundancy
# 1. Archive with name by day of month; maintains an extra day of files
saveRDS(houston_recent_new,paste0("data/source/archive/houston_recent_archive.rds"))
# 2. Latest day archived in source data as backup; overwritten daily
saveRDS(houston_recent_new,"data/source/recent/houston_recent_new.rds")
# 3. Latest day stored in scripts file for pickup by trackers
# If for some reason the script fails, file from day before is there
saveRDS(houston_recent_new,"scripts/rds/houston_recent_new.rds")

# Clean up
rm(houston_recent_new,houston_recent_prior)

