library(tidyverse)
library(sf)

### IMPORT HOUSTON PD'S RECENT 30 DAYS CRIME FILES TO APPEND
# Houston PD updates these json feeds daily and we will capture on a rolling basis
# Gathering the new day's files, merging together into one and then appending + de-duping
# This will fill the gap from HPD vetted monthly file posts third week of month

# SOURCE: https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer
# They can be queried one by one; we need 0, 1 and 2. Those are:
# 0 is Group A offenses against people (murder, sexual assault, etc.)
# 1 is Group A offenses against property (car theft, burglary, etc.)
# 2 is Group A offenses against society (drugs, prostitution, etc.)
# 3 is lesser Group B offenses we will gather for completeness
# 4 is 'non-crime' incidents we will gather for completeness (e.g., police list justifiable homicide here)

# There's a max output for feeds of 2,000 records
# We're manually streaming by section with offset to capture all

# First, for safety/redundancy sake, setting double backup of our two-day archive
file.copy("data/source/recent/houston_recent_prior.rds", # archives 2 days ago
          "data/source/archive/houston_recent_prior.rds", overwrite = TRUE)
file.copy("data/source/recent/houston_recent_new.rds", # archives yesterday
          "data/source/archive/houston_recent_new.rds", overwrite = TRUE)

# Download the Group A People file and open into a sf/df
# Now download today's version in streams; five is overly redundant but safest
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/0/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=0&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
              "data/source/archive/houston_30day_people1.json")
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/0/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=2000&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
              "data/source/archive/houston_30day_people2.json")
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/0/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=4000&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
              "data/source/archive/houston_30day_people3.json")
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/0/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=6000&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
              "data/source/archive/houston_30day_people4.json")
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/0/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=8000&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
              "data/source/archive/houston_30day_people5.json")
# Read in newly downloaded file
houston_30day_people1 <- st_read("data/source/archive/houston_30day_people1.json")
houston_30day_people2 <- st_read("data/source/archive/houston_30day_people2.json")
houston_30day_people3 <- st_read("data/source/archive/houston_30day_people3.json")
houston_30day_people4 <- st_read("data/source/archive/houston_30day_people4.json")
houston_30day_people5 <- st_read("data/source/archive/houston_30day_people5.json")

# Download the Group A Property file and open into a sf/df
# Download today's version; seven streams is overly redundant but safest
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/1/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=0&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
              "data/source/archive/houston_30day_property1.json")
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/1/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=2000&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
              "data/source/archive/houston_30day_property2.json")
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/1/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=4000&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
              "data/source/archive/houston_30day_property3.json")
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/1/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=6000&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
              "data/source/archive/houston_30day_property4.json")
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/1/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=8000&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
              "data/source/archive/houston_30day_property5.json")
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/1/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=10000&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
              "data/source/archive/houston_30day_property6.json")
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/1/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=12000&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
              "data/source/archive/houston_30day_property7.json")
# Read in newly downloaded file
houston_30day_property1 <- st_read("data/source/archive/houston_30day_property1.json")
houston_30day_property2 <- st_read("data/source/archive/houston_30day_property2.json")
houston_30day_property3 <- st_read("data/source/archive/houston_30day_property3.json")
houston_30day_property4 <- st_read("data/source/archive/houston_30day_property4.json")
houston_30day_property5 <- st_read("data/source/archive/houston_30day_property5.json")
houston_30day_property6 <- st_read("data/source/archive/houston_30day_property6.json")
houston_30day_property7 <- st_read("data/source/archive/houston_30day_property7.json")

# Download the Group A Society file and open into a sf/df
# Download today's version
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/2/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC%2C+OBJECTID&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
              "data/source/archive/houston_30day_society1.json")
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/2/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=USER_RMSOccurrenceDate+DESC%2C+OBJECTID&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=2000&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
              "data/source/archive/houston_30day_society2.json")
# Read in newly downloaded file; two streams is overly redundant but safest
houston_30day_society1 <- st_read("data/source/archive/houston_30day_society1.json")
houston_30day_society2 <- st_read("data/source/archive/houston_30day_society2.json")

# Download the Group B/Other files and open into a sf/df
# Download today's version; three is overly redundant but safest
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/3/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=0&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
              "data/source/archive/houston_30day_groupB1.json")
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/3/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=2000&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
              "data/source/archive/houston_30day_groupB2.json")
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/3/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=4000&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
              "data/source/archive/houston_30day_groupB3.json")
# Read in newly downloaded file; two streams is overly redundant but safest
houston_30day_groupB1 <- st_read("data/source/archive/houston_30day_groupB1.json")
houston_30day_groupB2 <- st_read("data/source/archive/houston_30day_groupB2.json")
houston_30day_groupB3 <- st_read("data/source/archive/houston_30day_groupB3.json")

# Download the Non Crime / Other files and open into a sf/df
# Download today's version; 1 will more than suffice
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HPD/NIBRS_Recent_Crime_Reports/MapServer/4/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson",
              "data/source/archive/houston_30day_other.json")
# Read in newly downloaded file; two streams is overly redundant but safest
houston_30day_other <- st_read("data/source/archive/houston_30day_other.json")

# Merge
houston_recent_new <- bind_rows(houston_30day_people1,
                                houston_30day_people2,
                                houston_30day_people3,
                                houston_30day_people4,
                                houston_30day_people5,
                                houston_30day_property1,
                                houston_30day_property2,
                                houston_30day_property3,
                                houston_30day_property4,
                                houston_30day_property5,
                                houston_30day_property6,
                                houston_30day_property7,
                                houston_30day_society1,
                                houston_30day_society2,
                                houston_30day_groupB1,
                                houston_30day_groupB2,
                                houston_30day_groupB3,
                                houston_30day_other)
# Clean up
rm(houston_30day_people1,
   houston_30day_people2,
   houston_30day_people3,
   houston_30day_people4,
   houston_30day_people5,
   houston_30day_property1,
   houston_30day_property2,
   houston_30day_property3,
   houston_30day_property4,
   houston_30day_property5,
   houston_30day_property6,
   houston_30day_property7,
   houston_30day_society1,
   houston_30day_society2,
   houston_30day_groupB1,
   houston_30day_groupB2,
   houston_30day_groupB3,
   houston_30day_other)

# Time fields are in this esri data are ms from a UTC origin
# This is my saved function to convert the milliseconds from UTC 
ms_to_date = function(ms, t0="1970-01-01", timezone) {
  sec = ms / 1000
  as.POSIXct(sec, origin=t0, tz=timezone)
}

# Convert occurrence date fields from esri ms since utc origin to dates in Central tz
houston_recent_new$date <- ms_to_date(as.numeric(houston_recent_new$USER_RMSOccurrenceDate), timezone="America/Chicago")
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

