library(tidyverse)
library(tidycensus)
library(sp)
library(sf)


# GEOGRAPHY
# Get police beat geographies from the city gis portal
download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HoustonMap/Public_safety/MapServer/5/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson&as_sfid=AAAAAAX9U41i0A-Vf-hRb3YNf7bAoeFJRxBqkyDooZpXoi0-qhqPbL-rIoVE5aCxw4MkyZKj1GZKVkzCkAujF1k9pvsNagR-Uzq5XwkoiD6GReZ6bI_3xncbLCxrVgd0lO1mqz8%3D&as_fid=8da5ff3e7b61102f7597eedfcdab445e7c7bae4d",
              "data/geo/houston_police_beats.geojson")
# Read in geojson and then transform to sf format
beats <- st_read("data/geo/houston_police_beats.geojson")
# Fix an invalid polygon(s) in the beats file
beats <- st_make_valid(beats)

# Get demographic data for Census block groups to aggregate/apportion to beats geography
blockgroups <- get_acs(geography = "block group", 
                       year = 2019,
                       output = 'wide',
                       variables = "B03002_001", 
                       state = "TX",
                       county = c("Harris County","Fort Bend","Montgomery"),
                       geometry = TRUE)
# Transform tidycensus data to match beats projection
blockgroups <- st_transform(blockgroups, 4326)
# Grab just the population and geometry from blockgroups from tidycensus
pop <- blockgroups["B03002_001E"] # keep only the values to be transferred

# Calculate the estimated population of beat geographies/interpolate with tidycensus bgs
# Reminder: ext=true SUMS the population during interpolation
beats_withpop <- st_interpolate_aw(pop, beats, ext = TRUE)
# Binds that new population column to the table
beats <- cbind(beats,beats_withpop)
# Two quick checks that we've got this right
# Checking that the geographies merged correct
ifelse(beats$geometry == beats$geometry.1,"yes","no")
# Check total population assigned/estimated across all beats
sum(beats$B03002_001E) # tally is 2.29M # city's reported pop is 2.30M in 2019; this works for our purposes
# Trim and clean beats file to just what we need
beats <- beats %>%
  rename("population"="B03002_001E","beat"="Beats") %>%
  select(beat,Area_sq_mi,population,geometry) %>% 
  janitor::clean_names()
beats$population <- round(beats$population,0)
beats$population <- ifelse(beats$population<2500,0,beats$population)
rm(beats_withpop)
sum(beats$population)
