library(tidyverse)
library(tidycensus)
library(sp)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(areal)


# GEOGRAPHY
# Get police beat geographies from the city gis portal
# download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HoustonMap/Public_safety/MapServer/5/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson&as_sfid=AAAAAAX9U41i0A-Vf-hRb3YNf7bAoeFJRxBqkyDooZpXoi0-qhqPbL-rIoVE5aCxw4MkyZKj1GZKVkzCkAujF1k9pvsNagR-Uzq5XwkoiD6GReZ6bI_3xncbLCxrVgd0lO1mqz8%3D&as_fid=8da5ff3e7b61102f7597eedfcdab445e7c7bae4d",
#              "data/geo/houston_police_beats.geojson")
# Read in geojson and then transform to sf format
beats <- st_read("data/geo/houston_police_beats.geojson")
# Fix an invalid polygon(s) in the beats file
beats <- st_make_valid(beats)
# Transform tidycensus data to match beats projection
# beats <- st_transform(beats, 4269)

# Get demographic data for Census block groups to aggregate/apportion to beats geography
# Note that we are asking for the more detailed tiger line files, so we can trim water later
# Also transforming to match the projection of the Houston PD's beats spatial file
blockgroups <- get_acs(geography = "block group", 
                       year = 2020,
                       output = 'wide',
                       variables = "B03002_001", 
                       state = "TX",
                       county = c("Harris County","Fort Bend","Montgomery"),
                       geometry = TRUE) %>%
st_transform(4326)


ar_validate(source = blockgroups, target = beats, varList = "B03002_001E", method = "aw",verbose=TRUE)


# test to look at quick map of blockgroups
# map_blockgroups <- leaflet(blockgroups) %>% setView(-95.45, 29.75, zoom = 10) %>% 
#  addProviderTiles(provider = "CartoDB.Positron") %>% 
#  addPolygons(color = "green")

# Grab just the population and geometry from blockgroups from tidycensus
pop <- blockgroups["B03002_001E"] # keep only the values to be transferred

# Calculate the estimated population of beat geographies/interpolate with tidycensus block groups
# Reminder: ext=true SUMS the population during interpolation; assumes pop is evenly distributed
beats_withpop <- st_interpolate_aw(pop, beats, ext = TRUE)
# Drops geometry so it's not duplicated in the merge
beats_withpop <- st_drop_geometry(beats_withpop)
# Binds that new population column to the table
beats <- cbind(beats,beats_withpop)

# Trim and clean beats file to just what we need
beats <- beats %>%
  rename("population"="B03002_001E","beat"="Beats") %>%
  select(beat,District,Area_sq_mi,population,geometry) %>% 
  janitor::clean_names()
# Round the population figure
beats$population <- round(beats$population,0)
# Eliminate population from two beats on airport property
beats$population <- ifelse(beats$population<1000,0,beats$population)
# Cleans up unneeded calculation file
rm(beats_withpop)
# Test aggregated population total for beats vs city population reported by Census
sum(beats$population) # tally is 2.29M # city's reported pop is 2.32M in 2020; this works for our purposes

# Adding place descriptions of the beats
beats$placename <- case_when(beats$beat == "1A10" ~ "Downtown Central Business District",
                        beats$beat == "1A20" ~ "Fourth Ward, Avondale, Westmoreland",
                        beats$beat == "1A30" ~ "Cherryhurst, Mandell Place, Montrose",
                        beats$beat == "1A40" ~ "Tall Timbers, Royden Oaks, River Oaks, Avalon Place, Oak Estates",
                        beats$beat == "1A50" ~ "Post Oak Park, Afton Oaks, Highland Village, Weslayan, Greenway Plaza, The Compaq Center",
                        beats$beat == "2A10" ~ "Irvington, Ryon",
                        beats$beat == "2A20" ~ "Sunset Heights, Brooksmith, Stude",
                        beats$beat == "2A30" ~ "Houston Heights, Norhill, Studemont",
                        beats$beat == "2A40" ~ "Old Sixth Ward, First Ward",
                        beats$beat == "2A50" ~ "Memorial Park, West End, Ashbury, Cottage Grove",
                        beats$beat == "2A60" ~ "Westport, Holly Park, Lazy Brook, Citadel, Shady Acres, Timbergrove",
                        beats$beat == "3B10" ~ "Northwest Mall, Brookhollow, White Oak Acres, Oak Forest, Langwood",
                        beats$beat == "3B30" ~ "White Oak Acres, Brook Hollow, Oak Forest, Garden Oaks, Shepherd Park Plaza",
                        beats$beat == "3B40" ~ "Independence Heights, Melrose, Graceland",
                        beats$beat == "3B50" ~ "Northline Mall, Stratton Place, Oakwood, Colonial Gardens, Lindale, Junction City",
                        beats$beat == "6B10" ~ "York Plaza, Pembrook Place, Hawthorne Place",
                        beats$beat == "6B20" ~ "Highland Heights, Oakdale, Melrose Gardens",
                        beats$beat == "6B30" ~ "Woodland Trails, Oaks of Inwood, Bayou Bend, Cole Creek Manor, Inwood Forest",
                        beats$beat == "6B40" ~ "Highland Gardens, Highland Heights, North Plaza",
                        beats$beat == "6B50" ~ "Aldine, Imperial Valley",
                        beats$beat == "6B60" ~ "Greenspoint",
                        beats$beat == "7C10" ~ "Fifth Ward, North Park, Pinecrest",
                        beats$beat == "7C20" ~ "Huntington Place, Willshire Place, Kashmere Gardens, Liberty Gardens",
                        beats$beat == "7C30" ~ "Camden Woods, Archers Acres, Croyden Gardens",
                        beats$beat == "7C40" ~ "Keith-Weiss Park, Farrington Place",
                        beats$beat == "7C50" ~ "Intercontinental Airport, Northwood, Lakeview Park, Glen Lee",
                        beats$beat == "8C10" ~ "Tidwell Park, Shady Oak, Buckingham Place",
                        beats$beat == "8C20" ~ "Houston Gardens, Rosewood, Homestead",
                        beats$beat == "8C30" ~ "Parkhurst, East Houston, Clairmont Place",
                        beats$beat == "8C40" ~ "Lake Houston, Greenriver, Knoll, Lake Forest",
                        beats$beat == "8C50" ~ "Park North, Forest Green, Kentshire Place, Glenwood Forest, Dorchester",
                        beats$beat == "8C60" ~ "Edgeworth, Northwood Manor, Scenic Woods",
                        beats$beat == "9C10" ~ "Port of Houston, Clinton Park, Pleasantville, Fidelity Manor",
                        beats$beat == "9C20" ~ "Denver Harbor, Houston Harbor",
                        beats$beat == "9C30" ~ "City Park East, Wynnewood Acres, Wallisville Gardens",
                        beats$beat == "9C40" ~ "Greens Bayou Estates, Shady Brook, Northshore",
                        beats$beat == "24C10" ~ "Kingwood",
                        beats$beat == "24C20" ~ "Kingwood",
                        beats$beat == "24C30" ~ "Kingwood",
                        beats$beat == "24C40" ~ "Kingwood",
                        beats$beat == "24C50" ~ "Kingwood",
                        beats$beat == "11H10" ~ "Mason Park, Forest Hill, Sunnyland, Central Park",
                        beats$beat == "11H20" ~ "Harrisburg, Manchester, Pecan Park",
                        beats$beat == "11H30" ~ "Meadow Brook, Park Place",
                        beats$beat == "11H40" ~ "Allendale, Oak Meadows",
                        beats$beat == "11H50" ~ "Meadow Creek, Ferndale Addition, Glenbrook Industrial",
                        beats$beat == "XXH10" ~ "Murder",
                        beats$beat == "XXH20" ~ "Murder",
                        beats$beat == "XXH30" ~ "Murder",
                        beats$beat == "XXH40" ~ "Murder",
                        beats$beat == "XXH50" ~ "Murder",
                        TRUE ~ "Unknown")




# Calculate District population totals to review vs. Houston PD web site
district_pops <- beats %>%
  group_by(district) %>%
  summarise(pop=sum(population),
            sqmi = sum(area_sq_mi)) %>% st_drop_geometry(district_pops)

