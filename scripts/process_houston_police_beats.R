library(tidyverse)
library(tidycensus)
# library(sp)
library(sf)

# Get city/police GIS file of police beat boundaries 
# download.file("https://mycity2.houstontx.gov/pubgis02/rest/services/HoustonMap/Public_safety/MapServer/5/query?where=0%3D0&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson&as_sfid=AAAAAAX9U41i0A-Vf-hRb3YNf7bAoeFJRxBqkyDooZpXoi0-qhqPbL-rIoVE5aCxw4MkyZKj1GZKVkzCkAujF1k9pvsNagR-Uzq5XwkoiD6GReZ6bI_3xncbLCxrVgd0lO1mqz8%3D&as_fid=8da5ff3e7b61102f7597eedfcdab445e7c7bae4d","data/source/geo/houston_police_beats.geojson")

# Read in geojson, transform to sf format
# Transform to consistent planar data for area calculation
# Reduce to only the fields we need and clean field names
beats <- st_read("data/source/geo/houston_police_beats.geojson") %>%
  st_transform(3857) %>% select(4,5,7) %>% janitor::clean_names() %>%
  rename("beat"="beats")
# st_crs(beats)

# Adding placenames for headers in place of the beats
# A little more work to do here; may want to replicate this with zips too
beats$placename <- case_when(beats$beat == "1A10" ~ "Downtown Central Business District",
                             beats$beat == "1A20" ~ "Fourth Ward, Avondale and Westmoreland",
                             beats$beat == "1A30" ~ "Cherryhurst, Mandell Place and Montrose",
                             beats$beat == "1A40" ~ "Tall Timbers, Royden Oaks, River Oaks, Avalon Place and Oak Estates",
                             beats$beat == "1A50" ~ "Post Oak Park, Afton Oaks, Highland Village, Weslayan, Greenway Plaza and The Compaq Center",
                             beats$beat == "2A10" ~ "Irvington and Ryon",
                             beats$beat == "2A20" ~ "Sunset Heights, Brooksmith and Stude",
                             beats$beat == "2A30" ~ "Houston Heights, Norhill and Studemont",
                             beats$beat == "2A40" ~ "Old Sixth Ward and the First Ward",
                             beats$beat == "2A50" ~ "Memorial Park, West End, Ashbury and Cottage Grove",
                             beats$beat == "2A60" ~ "Westport, Holly Park, Lazy Brook, Citadel, Shady Acres and Timbergrove",
                             beats$beat == "3B10" ~ "Northwest Mall, Brookhollow, White Oak Acres, Oak Forest and Langwood",
                             beats$beat == "3B30" ~ "White Oak Acres, Brook Hollow, Oak Forest, Garden Oaks and Shepherd Park Plaza",
                             beats$beat == "3B40" ~ "Independence Heights, Melrose and Graceland",
                             beats$beat == "3B50" ~ "Northline Mall, Stratton Place, Oakwood, Colonial Gardens, Lindale and Junction City",
                             beats$beat == "6B10" ~ "York Plaza, Pembrook Place and Hawthorne Place",
                             beats$beat == "6B20" ~ "Highland Heights, Oakdale and Melrose Gardens",
                             beats$beat == "6B30" ~ "Woodland Trails, Oaks of Inwood, Bayou Bend, Cole Creek Manor and Inwood Forest",
                             beats$beat == "6B40" ~ "Highland Gardens, Highland Heights and North Plaza",
                             beats$beat == "22B30" ~ "Aldine and Imperial Valley",
                             beats$beat == "22B20" ~ "Greenspoint",
                             beats$beat == "7C10" ~ "Fifth Ward, North Park and Pinecrest",
                             beats$beat == "7C20" ~ "Huntington Place, Willshire Place, Kashmere Gardens and Liberty Gardens",
                             beats$beat == "7C30" ~ "Camden Woods, Archers Acres and Croyden Gardens",
                             beats$beat == "7C40" ~ "Keith-Weiss Park and Farrington Place",
                             beats$beat == "22B40" ~ "Intercontinental Airport, Northwood, Lakeview Park and Glen Lee",
                             beats$beat == "8C10" ~ "Tidwell Park, Shady Oak and Buckingham Place",
                             beats$beat == "8C20" ~ "Houston Gardens, Rosewood and Homestead",
                             beats$beat == "8C30" ~ "Parkhurst, East Houston and Clairmont Place",
                             beats$beat == "8C40" ~ "Lake Houston, Greenriver, Knoll and Lake Forest",
                             beats$beat == "8C50" ~ "Park North, Forest Green, Kentshire Place, Glenwood Forest and Dorchester",
                             beats$beat == "8C60" ~ "Edgeworth, Northwood Manor and Scenic Woods",
                             beats$beat == "9C10" ~ "Port of Houston, Clinton Park, Pleasantville and Fidelity Manor",
                             beats$beat == "9C20" ~ "Denver Harbor and Houston Harbor",
                             beats$beat == "9C30" ~ "City Park East, Wynnewood Acres and Wallisville Gardens",
                             beats$beat == "9C40" ~ "Greens Bayou Estates, Shady Brook, Northshore",
                             beats$beat == "24C10" ~ "Kingwood",
                             beats$beat == "24C20" ~ "Kingwood",
                             beats$beat == "24C30" ~ "Kingwood",
                             beats$beat == "24C40" ~ "Kingwood",
                             beats$beat == "24C50" ~ "Kingwood",
                             beats$beat == "24C60" ~ "Kingwood",
                             beats$beat == "11H10" ~ "Mason Park, Forest Hill, Sunnyland and Central Park",
                             beats$beat == "11H20" ~ "Harrisburg, Manchester and Pecan Park",
                             beats$beat == "11H30" ~ "Meadow Brook and Park Place",
                             beats$beat == "11H40" ~ "Allendale and Oak Meadows",
                             beats$beat == "11H50" ~ "Meadow Creek, Ferndale Addition and Glenbrook Industrial",
                             beats$beat == "10H10" ~ "Oak Lawn and Ranger",
                             beats$beat == "10H20" ~ "Eastwood and Broadmoor",
                             beats$beat == "10H30" ~ "Near East End",
                             beats$beat == "10H40" ~ "Midtown",
                             beats$beat == "10H50" ~ "the University of Houston area",
                             beats$beat == "10H60" ~ "University Oaks, Riverside Terrace and the Texas Southern University area",
                             beats$beat == "10H70" ~ "Texas Medical Center, Hermann Park, Houston Zoo and Miller Outdoor Theatre",
                             beats$beat == "10H80" ~ "Southampton, Rice University area, Shadow Lawn and Broad Acres",
                             beats$beat == "12D10" ~ "Almeda Mall, Southridge, Edgebrook and South Houston Gardens",
                             beats$beat == "12D20" ~ "Sage",
                             beats$beat == "12D30" ~ "Gulf Palms and Beamer",
                             beats$beat == "12D40" ~ "Scarsdale and Ellington Field",
                             beats$beat == "12D50" ~ "Bay Glen, Bay Oaks, Brook Forest and Bay Forest",
                             beats$beat == "12D60" ~ "NASA's Johnson Space Center, Meadow Green, Oakbrook West and Camino South",
                             beats$beat == "12D70" ~ "Baybrook Mall and Sterling Knoll",
                             beats$beat == "13D10" ~ "Gulfgate Mall, Bay Forest, Riverview, Hampshire, Golfcrest and Barnett Stadium",
                             beats$beat == "13D20" ~ "Fairlawn, Garden Villas, Glenbrook Valley and Santa Rosa",
                             beats$beat == "13D30" ~ "Allen Farms, Airport Gardens and the Hobby Airport area",
                             beats$beat == "13D40" ~ "Val Verde East, Gulf Meadows and Houston Skyscraper Shadows",
                             beats$beat == "14D10" ~ "McGregor Terrace and Riverside Terrace",
                             beats$beat == "14D20" ~ "Blueridge, Sunnyside, Brook Haven and Bayou Estates",
                             beats$beat == "14D30" ~ "Crestmont, Edgewood, Bellfort Park, Inwood Terrace and Southpark",
                             beats$beat == "14D40" ~ "South Acres, Shamrock Manor, Hillwood and Cloverland",
                             beats$beat == "14D50" ~ "Golden Glade Estates, Langston and Almeda Genoa",
                             beats$beat == "17E10" ~ "Southwest Memorial Hospital, Burnett Bayland Park and Westmoreland",
                             beats$beat == "17E20" ~ "the Houston Baptist University area and Maplewood West",
                             beats$beat == "17E30" ~ "Braeburn Valley West and Glenshire",
                             beats$beat == "17E40" ~ "Creekbend, Southwood and Fondren Southwest",
                             beats$beat == "15E10" ~ "Meyerland Plaza, Robindell, Meyerland, Braeburn Terrace and Maplewood South",
                             beats$beat == "15E20" ~ "Westbury, Park West and Willow Creek",
                             beats$beat == "15E30" ~ "Meyer Park, Post Oak Manor, Westwood, Willow Bend and Linkwood",
                             beats$beat == "15E40" ~ "The Astrodome, Six Flags Astroworld, Lakes at 610 and Plaza del Oro",
                             beats$beat == "16E10" ~ "Canterbury Village and Monarch Estates",
                             beats$beat == "16E20" ~ "Almeda Plaza, South Glen, Dumbarton Oaks and Briarwick",
                             beats$beat == "16E30" ~ "Greenpark, Southwest Crossing and Windsor Village",
                             beats$beat == "16E40" ~ "Chasewood, Willowpark, Briarwick, Ridgemont and Quail Run",
                             beats$beat == "19G10" ~ "Concourse South and Westwood Mall",
                             beats$beat == "19G20" ~ "Catalina Square, Bellaire West, Imperial Point",
                             beats$beat == "19G30" ~ "Rodeo Square, Wildflower Green, Braewood Glen",
                             beats$beat == "19G40" ~ "Huntington Village, Wellington Park, Leawood, Keegans Glen",
                             beats$beat == "19G50" ~ "Forum Park, Stancliff and Southwest Park",
                             beats$beat == "20G10" ~ "Westchase, Woodchase Village and Point West",
                             beats$beat == "20G20" ~ "Lakeside Estates, City West Place, Rivercrest and Briargrove Park",
                             beats$beat == "20G30" ~ "West Houston Medical Center, Andrau Airport, Westchase and Westchase Mall",
                             beats$beat == "20G40" ~ "Lakeview Forest, Lakeside Place, Ashford Village and Southlake",
                             beats$beat == "20G50" ~ "Nottingham, Yorkshire, Wilchester, Town and Country Village and Memorial City Shopping Center",
                             beats$beat == "20G60" ~ "Barkers Landing, Fleetwood, Thornwood and Darrel Tully Stadium",
                             beats$beat == "20G70" ~ "Westpark Village and Ashford Point",
                             beats$beat == "20G80" ~ "City Limits",
                             beats$beat == "5F10" ~ "Afton Village and Pine Terrace",
                             beats$beat == "5F20" ~ "Spring Branch and Schwartz Park",
                             beats$beat == "5F30" ~ "Fairbanks, Carverdale, Fawndale and Northwest Crossing",
                             beats$beat == "22B10" ~ "Willowbrook, Willow Chase Park and Centerfield",
                             beats$beat == "4F10" ~ "Timber Oaks, Shadow Oaks and Long Point Acres",
                             beats$beat == "4F20" ~ "Claymore Park, Westbranch, Spring Shadows and Kempwood Forest",
                             beats$beat == "4F30" ~ "Addicks Reservoir Area, Claymore Park, Brittmore Village, Park Ten Place and Mayde Creek Farms",
                             beats$beat == "18F10" ~ "Saddlebrook, Bayou Woods and Sherwood Forest",
                             beats$beat == "18F20" ~ "The Galleria, Tanglewood, Riverway and Post Oak",
                             beats$beat == "18F30" ~ "The Richmond Strip, Briargrove, Post Oak Estates and Westhaven",
                             beats$beat == "18F40" ~ "Rosewood General Hospital, Woodlake and Briarmeadow",
                             beats$beat == "18F50" ~ "Tanglewild, Sharpstown Mall and Regency Square",
                             beats$beat == "18F60" ~ "Sharpstown Country Club and Southway Center",
                             beats$beat == "21I10" ~ "Hobby Airport",
                             beats$beat == "21I20" ~ "Hobby Airport",
                             beats$beat == "21I30" ~ "Hobby Airport",
                             beats$beat == "21I40" ~ "Hobby Airport",                        beats$beat == "36E10" ~ "xxxx",
                             beats$beat == "21I50" ~ "Hobby Airport",
                             beats$beat == "21I60" ~ "Hobby Airport",
                             beats$beat == "21I70" ~ "Hobby Airport",
                             beats$beat == "23J40" ~ "Intercontinental Airport",    
                             beats$beat == "23J50" ~ "Intercontinental Airport",    
                             TRUE ~ "Unknown")


# Get demographic data for Census block groups to calculate population of beat areas
# Also transforming to match the projection of the Houston PD's beats spatial file
# This also reduces us down to just the numeric population est and geometry
blocks <- get_decennial(geography = "block", 
                       year = 2020,
                       output = 'wide',
                       variables = "P1_001N", 
                       state = "TX",
                       county = c("Harris County","Fort Bend","Montgomery"),
                       geometry = TRUE) %>%
  rename("population"="P1_001N") %>% 
  select(3) %>%
  janitor::clean_names() %>%
  st_transform(3857)

# Quick test of whether data is fit for interpolation
# library(areal)
# ar_validate(source = blockgroups, target = beats, varList = "population", method = "aw",verbose=TRUE)

# test to look at quick map of blockgroups
#poplabel <- paste0(blockgroups$population)
#map_blockgroups3 <- leaflet(blockgroups) %>% setView(-95.45, 29.75, zoom = 10) %>% 
#  addProviderTiles(provider = "CartoDB.Positron") %>% 
#  addPolygons(color = "green", label = poplabel)
#map_blockgroups3

# Calculate the estimated population of beat geographies/interpolate with tidycensus block groups
# Reminder: ext=true SUMS the population during interpolation; assumes pop is evenly distributed
beats_withpop <- st_interpolate_aw(blocks, beats, ext = TRUE)
# Drops geometry so it's not duplicated in the merge
beats_withpop <- st_drop_geometry(beats_withpop)
# Binds that new population column to the table
beats <- cbind(beats,beats_withpop)
# Cleans up unneeded calculation file
rm(beats_withpop, blockgroups)
# Round the population figure to nearest thousand as estimate
beats$population <- round(beats$population,-3)
# Eliminate population from beats on airport property showing population
beats$population <- ifelse(beats$population<1000,0,beats$population)

# Test aggregated population total for beats vs city population reported by Census
# sum(beats$population) # tally is 2.29M # city's reported pop is 2.32M in 2020; this works for our purposes

beats <- beats %>% st_transform(4326)
beats <- st_make_valid(beats)

# cleanup unneeded blocks file
rm(blocks)

st_write(beats,"data/source/geo/beats.geojson")
# add line  below when uploading data for pages
# beats <- st_read("data/source/geo/beats.geojson")
