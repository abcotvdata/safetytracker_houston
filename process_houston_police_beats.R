library(tidyverse)
library(tidycensus)
library(sp)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(tigris)
#library(areal)


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
# Also transforming to match the projection of the Houston PD's beats spatial file
blockgroups <- get_acs(geography = "block group", 
                       year = 2020,
                       output = 'wide',
                       variables = "B03002_001", 
                       state = "TX",
                       county = c("Harris County","Fort Bend","Montgomery"),
                       geometry = TRUE) %>%
st_transform(4326)



# ar_validate(source = blockgroups, target = beats, varList = "B03002_001E", method = "aw",verbose=TRUE)


# test to look at quick map of blockgroups
#poplabel <- paste0(blockgroups$B03002_001E)
#map_blockgroups3 <- leaflet(blockgroups) %>% setView(-95.45, 29.75, zoom = 10) %>% 
#  addProviderTiles(provider = "CartoDB.Positron") %>% 
#  addPolygons(color = "green", label = poplabel)
#map_blockgroups3

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
                        beats$beat == "22B30" ~ "Aldine, Imperial Valley",
                        beats$beat == "22B20" ~ "Greenspoint",
                        beats$beat == "7C10" ~ "Fifth Ward, North Park, Pinecrest",
                        beats$beat == "7C20" ~ "Huntington Place, Willshire Place, Kashmere Gardens, Liberty Gardens",
                        beats$beat == "7C30" ~ "Camden Woods, Archers Acres, Croyden Gardens",
                        beats$beat == "7C40" ~ "Keith-Weiss Park, Farrington Place",
                        beats$beat == "22B40" ~ "Intercontinental Airport, Northwood, Lakeview Park, Glen Lee",
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
                        beats$beat == "24C60" ~ "Kingwood",
                        beats$beat == "11H10" ~ "Mason Park, Forest Hill, Sunnyland, Central Park",
                        beats$beat == "11H20" ~ "Harrisburg, Manchester, Pecan Park",
                        beats$beat == "11H30" ~ "Meadow Brook, Park Place",
                        beats$beat == "11H40" ~ "Allendale, Oak Meadows",
                        beats$beat == "11H50" ~ "Meadow Creek, Ferndale Addition, Glenbrook Industrial",
                        beats$beat == "10H10" ~ "Oak Lawn, Ranger",
                        beats$beat == "10H20" ~ "Eastwood, Broadmoor",
                        beats$beat == "10H30" ~ "Near East End",
                        beats$beat == "10H40" ~ "Midtown",
                        beats$beat == "10H50" ~ "University of Houston area",
                        beats$beat == "10H60" ~ "University Oaks, Riverside Terrace, Texas Southern University area",
                        beats$beat == "10H70" ~ "Texas Medical Center, Hermann Park, Houston Zoo, Miller Outdoor Theatre",
                        beats$beat == "10H80" ~ "Southampton, Rice University area, Shadow Lawn, Broad Acres",
                        beats$beat == "12D10" ~ "Almeda Mall, Southridge, Edgebrook, South Houston Gardens",
                        beats$beat == "12D20" ~ "Sage",
                        beats$beat == "12D30" ~ "Gulf Palms, Beamer",
                        beats$beat == "12D40" ~ "Scarsdale, Ellington Field",
                        beats$beat == "12D50" ~ "Bay Glen, Bay Oaks, Brook Forest, Bay Forest",
                        beats$beat == "12D60" ~ "NASA's Johnson Space Center, Meadow Green, Oakbrook West, Camino South",
                        beats$beat == "12D70" ~ "Baybrook Mall, Sterling Knoll",
                        beats$beat == "13D10" ~ "Gulfgate Mall, Bay Forest, Riverview, Hampshire, Golfcrest, Barnett Stadium",
                        beats$beat == "13D20" ~ "Fairlawn, Garden Villas, Glenbrook Valley, Santa Rosa",
                        beats$beat == "13D30" ~ "Allen Farms, Airport Gardens, Hobby Airport area",
                        beats$beat == "13D40" ~ "Val Verde East, Gulf Meadows, Houston Skyscraper Shadows",
                        beats$beat == "14D10" ~ "McGregor Terrace, Riverside Terrace",
                        beats$beat == "14D20" ~ "Blueridge, Sunnyside, Brook Haven, Bayou Estates",
                        beats$beat == "14D30" ~ "Crestmont, Edgewood, Bellfort Park, Inwood Terrace, Southpark",
                        beats$beat == "14D40" ~ "South Acres, Shamrock Manor, Hillwood, Cloverland",
                        beats$beat == "14D50" ~ "Golden Glade Estates, Langston, Almeda Genoa",
                        beats$beat == "17E10" ~ "Southwest Memorial Hospital, Burnett Bayland Park, Westmoreland",
                        beats$beat == "17E20" ~ "Houston Baptist University area, Maplewood West",
                        beats$beat == "17E30" ~ "Braeburn Valley West, Glenshire",
                        beats$beat == "17E40" ~ "Creekbend, Southwood, Fondren Southwest",
                        beats$beat == "15E10" ~ "Meyerland Plaza, Robindell, Meyerland, Braeburn Terrace, Maplewood South",
                        beats$beat == "15E20" ~ "Westbury, Park West, Willow Creek",
                        beats$beat == "15E30" ~ "Meyer Park, Post Oak Manor, Westwood, Willow Bend, Linkwood",
                        beats$beat == "15E40" ~ "The Astrodome, Six Flags Astroworld, Lakes at 610, Plaza del Oro",
                        beats$beat == "16E10" ~ "Canterbury Village, Monarch Estates",
                        beats$beat == "16E20" ~ "Almeda Plaza, South Glen, Dumbarton Oaks, Briarwick",
                        beats$beat == "16E30" ~ "Greenpark, Southwest Crossing, Windsor Village",
                        beats$beat == "16E40" ~ "Chasewood, Willowpark, Briarwick, Ridgemont, Quail Run",
                        beats$beat == "19G10" ~ "Concourse South, Westwood Mall",
                        beats$beat == "19G20" ~ "Catalina Square, Bellaire West, Imperial Point",
                        beats$beat == "19G30" ~ "Rodeo Square, Wildflower Green, Braewood Glen",
                        beats$beat == "19G40" ~ "Huntington Village, Wellington Park, Leawood, Keegans Glen",
                        beats$beat == "19G50" ~ "Forum Park, Stancliff, Southwest Park",
                        beats$beat == "20G10" ~ "Westchase, Woodchase Village, Point West",
                        beats$beat == "20G20" ~ "Lakeside Estates, City West Place, Rivercrest, Briargrove Park",
                        beats$beat == "20G30" ~ "West Houston Medical Center, Andrau Airport, Westchase, Westchase Mall",
                        beats$beat == "20G40" ~ "Lakeview Forest, Lakeside Place, Ashford Village, Southlake",
                        beats$beat == "20G50" ~ "Nottingham, Yorkshire, Wilchester, Town and Country Village, Memorial City Shopping Center",
                        beats$beat == "20G60" ~ "Barkers Landing, Fleetwood, Thornwood, Darrel Tully Stadium",
                        beats$beat == "20G70" ~ "Westpark Village, Ashford Point",
                        beats$beat == "20G80" ~ "City Limits",
                        beats$beat == "5F10" ~ "Afton Village, Pine Terrace",
                        beats$beat == "5F20" ~ "Spring Branch, Schwartz Park",
                        beats$beat == "5F30" ~ "Fairbanks, Carverdale, Fawndale, Northwest Crossing",
                        beats$beat == "22B10" ~ "Willowbrook, Willow Chase Park, Centerfield",
                        beats$beat == "4F10" ~ "Timber Oaks, Shadow Oaks, Long Point Acres",
                        beats$beat == "4F20" ~ "Claymore Park, Westbranch, Spring Shadows, Kempwood Forest",
                        beats$beat == "4F30" ~ "Addicks Reservoir Area, Claymore Park, Brittmore Village, Park Ten Place, Mayde Creek Farms",
                        beats$beat == "18F10" ~ "Saddlebrook, Bayou Woods, Sherwood Forest",
                        beats$beat == "18F20" ~ "The Galleria, Tanglewood, Riverway, Post Oak",
                        beats$beat == "18F30" ~ "The Richmond Strip, Briargrove, Post Oak Estates, Westhaven",
                        beats$beat == "18F40" ~ "Rosewood General Hospital, Woodlake, Briarmeadow",
                        beats$beat == "18F50" ~ "Tanglewild, Sharpstown Mall, Regency Square",
                        beats$beat == "18F60" ~ "Sharpstown Country Club, Southway Center",
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




# Calculate District population totals to review vs. Houston PD web site
district_pops <- beats %>%
  group_by(district) %>%
  summarise(pop=sum(population),
            sqmi = sum(area_sq_mi)) %>% st_drop_geometry(district_pops)

