library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.providers)
# library(tidyr)
library(sf)

# Get demographic data and geography for Census places
tx_places <- get_decennial(geography = "place", 
                            year = 2020,
                            output = 'wide',
                            variables = "P1_001N", 
                            state = "TX",
                            geometry = TRUE) %>%
  rename("population"="P1_001N") %>%
  janitor::clean_names()

# Get demographic data and geography for Census places
tx_counties <- get_decennial(geography = "county", 
                              year = 2020,
                              output = 'wide',
                              variables = "P1_001N", 
                              state = "TX",
                              geometry = TRUE) %>%
  rename("population"="P1_001N") %>%
  janitor::clean_names()

# Some adjustments to the fields in tx_places to merge to crime data
tx_places$place <- tx_places$name
tx_places$place <- str_replace(tx_places$place," CDP, Texas","")
tx_places$place <- str_replace(tx_places$place," city, Texas","")
tx_places$place <- str_replace(tx_places$place," town, Texas","")
tx_places$place <- str_replace(tx_places$place," village, Texas","")

tx_places$place <- sub(" CDP.*", "\\1", tx_places$place)
tx_places$place <- sub(" city.*", "\\1", tx_places$place)
tx_places$place <- sub(" town.*", "\\1", tx_places$place)

tx_places$state <- "Texas"

#tx_places$place <- str_replace(tx_places$place,"Big Bear City","Big Bear")
#tx_places$place <- str_replace(tx_places$place,"La Cañada Flintridge","La Canada-Flintridge")
#tx_places$place <- ifelse(tx_places$geoid=="0665042","Ventura",tx_places$place)

# Some adjustments to the fields in tx_counties to merge to crime data
tx_counties$county <- tx_counties$name
tx_counties$county <- str_replace(tx_counties$county," County, Texas","")

# Assign county names for filter and temporarily import Cal DOJ list for filter
counties <- c("Harris", "Montgomery", "Liberty", "Waller", "Fort Bend", "Brazoria", "Galveston", "Chambers")

socal_annual <- readRDS("scripts/rds/socal_annual.rds")
# Apply filters to assign places to counties, filter for counties and state data agencies
ourtx_places <- st_join(tx_places, tx_counties %>% select(5), left = FALSE, largest = TRUE) %>%
  filter(county %in% counties)
# Round population to estimate, nearest hundred, consistent with Houston PD beats' estimates
ourtx_places$population <- round(ourtx_places$population,-2)

# Creating a singular file for making rural cutouts by county
# make sure to make the resulting file a valid sf file
all_ourtx_places <- ourtx_places %>%
  group_by(state) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() %>% st_make_valid()

# Create five single polygon county files
riverside_county <- tx_counties %>% filter(county=="Riverside County") %>% st_make_valid()
orange_county <- tx_counties %>% filter(county=="Orange County") %>% st_make_valid()
sanbern_county <- tx_counties %>% filter(county=="San Bernardino County") %>% st_make_valid()
ventura_county <- tx_counties %>% filter(county=="Ventura County") %>% st_make_valid()

# Make the rural "remnant" area polygons for each county
rural_riverside <- st_difference(riverside_county,all_sotx_places)
rural_orange <- st_difference(orange_county,all_sotx_places)
rural_sanbern <- st_difference(sanbern_county,all_sotx_places)
rural_ventura <- st_difference(ventura_county,all_sotx_places)

# Make the rural "remnant" area polygons for each county
rural_riverside$place <- "Riverside Co. Sheriff's Department"
rural_orange$place <- "Orange Co. Sheriff's Department"
rural_sanbern$place <- "San Bernardino Co. Sheriff's Department"
rural_ventura$place <- "Ventura Co. Sheriff's Department"

# Make the rural "remnant" area polygons for each county
socal_county_pops <- sotx_places %>% group_by(county) %>% summarise(pop=sum(population))
rural_riverside$population <- riverside_county$population - 2027000
rural_orange$population <- orange_county$population - 3053900
rural_sanbern$population <- sanbern_county$population - 1888800
rural_ventura$population <- ventura_county$population - 750000

# Add these rural sheriff's coverage areas back into sotx_places
sotx_places <- rbind(sotx_places,rural_riverside,rural_orange,rural_sanbern,rural_ventura)

# Do some cleanup 
rm(rural_riverside,rural_orange,rural_sanbern,rural_ventura)
rm(riverside_county,orange_county,sanbern_county,ventura_county)
rm(tx_counties, tx_places, all_sotx_places,socal_county_pops)

# Create new police_map
# la_districts <- readRDS(data/rds/la_county_police_districts.rds)
# Remove all LA County places from the so_cal places now
police_map <- bind_rows(sotx_places %>% filter(county!="Los Angeles County"),la_districts)
# Add upgraded LA County districts to the new region wide police districts map
police_map$county <- ifelse(is.na(police_map$county),"Los Angeles County",police_map$county)
# Recode place field for all the LA county policing districts
police_map$place <- case_when(police_map$agency=="OTHER" & police_map$commtype=="City" ~ police_map$place_name,
                              police_map$agency=="LASD" & police_map$commtype=="City" ~ police_map$place_name,
                              police_map$agency=="LASD" & police_map$commtype=="Unincorporated" ~ paste(police_map$place_name, police_map$district),
                              police_map$agency=="LAPD" ~ paste(police_map$agency, police_map$district),
                              TRUE ~ police_map$place)


# Set bins for beats pop map
popbins <- c(0,1000,5000,25000,50000,75000,100000,125000,150000,175000,200000,Inf)
poppal <- colorBin("viridis", police_map$population, bins = popbins)
poplabel <- paste(sep = "<br>", police_map$place,police_map$county,police_map$agency,police_map$commtype,prettyNum(police_map$population, big.mark = ","))
# Create map
socal_police_map <- leaflet(police_map) %>%
  setView(-117.243, 33.70, zoom = 10) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "CartoDB.PositronOnlyLabels") %>%
  addPolygons(color = "white", popup = poplabel, weight = 2, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.5,
              fillColor = ~poppal(`population`)) 
socal_police_map 

# Saving final product for reuse; police map includes 4 counties + LA Co
saveRDS(police_map,"scripts/rds/police_map.rds")
# Saving the file with only the police districts in the 4 counties besides LA too
saveRDS(sotx_places,"scripts/rds/police_map.rds")

rm(socal_police_map)
