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

#### OPEN WORK TO GET A CRIME FILE HERE FOR TEXAS
#ourtx_annual <- readRDS("scripts/rds/ourtx_annual.rds")

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
harris_county <- tx_counties %>% filter(county=="Harris") %>% st_make_valid()
montgomery_county <- tx_counties %>% filter(county=="Montgomery") %>% st_make_valid()
liberty_county <- tx_counties %>% filter(county=="Liberty") %>% st_make_valid()
waller_county <- tx_counties %>% filter(county=="Waller") %>% st_make_valid()
fortbend_county <- tx_counties %>% filter(county=="Fort Bend") %>% st_make_valid()
brazoria_county <- tx_counties %>% filter(county=="Brazoria") %>% st_make_valid()
galveston_county <- tx_counties %>% filter(county=="Galveston") %>% st_make_valid()
chambers_county <- tx_counties %>% filter(county=="Chambers") %>% st_make_valid()


# Make the rural "remnant" area polygons for each county
rural_harris <- st_difference(harris_county,all_ourtx_places)
rural_montgomery <- st_difference(montgomery_county,all_ourtx_places)
rural_liberty <- st_difference(liberty_county,all_ourtx_places)
rural_waller <- st_difference(waller_county,all_ourtx_places)
rural_fortbend <- st_difference(fortbend_county,all_ourtx_places)
rural_brazoria <- st_difference(brazoria_county,all_ourtx_places)
rural_galveston <- st_difference(galveston_county,all_ourtx_places)
rural_chambers <- st_difference(chambers_county,all_ourtx_places)

# Make the rural "remnant" area polygons for each county
rural_harris$place <- "Harris Co. Sheriff's Department"
rural_montgomery$place <- "Montgomery Co. Sheriff's Department"
rural_liberty$place <- "Liberty Co. Sheriff's Department"
rural_waller$place <- "Waller Co. Sheriff's Department"
rural_fortbend$place <- "Fort Bend Co. Sheriff's Department"
rural_brazoria$place <- "Brazoria Co. Sheriff's Department"
rural_galveston$place <- "Galveston Co. Sheriff's Department"
rural_chambers$place <- "Chambers Co. Sheriff's Department"

# Make the rural "remnant" area polygons for each county
ourtx_county_pops <- ourtx_places %>% group_by(county) %>% summarise(pop=sum(population))
rural_harris$population <- harris_county$population - 2027000
rural_montgomery$population <- montgomery_county$population - 3053900
rural_liberty$population <- liberty_county$population - 1888800
rural_waller$population <- waller_county$population - 750000
rural_fortbend$population <- fortbend_county$population - 2027000
rural_brazoria$population <- brazoria_county$population - 3053900
rural_galveston$population <- galveston_county$population - 1888800
rural_chambers$population <- chambers_county$population - 750000

# Add these rural sheriff's coverage areas back into ourtx_places
ourtx_places <- rbind(ourtx_places,rural_harris,rural_montgomery,rural_liberty,rural_waller,rural_fortbend,
                      rural_brazoria,rural_galveston,rural_chambers) %>%
  st_transform(4326)

# Do some cleanup 
rm(rural_harris,rural_montgomery,rural_liberty,rural_waller,rural_fortbend,rural_brazoria,rural_galveston,rural_chambers)
rm(harris_county,montgomery_county,liberty_county,waller_county,fortbend_county,brazoria_county,galveston_county,chambers_county)
rm(tx_counties, tx_places, all_ourtx_places, ourtx_county_pops)

# Create new police_map
# la_districts <- readRDS(data/rds/la_county_police_districts.rds)
# Remove all LA County places from the so_cal places now
police_map <- bind_rows(ourtx_places %>% filter(place!="Houston"),beats)
# Add upgraded LA County districts to the new region wide police districts map
police_map$county <- ifelse(is.na(police_map$county),"Harris",police_map$county)
# Recode place field for all the LA county policing districts
#police_map$place <- case_when(police_map$agency=="OTHER" & police_map$commtype=="City" ~ police_map$place_name,
#                              police_map$agency=="LASD" & police_map$commtype=="City" ~ police_map$place_name,
#                              police_map$agency=="LASD" & police_map$commtype=="Unincorporated" ~ paste(police_map$place_name, police_map$district),
#                              police_map$agency=="LAPD" ~ paste(police_map$agency, police_map$district),
#                              TRUE ~ police_map$place)

# Set bins for beats pop map
popbins <- c(0,1000,5000,25000,50000,75000,100000,125000,150000,175000,200000,Inf)
poppal <- colorBin("viridis", police_map$population, bins = popbins)
poplabel <- paste(sep = "<br>", police_map$place,police_map$county,prettyNum(police_map$population, big.mark = ","))
# Create map
ourtx_police_map <- leaflet(police_map) %>%
  setView(-95.45, 29.75, zoom = 9) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "CartoDB.PositronOnlyLabels") %>%
  addPolygons(color = "white", popup = poplabel, weight = 2, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.5,
              fillColor = ~poppal(`population`)) 
ourtx_police_map 

# Saving final product for reuse; police map includes 4 counties + LA Co
saveRDS(police_map,"scripts/rds/police_map.rds")

rm(socal_police_map)
