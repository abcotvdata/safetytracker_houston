# var2020 <- load_variables(2020, "pl")

var2010 <- load_variables(2010, "pl")

# Get demographic data for Census block groups to aggregate/apportion to beats geography
blocks <- get_decennial(geography = "block", 
                        year = 2020,
                        sumfile="pl",
                        output = 'wide',
                        variables = "P2_001N", 
                        state = "TX",
                        county = c("Harris County","Fort Bend","Montgomery"),
                        geometry = TRUE)
# Transform tidycensus data to match beats projection
blocks <- st_transform(blocks, 4326)
# Grab just the population and geometry from blockgroups from tidycensus
popblocks <- blocks["P2_001N"] # keep only the values to be transferred

# Calculate the estimated population of beat geographies/interpolate with tidycensus bgs
# Reminder: ext=true SUMS the population during interpolation
beats_withpopblocks <- st_interpolate_aw(popblocks, beats, ext = TRUE)



# Binds that new population column to the table
beats <- cbind(beats,beats_withpopblocks) %>% select(1:13)
# Two quick checks that we've got this right
# Checking that the geographies merged correct
# ifelse(beats$geometry == beats$geometry.1,"yes","no")
# Check total population assigned/estimated across all beats
sum(beats$P2_001N) # tally is 2.29M # city's reported pop is 2.30M in 2019; this works for our purposes
# Trim and clean beats file to just what we need
beats <- beats %>%
  rename("population"="P2_001N","beat"="Beats") %>%
  select(beat,Area_sq_mi,population,geometry) %>% 
  janitor::clean_names()
beats$population <- round(beats$population,0)
# beats$population <- ifelse(beats$population<2500,0,beats$population)
# rm(beats_withpopblocks)
sum(beats$population)

