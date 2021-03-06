library(tidyverse)
library(tidycensus)
library(readxl)
library(sf)
library(zoo)


# Download and save for backup the latest posted files for 2021, 2020 and 2019
# download.file("https://www.houstontx.gov/police/cs/xls/NIBRSPublicViewDec21.xlsx","data/latest/houston_NIBRS2021.xlsx")
# download.file("https://www.houstontx.gov/police/cs/xls/NIBRSPublicView.Jan1-Dec31-2020.xlsx","data/latest/houston_NIBRS2020.xlsx")
# download.file("https://www.houstontx.gov/police/cs/xls/2019_NIBRSPublicView.Jan1-Dec31.xlsx","data/latest/houston_NIBRS2019.xlsx")
# download.file("https://www.houstontx.gov/police/cs/xls/NIBRSPublicViewJan-May22.xlsx","data/latest/houston_NIBRS2022.xlsx")

# Import and combine 4 years' of crime data; note 2022 changes monthly

# Read in the 2022 file, renaming to cols to standardize across all years
houston22 <- read_excel("data/latest/houston_NIBRS2022.xlsx", 
                        col_types = c("text", "date", "numeric", 
                                      "text", "text", "numeric", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text","numeric","numeric"))
names(houston22) <- c("incident", "date", "hour", 
                      "nibrs_class", "offense_type", "offense_count", "beat", 
                      "premise", "street_no", "street_name", "street_type", "street_suffix", 
                      "city", "zip","longitude","latitude")

# Read in the 2021 file, renaming to cols to standardize across 3 years
houston21 <- read_excel("data/latest/houston_NIBRS2021.xlsx", 
                        col_types = c("text", "date", "numeric", 
                                      "text", "text", "numeric", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text"))
names(houston21) <- c("incident", "date", "hour", 
                      "nibrs_class", "offense_type", "offense_count", "beat", 
                      "premise", "street_no", "street_name", "street_type", "street_suffix", 
                      "city", "zip")

# Read in the 2020 file, renaming to cols to standardize
houston20 <- read_excel("data/latest/houston_NIBRS2020.xlsx", 
                        col_types = c("text", "date", "numeric", 
                                      "text", "text", "numeric", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text"))
names(houston20) <- c("incident", "date", "hour", 
                      "nibrs_class", "offense_type", "offense_count", "beat", 
                      "premise", "street_no", "street_name", "street_type", "street_suffix", 
                      "city", "zip")

# Read in the 2019 file, renaming to cols to standardize
houston19 <- read_excel("data/latest/houston_NIBRS2019.xlsx", 
                        col_types = c("text", "date", "numeric", 
                                      "text", "text", "numeric", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text"))
names(houston19) <- c("incident", "date", "hour", 
                      "nibrs_class", "offense_type", "offense_count", "beat", 
                      "premise", "street_no", "street_name", "street_type", "street_suffix", 
                      "city", "zip")

# Calculate the multiplier for the 2022 data
# Based on number days of data in newest 2022 file
days_so_far_2022 <- n_distinct(houston22$date)
# projected_multiplier <- 1/(days_so_far_2022/365)

# Combine all four years' of into single table, then delete yearlies
houston_crime <- bind_rows(houston19,houston20,houston21,houston22)
rm(houston19,houston20,houston21,houston22)
houston_crime$year <- substr(houston_crime$date,1,4)
houston_crime$date <- as.Date(houston_crime$date, "%Y-%m-%d")
# adds a month reference point for grouping for monthly trend figures
houston_crime$month <- lubridate::floor_date(as.Date(houston_crime$date),"month")

# Build a class-code table to classify offense types and categories
classcodes <- houston_crime %>%
  group_by(offense_type,nibrs_class) %>%
  summarise(number_offense_type=n())
classcodes$category_code <- substr(classcodes$nibrs_class,1,2)
classcodes$category_name <- case_when(classcodes$category_code == "09" ~ "Murder",
                                      classcodes$category_code == "10" ~ "Kidnapping",
                                      classcodes$category_code == "11" ~ "Sexual Assault",
                                      classcodes$category_code == "12" ~ "Robbery",
                                      classcodes$category_code == "13" ~ "Assault",
                                      classcodes$category_code == "20" ~ "Arson",
                                      classcodes$category_code == "22" ~ "Burglary",
                                      classcodes$category_code == "23" ~ "Theft",
                                      classcodes$category_code == "24" ~ "Auto Theft",
                                      classcodes$category_code == "28" ~ "Theft",
                                      classcodes$category_code == "35" ~ "Drug Offenses",
                                      TRUE ~ "Other")
# add violent and property flags
classcodes$type <- case_when(classcodes$category_code == "09" ~ "Violent",
                             classcodes$category_code == "11" ~ "Violent",
                             classcodes$category_code == "12" ~ "Violent",
                             classcodes$category_code == "20" ~ "Property",
                             classcodes$category_code == "22" ~ "Property",
                             classcodes$category_code == "23" ~ "Property",
                             classcodes$category_code == "24" ~ "Property",
                             classcodes$category_code == "28" ~ "Property",
                             classcodes$nibrs_class == "13A" ~ "Violent",
                             classcodes$nibrs_class == "13B" ~ "Violent",
                             TRUE ~ "Other")
# add flag for 'major' crimes, generally considered 'Part I' crimes in FBI data
classcodes$major <- case_when(classcodes$category_code == "09" ~ "Major",
                              classcodes$category_code == "11" ~ "Major",
                              classcodes$category_code == "12" ~ "Major",
                              classcodes$category_code == "20" ~ "Major",
                              classcodes$category_code == "22" ~ "Major",
                              classcodes$category_code == "23" ~ "Major",
                              classcodes$category_code == "24" ~ "Major",
                              classcodes$category_code == "28" ~ "Major",
                              classcodes$nibrs_class == "13A" ~ "Major",
                              TRUE ~ "Other")

# Add categories,types from classcodes to the individual crime records
houston_crime <- left_join(houston_crime,classcodes %>% select(2,4:7),by=c("nibrs_class","offense_type"))
# If beat is blank, add word Unknown
houston_crime$beat[is.na(houston_crime$beat)] <- "Unknown"
# Fix non-existent beat 15E11, which should be 15E10
houston_crime$beat <- ifelse(houston_crime$beat == "15E11","15E10",houston_crime$beat)
# Fix beats in District 5, 6 and 7 that became District 22
houston_crime$beat <- ifelse(houston_crime$beat == "5F40","22B10",houston_crime$beat)
houston_crime$beat <- ifelse(houston_crime$beat == "6B50","22B30",houston_crime$beat)
houston_crime$beat <- ifelse(houston_crime$beat == "6B60","22B20",houston_crime$beat)
houston_crime$beat <- ifelse(houston_crime$beat == "7C50","22B40",houston_crime$beat)

# write csv of houston crime as a backup
write_csv(houston_crime,"data/latest/houston_crime.csv")

# pull last 12 months of raw crimes
houston_crime_last12 <- houston_crime %>% filter(date>max(houston_crime$date)-365)

# CITYWIDE CRIME TOTALS
# Calculate of each detailed offense type CITYWIDE
citywide_detailed <- houston_crime %>%
  group_by(offense_type,nibrs_class,year) %>%
  summarise(count = sum(offense_count)) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_detailed <- citywide_detailed %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
citywide_detailed_last12 <- houston_crime_last12 %>%
  group_by(offense_type,nibrs_class) %>%
  summarise(last12mos = sum(offense_count))
citywide_detailed <- left_join(citywide_detailed,citywide_detailed_last12,by=c("offense_type","nibrs_class"))
# add zeros where there were no crimes tallied that year
citywide_detailed[is.na(citywide_detailed)] <- 0
# Calculate a total across the 3 prior years
citywide_detailed$total_prior3years <- citywide_detailed$total19+citywide_detailed$total20+citywide_detailed$total21
citywide_detailed$avg_prior3years <- round(citywide_detailed$total_prior3years/3,1)
# calculate increases
citywide_detailed$inc_19to21 <- round(citywide_detailed$total21/citywide_detailed$total19*100-100,1)
citywide_detailed$inc_19tolast12 <- round(citywide_detailed$last12mos/citywide_detailed$total19*100-100,1)
citywide_detailed$inc_21tolast12 <- round(citywide_detailed$last12mos/citywide_detailed$total21*100-100,1)
citywide_detailed$inc_prior3yearavgtolast12 <- round((citywide_detailed$last12mos/citywide_detailed$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_detailed$rate19 <- round(citywide_detailed$total19/2304580*100000,1)
citywide_detailed$rate20 <- round(citywide_detailed$total20/2304580*100000,1)
citywide_detailed$rate21 <- round(citywide_detailed$total21/2304580*100000,1)
citywide_detailed$rate_last12 <- round(citywide_detailed$last12mos/2304580*100000,1)
# calculate a multiyear rate
citywide_detailed$rate_prior3years <- round(citywide_detailed$avg_prior3years/2304580*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
citywide_detailed <- citywide_detailed %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
citywide_detailed <- citywide_detailed %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# Calculate of each detailed offense type CITYWIDE
citywide_detailed_monthly <- houston_crime %>%
  group_by(offense_type,nibrs_class,month) %>%
  summarise(count = sum(offense_count)) %>% 
  filter(nibrs_class=="09A") 
# add rolling average of 3 months for chart trend line & round to clean
citywide_detailed_monthly <- citywide_detailed_monthly %>%
  dplyr::mutate(rollavg_3month = rollsum(count, k = 3, fill = NA, align = "right")/3)
citywide_detailed_monthly$rollavg_3month <- round(citywide_detailed_monthly$rollavg_3month,0)
# write to save for charts
write_csv(citywide_detailed_monthly,"murders_monthly.csv")

# Calculate of each category of offense CITYWIDE
citywide_category <- houston_crime %>%
  group_by(category_name,year) %>%
  summarise(count = sum(offense_count)) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_category <- citywide_category %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
citywide_category_last12 <- houston_crime_last12 %>%
  group_by(category_name) %>%
  summarise(last12mos = sum(offense_count))
citywide_category <- left_join(citywide_category,citywide_category_last12,by=c("category_name"))
# add zeros where there were no crimes tallied that year
citywide_category[is.na(citywide_category)] <- 0
# Calculate a total across the 3 prior years
citywide_category$total_prior3years <- citywide_category$total19+citywide_category$total20+citywide_category$total21
citywide_category$avg_prior3years <- round(citywide_category$total_prior3years/3,1)
# calculate increases
citywide_category$inc_19to21 <- round(citywide_category$total21/citywide_category$total19*100-100,1)
citywide_category$inc_19tolast12 <- round(citywide_category$last12mos/citywide_category$total19*100-100,1)
citywide_category$inc_21tolast12 <- round(citywide_category$last12mos/citywide_category$total21*100-100,1)
citywide_category$inc_prior3yearavgtolast12 <- round((citywide_category$last12mos/citywide_category$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_category$rate19 <- round(citywide_category$total19/2304580*100000,1)
citywide_category$rate20 <- round(citywide_category$total20/2304580*100000,1)
citywide_category$rate21 <- round(citywide_category$total21/2304580*100000,1)
citywide_category$rate_last12 <- round(citywide_category$last12mos/2304580*100000,1)
# calculate a multiyear rate
citywide_category$rate_prior3years <- round(citywide_category$avg_prior3years/2304580*100000,1)

# Calculate monthly totals for categories of crimes CITYWIDE
citywide_category_monthly <- houston_crime %>%
  group_by(category_name,month) %>%
  summarise(count = sum(offense_count))
# add rolling average of 3 months for chart trend line & round to clean
citywide_category_monthly <- citywide_category_monthly %>%
  arrange(category_name,month) %>%
  dplyr::mutate(rollavg_3month = rollsum(count, k = 3, fill = NA, align = "right")/3)
citywide_category_monthly$rollavg_3month <- round(citywide_category_monthly$rollavg_3month,0)
# write series of monthly files for charts
citywide_category_monthly %>% filter(category_name=="Sexual Assault") %>% write_csv("sexassaults_monthly.csv")
citywide_category_monthly %>% filter(category_name=="Auto Theft") %>% write_csv("autothefts_monthly.csv")
citywide_category_monthly %>% filter(category_name=="Theft") %>% write_csv("thefts_monthly.csv")
citywide_category_monthly %>% filter(category_name=="Burglary") %>% write_csv("burglaries_monthly.csv")
citywide_category_monthly %>% filter(category_name=="Robbery") %>% write_csv("robberies_monthly.csv")
citywide_category_monthly %>% filter(category_name=="Assault") %>% write_csv("assaults_monthly.csv")
citywide_category_monthly %>% filter(category_name=="Drug Offenses") %>% write_csv("drugs_monthly.csv")




# Calculate of each type of crime CITYWIDE
citywide_type <- houston_crime %>%
  group_by(type,year) %>%
  summarise(count = sum(offense_count)) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_type <- citywide_type %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
citywide_type_last12 <- houston_crime_last12 %>%
  group_by(type) %>%
  summarise(last12mos = sum(offense_count))
citywide_type <- left_join(citywide_type,citywide_type_last12,by=c("type"))
# Calculate a total across the 3 prior years
citywide_type$total_prior3years <- citywide_type$total19+citywide_type$total20+citywide_type$total21
citywide_type$avg_prior3years <- round(citywide_type$total_prior3years/3,1)
# add zeros where there were no crimes tallied that year
citywide_type[is.na(citywide_type)] <- 0
# calculate increases
citywide_type$inc_19to21 <- round(citywide_type$total21/citywide_type$total19*100-100,1)
citywide_type$inc_19tolast12 <- round(citywide_type$last12mos/citywide_type$total19*100-100,1)
citywide_type$inc_21tolast12 <- round(citywide_type$last12mos/citywide_type$total21*100-100,1)
citywide_type$inc_prior3yearavgtolast12 <- round((citywide_type$last12mos/citywide_type$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_type$rate19 <- round(citywide_type$total19/2304580*100000,1)
citywide_type$rate20 <- round(citywide_type$total20/2304580*100000,1)
citywide_type$rate21 <- round(citywide_type$total21/2304580*100000,1)
citywide_type$rate_last12 <- round(citywide_type$last12mos/2304580*100000,1)
# calculate a multiyear rate
citywide_type$rate_prior3years <- round(citywide_type$avg_prior3years/2304580*100000,1)

# MERGE WITH BEATS GEOGRAPHY AND POPULATION
# Geography and populations processed separately in 
# source(process_houston_police_beats.R)
# Test that all beats show in data and identify beat #s that do not
# beatsindata <- houston_crime %>% group_by(beat,year) %>% summarise(count=n()) %>% pivot_wider(names_from=year, values_from=count)
# anti_join(beatsindata,beats,by="beat")

# BY POLICE BEAT
# Calculate total of each detailed offense type BY POLICE BEAT
beat_detailed <- houston_crime %>%
  group_by(beat,offense_type,nibrs_class,year) %>%
  summarise(count = sum(offense_count)) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
beat_detailed <- beat_detailed %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
beat_detailed_last12 <- houston_crime_last12 %>%
  group_by(beat,offense_type,nibrs_class) %>%
  summarise(last12mos = sum(offense_count))
beat_detailed <- left_join(beat_detailed,beat_detailed_last12,by=c("beat","offense_type","nibrs_class"))
rm(beat_detailed_last12)
# add zeros where there were no crimes tallied that year
beat_detailed[is.na(beat_detailed)] <- 0
# Calculate a total across the 3 prior years
beat_detailed$total_prior3years <- beat_detailed$total19+beat_detailed$total20+beat_detailed$total21
beat_detailed$avg_prior3years <- round(beat_detailed$total_prior3years/3,1)
# calculate increases
beat_detailed$inc_19to21 <- round(beat_detailed$total21/beat_detailed$total19*100-100,1)
beat_detailed$inc_19tolast12 <- round(beat_detailed$last12mos/beat_detailed$total19*100-100,1)
beat_detailed$inc_21tolast12 <- round(beat_detailed$last12mos/beat_detailed$total21*100-100,1)
beat_detailed$inc_prior3yearavgtolast12 <- round((beat_detailed$last12mos/beat_detailed$avg_prior3years)*100-100,0)
# add population for beats
beat_detailed <- full_join(beats,beat_detailed,by="beat") 
# calculate the beat by beat rates PER 1K people
beat_detailed$rate19 <- round(beat_detailed$total19/beat_detailed$population*100000,1)
beat_detailed$rate20 <- round(beat_detailed$total20/beat_detailed$population*100000,1)
beat_detailed$rate21 <- round(beat_detailed$total21/beat_detailed$population*100000,1)
beat_detailed$rate_last12 <- round(beat_detailed$last12mos/beat_detailed$population*100000,1)
# calculate a multiyear rate
beat_detailed$rate_prior3years <- round(beat_detailed$avg_prior3years/beat_detailed$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
beat_detailed <- beat_detailed %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
beat_detailed <- beat_detailed %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# Calculate total of each category of offense BY POLICE BEAT
beat_category <- houston_crime %>%
  group_by(beat,category_name,year) %>%
  summarise(count = sum(offense_count)) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
beat_category <- beat_category %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
beat_category_last12 <- houston_crime_last12 %>%
  group_by(beat,category_name) %>%
  summarise(last12mos = sum(offense_count))
beat_category <- left_join(beat_category,beat_category_last12,by=c("beat","category_name"))
rm(beat_category_last12)
# add zeros where there were no crimes tallied that year
beat_category[is.na(beat_category)] <- 0
# Calculate a total across the 3 prior years
beat_category$total_prior3years <- beat_category$total19+beat_category$total20+beat_category$total21
beat_category$avg_prior3years <- round(beat_category$total_prior3years/3,1)
# calculate increases
beat_category$inc_19to21 <- round(beat_category$total21/beat_category$total19*100-100,1)
beat_category$inc_19tolast12 <- round(beat_category$last12mos/beat_category$total19*100-100,1)
beat_category$inc_21tolast12 <- round(beat_category$last12mos/beat_category$total21*100-100,1)
beat_category$inc_prior3yearavgtolast12 <- round((beat_category$last12mos/beat_category$avg_prior3years)*100-100,0)
# add population for beats
beat_category <- full_join(beats,beat_category,by="beat") 
# calculate the beat by beat rates PER 1K people
beat_category$rate19 <- round(beat_category$total19/beat_category$population*100000,1)
beat_category$rate20 <- round(beat_category$total20/beat_category$population*100000,1)
beat_category$rate21 <- round(beat_category$total21/beat_category$population*100000,1)
beat_category$rate_last12 <- round(beat_category$last12mos/beat_category$population*100000,1)
# calculate a multiyear rate
beat_category$rate_prior3years <- round(beat_category$avg_prior3years/beat_category$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
beat_category <- beat_category %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
beat_category <- beat_category %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# Calculate total of each type of crime BY POLICE BEAT
beat_type <- houston_crime %>%
  group_by(beat,type,year) %>%
  summarise(count = sum(offense_count)) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
beat_type <- beat_type %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
beat_type_last12 <- houston_crime_last12 %>%
  group_by(beat,type) %>%
  summarise(last12mos = sum(offense_count))
beat_type <- left_join(beat_type,beat_type_last12,by=c("beat","type"))
rm(beat_type_last12)
# add zeros where there were no crimes tallied that year
beat_type[is.na(beat_type)] <- 0
# Calculate a total across the 3 prior years
beat_type$total_prior3years <- beat_type$total19+beat_type$total20+beat_type$total21
beat_type$avg_prior3years <- round(beat_type$total_prior3years/3,1)
# calculate increases
beat_type$inc_19to21 <- round(beat_type$total21/beat_type$total19*100-100,1)
beat_type$inc_19tolast12 <- round(beat_type$last12mos/beat_type$total19*100-100,1)
beat_type$inc_21tolast12 <- round(beat_type$last12mos/beat_type$total21*100-100,1)
beat_type$inc_prior3yearavgtolast12 <- round((beat_type$last12mos/beat_type$avg_prior3years)*100-100,0)
# add population for beats
beat_type <- full_join(beats,beat_type,by="beat") 
# calculate the beat by beat rates PER 1K people
beat_type$rate19 <- round(beat_type$total19/beat_type$population*100000,1)
beat_type$rate20 <- round(beat_type$total20/beat_type$population*100000,1)
beat_type$rate21 <- round(beat_type$total21/beat_type$population*100000,1)
beat_type$rate_last12 <- round(beat_type$last12mos/beat_type$population*100000,1)
# calculate a multiyear rate
beat_type$rate_prior3years <- round(beat_type$avg_prior3years/beat_type$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
beat_type <- beat_type %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
beat_type <- beat_type %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# output various csvs for basic tables to be made with crime totals
# we are dropping geometry for beats here because this is just for tables
beat_detailed %>% st_drop_geometry() %>% write_csv("beat_detailed.csv")
beat_category %>% st_drop_geometry() %>% write_csv("beat_category.csv")
beat_type %>% st_drop_geometry() %>% write_csv("beat_type.csv")
citywide_detailed %>% write_csv("citywide_detailed.csv")
citywide_category %>% write_csv("citywide_category.csv")
citywide_type %>% write_csv("citywide_type.csv")




# Create individual spatial tables of crimes by major categories and types
murders_beat <- beat_detailed %>% filter(nibrs_class=="09A")
sexassaults_beat <- beat_category %>% filter(category_name=="Sexual Assault")
autothefts_beat <- beat_category %>% filter(category_name=="Auto Theft")
thefts_beat <- beat_category %>% filter(category_name=="Theft")
burglaries_beat <- beat_category %>% filter(category_name=="Burglary")
robberies_beat <- beat_category %>% filter(category_name=="Robbery")
assaults_beat <- beat_category %>% filter(category_name=="Assault")
drugs_beat <- beat_category %>% filter(category_name=="Drug Offenses")
violence_beat <- beat_type %>% filter(type=="Violent")
property_beat <- beat_type %>% filter(type=="Property")
# Create same set of tables for citywide figures
murders_city <- citywide_detailed %>% filter(nibrs_class=="09A")
sexassaults_city <- citywide_category %>% filter(category_name=="Sexual Assault")
autothefts_city <- citywide_category %>% filter(category_name=="Auto Theft")
thefts_city <- citywide_category %>% filter(category_name=="Theft")
burglaries_city <- citywide_category %>% filter(category_name=="Burglary")
robberies_city <- citywide_category %>% filter(category_name=="Robbery")
assaults_city <- citywide_category %>% filter(category_name=="Assault")
drugs_city <- citywide_category %>% filter(category_name=="Drug Offenses")
violence_city <- citywide_type %>% filter(type=="Violent")
property_city <- citywide_type %>% filter(type=="Property")

## MAY NEED FIXING FOR PROJECTED/22 ISSUES
# Using premise to identify the kinds of places where murders happen
where_murders_happen <- houston_crime %>%
  filter(nibrs_class=="09A") %>%
  group_by(year,premise) %>%
  summarise(count=n()) %>%
  pivot_wider(names_from=year, values_from=count)
# Using premise to identify the kinds of places where murders happen
where_murders_happen_last12 <- houston_crime_last12 %>%
  filter(nibrs_class=="09A") %>%
  group_by(premise) %>%
  summarise(last12=n())
# merge last 12 into the table
where_murders_happen <- full_join(where_murders_happen,where_murders_happen_last12,by="premise")
# add zeros where there were no crimes tallied that year
where_murders_happen[is.na(where_murders_happen)] <- 0
rm(where_murders_happen_last12)

# Using premise to identify the kinds of places where all violent crimes happen
where_violentcrimes_happen <- houston_crime %>%
  filter(type=="Violent") %>%
  group_by(premise,year) %>%
  summarise(count=n()) %>%
  pivot_wider(names_from=year, values_from=count) %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022") 

# Using premise to identify the kinds of places where all violent crimes happen
where_propertycrimes_happen <- houston_crime %>%
  filter(type=="Property") %>%
  group_by(premise,year) %>%
  summarise(count=n()) %>%
  pivot_wider(names_from=year, values_from=count) %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022") 

# Using hour to identify the hours of day when murders happen
when_murders_happen <- houston_crime %>%
  filter(nibrs_class=="09A") %>%
  group_by(hour) %>%
  summarise(count=n()) %>% 
  arrange(hour)
#when_murders_happen$time <- case_when(when_murders_happen$hour %in% c("0","1","2","3","4","21","22","23") ~ "Overnight",
#                                      when_murders_happen$hour %in% c("5","6","7","8","9","10","11","12") ~ "Morning",
#                                      when_murders_happen$hour %in% c("13","14","15","16","17","18","19","20")  ~ "Afternoon/Evening",
#                                      TRUE ~ "Other")
when_murders_happen$time <- case_when(when_murders_happen$hour == "0" ~ "12 a.m.",
                                      when_murders_happen$hour %in% c("1","2","3","4","5","6","7","8","9","10","11") ~ paste0(when_murders_happen$hour," a.m."),
                                      when_murders_happen$hour %in% c("12") ~ paste0(when_murders_happen$hour," p.m."),
                                      when_murders_happen$hour %in% c("13","14","15","16","17","18","19","20","21","22","23") ~ paste0((as.numeric(when_murders_happen$hour)-12)," p.m."),
                                      TRUE ~ "Other")

# Create individual spatial tables of crimes by major categories and types
murders_beat %>% st_drop_geometry() %>% write_csv("murders_beat.csv")
sexassaults_beat %>% st_drop_geometry() %>% write_csv("sexassaults_beat.csv")
autothefts_beat %>% st_drop_geometry() %>% write_csv("autothefts_beat.csv")
thefts_beat %>% st_drop_geometry() %>% write_csv("thefts_beat.csv")
burglaries_beat %>% st_drop_geometry() %>% write_csv("burglaries_beat.csv")
robberies_beat %>% st_drop_geometry() %>% write_csv("robberies_beat.csv")
assaults_beat %>% st_drop_geometry() %>% write_csv("assaults_beat.csv")
drugs_beat %>% st_drop_geometry() %>% write_csv("drugs_beat.csv")
violence_beat %>% st_drop_geometry() %>% write_csv("violence_beat.csv")
property_beat %>% st_drop_geometry() %>% write_csv("property_beat.csv")

# additional table exports for specific charts
where_murders_happen %>% write_csv("where_murders_happen.csv")
when_murders_happen %>% write_csv("when_murders_happen.csv")

# Calculating some murder frequency figures for the homicides page story element
beatpops_down <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(murders_beat) %>% 
  filter(rate_last12<=rate_prior3years) %>%
  select(1,2,23,24)
beatpops_down$diff <- beatpops_down$rate_last12-beatpops_down$rate_prior3years
pop_murdersdown <- sum(beatpops_down$population)
# population where murders are up 20%
beatpops_up <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(murders_beat) %>% 
  filter(rate_last12>=(1.2*rate_prior3years)) %>%
  select(1,2,23,24)
pop_murdersup <- sum(beatpops_up$population)
# population where there were zero murders
beatpops_zero <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(murders_beat) %>% 
  filter(last12mos<=0) %>%
  select(1,2)
pop_murderszero <- sum(beatpops_zero$population)
# population where there was at least one murder every month
beatpops_monthly <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(murders_beat) %>% 
  filter(last12mos>=12) %>%
  select(1,2)
pop_murdersmonthly <- sum(beatpops_monthly$population)
# cleanup
rm(beatpops_down,beatpops_up,beatpops_monthly,beatpops_zero)


# Calculating some sex assault frequency figures for the homicides page story element
beatpops_down <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(sexassaults_beat) %>% 
  filter(rate_last12<=rate_prior3years) %>%
  select(1,2,22,23)
beatpops_down$diff <- beatpops_down$rate_last12-beatpops_down$rate_prior3years
pop_sexassaultsdown <- sum(beatpops_down$population)
# population where sexassaults are up 20%
beatpops_up <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(sexassaults_beat) %>% 
  filter(rate_last12>=(1.2*rate_prior3years)) %>%
  select(1,2,22,23)
pop_sexassaultsup <- sum(beatpops_up$population)
# population where there were zero sexassaults
beatpops_zero <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(sexassaults_beat) %>% 
  filter(last12mos<=0) %>%
  select(1,2)
pop_sexassaultszero <- sum(beatpops_zero$population)
# population where there was at least one murder every month
beatpops_monthly <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(sexassaults_beat) %>% 
  filter(last12mos>=12) %>%
  select(1,2)
pop_sexassaultsmonthly <- sum(beatpops_monthly$population)
# cleanup
rm(beatpops_down,beatpops_up,beatpops_monthly,beatpops_zero)


# Calculating some AUTOTHEFT frequency figures for the homicides page story element
beatpops_down <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(autothefts_beat) %>% 
  filter(rate_last12<=rate_prior3years) %>%
  select(1,2,22,23)
beatpops_down$diff <- beatpops_down$rate_last12-beatpops_down$rate_prior3years
pop_autotheftsdown <- sum(beatpops_down$population)
# population where autothefts are up 20%
beatpops_up <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(autothefts_beat) %>% 
  filter(rate_last12>=(1.2*rate_prior3years)) %>%
  select(1,2,22,23)
pop_autotheftsup <- sum(beatpops_up$population)
# population where there were zero autothefts
beatpops_zero <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(autothefts_beat) %>% 
  filter(last12mos<=0) %>%
  select(1,2)
pop_autotheftszero <- sum(beatpops_zero$population)
# population where there was at least one murder every month
beatpops_monthly <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(autothefts_beat) %>% 
  filter(last12mos>=240) %>%
  select(1,2)
pop_autotheftsmonthly <- sum(beatpops_monthly$population)
# cleanup
rm(beatpops_down,beatpops_up,beatpops_monthly,beatpops_zero)

# Calculating some OTHER THEFT frequency figures for the homicides page story element
beatpops_down <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(thefts_beat) %>% 
  filter(rate_last12<=rate_prior3years) %>%
  select(1,2,22,23)
beatpops_down$diff <- beatpops_down$rate_last12-beatpops_down$rate_prior3years
pop_theftsdown <- sum(beatpops_down$population)
# population where thefts are up 20%
beatpops_up <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(thefts_beat) %>% 
  filter(rate_last12>=(1.2*rate_prior3years)) %>%
  select(1,2,22,23)
pop_theftsup <- sum(beatpops_up$population)
# population where there were zero thefts
beatpops_zero <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(thefts_beat) %>% 
  filter(last12mos<=0) %>%
  select(1,2)
pop_theftszero <- sum(beatpops_zero$population)
# population where there was at least one murder every month
beatpops_monthly <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(thefts_beat) %>% 
  filter(last12mos>=240) %>%
  select(1,2)
pop_theftsmonthly <- sum(beatpops_monthly$population)
# cleanup
rm(beatpops_down,beatpops_up,beatpops_monthly,beatpops_zero)



# Calculating some BURGLARIES frequency figures for the homicides page story element
beatpops_down <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(burglaries_beat) %>% 
  filter(rate_last12<=rate_prior3years) %>%
  select(1,2,22,23)
beatpops_down$diff <- beatpops_down$rate_last12-beatpops_down$rate_prior3years
pop_burglariesdown <- sum(beatpops_down$population)
# population where burglaries are up 20%
beatpops_up <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(burglaries_beat) %>% 
  filter(rate_last12>=(1.2*rate_prior3years)) %>%
  select(1,2,22,23)
pop_burglariesup <- sum(beatpops_up$population)
# population where there were zero burglaries
beatpops_zero <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(burglaries_beat) %>% 
  filter(last12mos<=0) %>%
  select(1,2)
pop_burglarieszero <- sum(beatpops_zero$population)
# population where there was at least one murder every month
beatpops_monthly <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(burglaries_beat) %>% 
  filter(last12mos>=240) %>%
  select(1,2)
pop_burglariesmonthly <- sum(beatpops_monthly$population)
# cleanup
rm(beatpops_down,beatpops_up,beatpops_monthly,beatpops_zero)

# Calculating some ROBBERIES frequency figures for the homicides page story element
beatpops_down <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(robberies_beat) %>% 
  filter(rate_last12<=rate_prior3years) %>%
  select(1,2,22,23)
beatpops_down$diff <- beatpops_down$rate_last12-beatpops_down$rate_prior3years
pop_robberiesdown <- sum(beatpops_down$population)
# population where robberies are up 20%
beatpops_up <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(robberies_beat) %>% 
  filter(rate_last12>=(1.2*rate_prior3years)) %>%
  select(1,2,22,23)
pop_robberiesup <- sum(beatpops_up$population)
# population where there were zero robberies
beatpops_zero <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(robberies_beat) %>% 
  filter(last12mos<=0) %>%
  select(1,2)
pop_robberieszero <- sum(beatpops_zero$population)
# population where there was at least one murder every month
beatpops_monthly <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(robberies_beat) %>% 
  filter(last12mos>=240) %>%
  select(1,2)
pop_robberiesmonthly <- sum(beatpops_monthly$population)
# cleanup
rm(beatpops_down,beatpops_up,beatpops_monthly,beatpops_zero)

# Calculating some ASSAULTS frequency figures for the homicides page story element
beatpops_down <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(assaults_beat) %>% 
  filter(rate_last12<=rate_prior3years) %>%
  select(1,2,22,23)
beatpops_down$diff <- beatpops_down$rate_last12-beatpops_down$rate_prior3years
pop_assaultsdown <- sum(beatpops_down$population)
# population where assaults are up 20%
beatpops_up <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(assaults_beat) %>% 
  filter(rate_last12>=(1.2*rate_prior3years)) %>%
  select(1,2,22,23)
pop_assaultsup <- sum(beatpops_up$population)
# population where there were zero assaults
beatpops_zero <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(assaults_beat) %>% 
  filter(last12mos<=0) %>%
  select(1,2)
pop_assaultszero <- sum(beatpops_zero$population)
# population where there was at least one murder every month
beatpops_monthly <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(assaults_beat) %>% 
  filter(last12mos>=240) %>%
  select(1,2)
pop_assaultsmonthly <- sum(beatpops_monthly$population)
# cleanup
rm(beatpops_down,beatpops_up,beatpops_monthly,beatpops_zero)

# Calculating some DRUGS frequency figures for the homicides page story element
beatpops_down <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(drugs_beat) %>% 
  filter(rate_last12<=rate_prior3years) %>%
  select(1,2,22,23)
beatpops_down$diff <- beatpops_down$rate_last12-beatpops_down$rate_prior3years
pop_drugsdown <- sum(beatpops_down$population)
# population where drugs are up 20%
beatpops_up <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(drugs_beat) %>% 
  filter(rate_last12>=(1.2*rate_prior3years)) %>%
  select(1,2,22,23)
pop_drugsup <- sum(beatpops_up$population)
# population where there were zero drugs
beatpops_zero <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(drugs_beat) %>% 
  filter(last12mos<=0) %>%
  select(1,2)
pop_drugszero <- sum(beatpops_zero$population)
# population where there was at least one murder every month
beatpops_monthly <- beats %>% st_drop_geometry() %>% select(1,5) %>% 
  left_join(drugs_beat) %>% 
  filter(last12mos>=240) %>%
  select(1,2)
pop_drugsmonthly <- sum(beatpops_monthly$population)
# cleanup
rm(beatpops_down,beatpops_up,beatpops_monthly,beatpops_zero)

