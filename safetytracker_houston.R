library(tidyverse)
library(tidycensus)
library(readxl)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(mapview)
library(sp)
library(sf)
library(htmlwidgets)
library(htmltools)
library(RCurl)
library(Hmisc)
library(rmarkdown)



# Save for backup the latest posted files for 2021, 2020 and 2019
# download.file("https://www.houstontx.gov/police/cs/xls/NIBRSPublicViewDec21.xlsx","data/latest/houston_NIBRS2021.xlsx")
# download.file("https://www.houstontx.gov/police/cs/xls/NIBRSPublicView.Jan1-Dec31-2020.xlsx","data/latest/houston_NIBRS2020.xlsx")
# download.file("https://www.houstontx.gov/police/cs/xls/2019_NIBRSPublicView.Jan1-Dec31.xlsx","data/latest/houston_NIBRS2019.xlsx")
# download.file("https://www.houstontx.gov/police/cs/xls/NIBRSPublicViewJan-Feb22.xlsx","data/latest/houston_NIBRS2022.xlsx")


# BRING IN THE HPD RAW FILES FOR 2019, 2020, 2021 and 2022 (to date)
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

# Combine the three years' of into single table
houston_crime <- bind_rows(houston19,houston20,houston21,houston22)
rm(houston19,houston20,houston21)
houston_crime$year <- substr(houston_crime$date,1,4)
houston_crime$date <- as.Date(houston_crime$date, "%Y-%m-%d")

# Build a class-code table
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
houston_crime$beat[is.na(houston_crime$beat)] <- "UNKNOWN"
# Fix non-existent beat 15E11, which should be 15E10
houston_crime$beat <- ifelse(houston_crime$beat == "15E11","15E10",houston_crime$beat)
# Fix beats in District 5, 6 and 7 that became District 22
houston_crime$beat <- ifelse(houston_crime$beat == "5F40","22B10",houston_crime$beat)
houston_crime$beat <- ifelse(houston_crime$beat == "6B50","22B30",houston_crime$beat)
houston_crime$beat <- ifelse(houston_crime$beat == "6B60","22B20",houston_crime$beat)
houston_crime$beat <- ifelse(houston_crime$beat == "7C50","22B40",houston_crime$beat)
# Calculate the multiplier for the 2022 data based on days of data in file
days_so_far_2022 <- 1/(n_distinct(houston22$date)/365)
# write csv of houston crime
write_csv(houston_crime,"data/latest/houston_crime.csv")


# Calculate the total of each DETAILED crime/incident CITYWIDE
totals_by_crime_detailed <- houston_crime %>%
  group_by(offense_type,nibrs_class,year) %>%
  summarise(count = sum(offense_count)) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
totals_by_crime_detailed <- totals_by_crime_detailed %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add projected22 column to project/forecast annual number
totals_by_crime_detailed$projected22 <- round(totals_by_crime_detailed$total22*days_so_far_2022,0)
# add zeros where there were no crimes tallied that year
totals_by_crime_detailed[is.na(totals_by_crime_detailed)] <- 0
# calculate increases
totals_by_crime_detailed$inc_19to21 <- round(totals_by_crime_detailed$total21/totals_by_crime_detailed$total19*100-100,1)
totals_by_crime_detailed$inc_19to22sofar <- round(totals_by_crime_detailed$projected22/totals_by_crime_detailed$total19*100-100,1)
totals_by_crime_detailed$inc_21to22sofar <- round(totals_by_crime_detailed$projected22/totals_by_crime_detailed$total21*100-100,1)
totals_by_crime_detailed$inc_3yearto22sofar <- round(totals_by_crime_detailed$projected22/((totals_by_crime_detailed$total19+totals_by_crime_detailed$total20+totals_by_crime_detailed$total21)/3)*100-100,0)
# calculate the citywide rates
totals_by_crime_detailed$rate19 <- round(totals_by_crime_detailed$total19/2304580*100000,1)
totals_by_crime_detailed$rate20 <- round(totals_by_crime_detailed$total20/2304580*100000,1)
totals_by_crime_detailed$rate21 <- round(totals_by_crime_detailed$total21/2304580*100000,1)
totals_by_crime_detailed$rate22 <- round(totals_by_crime_detailed$projected22/2304580*100000,1)
# calculate a multiyear rate
totals_by_crime_detailed$rate_multiyear <- round(((totals_by_crime_detailed$total19+totals_by_crime_detailed$total20+totals_by_crime_detailed$total21)/3)/2304580*100000,1)

# Calculate the total of each CATEGORY of crime/incident CITYWIDE
totals_by_crime_category <- houston_crime %>%
  group_by(category_name,year) %>%
  summarise(count = sum(offense_count)) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
totals_by_crime_category <- totals_by_crime_category %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add projected22 column to project/forecast annual number
totals_by_crime_category$projected22 <- round(totals_by_crime_category$total22*days_so_far_2022,0)
# add zeros where there were no crimes tallied that year
totals_by_crime_category[is.na(totals_by_crime_category)] <- 0
# calculate increases
totals_by_crime_category$inc_19to21 <- round(totals_by_crime_category$total21/totals_by_crime_category$total19*100-100,1)
totals_by_crime_category$inc_19to22sofar <- round(totals_by_crime_category$projected22/totals_by_crime_category$total19*100-100,1)
totals_by_crime_category$inc_21to22sofar <- round(totals_by_crime_category$projected22/totals_by_crime_category$total21*100-100,1)
# calculate the citywide rates
totals_by_crime_category$rate19 <- round(totals_by_crime_category$total19/2304580*100000,1)
totals_by_crime_category$rate20 <- round(totals_by_crime_category$total20/2304580*100000,1)
totals_by_crime_category$rate21 <- round(totals_by_crime_category$total21/2304580*100000,1)
totals_by_crime_category$rate22 <- round(totals_by_crime_category$projected22/2304580*100000,1)

# Calculate the total of each TYPE of crime/incident CITYWIDE
totals_by_crime_type <- houston_crime %>%
  group_by(type,year) %>%
  summarise(count = sum(offense_count)) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
totals_by_crime_type <- totals_by_crime_type %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add projected22 column to project/forecast annual number
totals_by_crime_type$projected22 <- round(totals_by_crime_type$total22*days_so_far_2022,0)
# add zeros where there were no crimes tallied that year
totals_by_crime_type[is.na(totals_by_crime_type)] <- 0
# calculate increases
totals_by_crime_type$inc_19to21 <- round(totals_by_crime_type$total21/totals_by_crime_type$total19*100-100,1)
totals_by_crime_type$inc_19to22sofar <- round(totals_by_crime_type$projected22/totals_by_crime_type$total19*100-100,1)
totals_by_crime_type$inc_21to22sofar <- round(totals_by_crime_type$projected22/totals_by_crime_type$total21*100-100,1)
# calculate the citywide rates
totals_by_crime_type$rate19 <- round(totals_by_crime_type$total19/2304580*100000,1)
totals_by_crime_type$rate20 <- round(totals_by_crime_type$total20/2304580*100000,1)
totals_by_crime_type$rate21 <- round(totals_by_crime_type$total21/2304580*100000,1)
totals_by_crime_type$rate22 <- round(totals_by_crime_type$projected22/2304580*100000,1)

# ADD BEATS/GEOGRAPHY
# Geography and populations processed separately in 
# process_houston_police_beats.R
# WORK TO BE DONE includes tagging beats with 
# audience-friendly known geographical reference points
beatsindata <- houston_crime %>%
  group_by(beat,year) %>%
  summarise(count=n()) %>%
  pivot_wider(names_from=year, values_from=count)
anti_join(beatsindata,beats,by="beat")


# Calculate the total of each DETAILED crime/incident CITYWIDE
totals_by_beat_detailed <- houston_crime %>%
  group_by(beat,offense_type,nibrs_class,year) %>%
  summarise(count = sum(offense_count)) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
totals_by_beat_detailed <- totals_by_beat_detailed %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add projected22 column to project/forecast annual number
totals_by_beat_detailed$projected22 <- round(totals_by_beat_detailed$total22*days_so_far_2022,0)
# add zeros where there were no crimes tallied that year
totals_by_beat_detailed[is.na(totals_by_beat_detailed)] <- 0
# calculate increases
totals_by_beat_detailed$inc_19to21 <- round(totals_by_beat_detailed$total21/totals_by_beat_detailed$total19*100-100,1)
totals_by_beat_detailed$inc_19to22sofar <- round(totals_by_beat_detailed$projected22/totals_by_beat_detailed$total19*100-100,1)
totals_by_beat_detailed$inc_21to22sofar <- round(totals_by_beat_detailed$projected22/totals_by_beat_detailed$total21*100-100,1)
totals_by_beat_detailed$inc_3yearto22sofar <- round(totals_by_beat_detailed$projected22/((totals_by_beat_detailed$total19+totals_by_beat_detailed$total20+totals_by_beat_detailed$total21)/3)*100-100,0)
# add population for beats
totals_by_beat_detailed <- full_join(beats,totals_by_beat_detailed,by="beat") 
# calculate the beat by beat rates PER 1K people
totals_by_beat_detailed$rate19 <- round(totals_by_beat_detailed$total19/totals_by_beat_detailed$population*100000,1)
totals_by_beat_detailed$rate20 <- round(totals_by_beat_detailed$total20/totals_by_beat_detailed$population*100000,1)
totals_by_beat_detailed$rate21 <- round(totals_by_beat_detailed$total21/totals_by_beat_detailed$population*100000,1)
totals_by_beat_detailed$rate22 <- round(totals_by_beat_detailed$projected22/totals_by_beat_detailed$population*100000,1)
# calculate a multiyear rate
totals_by_beat_detailed$rate_multiyear <- round(((totals_by_beat_detailed$total19+totals_by_beat_detailed$total20+totals_by_beat_detailed$total21)/3)/totals_by_beat_detailed$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
totals_by_beat_detailed <- totals_by_beat_detailed %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
totals_by_beat_detailed <- totals_by_beat_detailed %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# Calculate the total of each CATEGORY of crime/incident CITYWIDE
totals_by_beat_category <- houston_crime %>%
  group_by(beat,category_name,year) %>%
  summarise(count = sum(offense_count)) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
totals_by_beat_category <- totals_by_beat_category %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add projected22 column to project/forecast annual number
totals_by_beat_category$projected22 <- round(totals_by_beat_category$total22*days_so_far_2022,0)
# add zeros where there were no crimes tallied that year
totals_by_beat_category[is.na(totals_by_beat_category)] <- 0
# calculate increases
totals_by_beat_category$inc_19to21 <- round(totals_by_beat_category$total21/totals_by_beat_category$total19*100-100,1)
totals_by_beat_category$inc_19to22sofar <- round(totals_by_beat_category$projected22/totals_by_beat_category$total19*100-100,1)
totals_by_beat_category$inc_21to22sofar <- round(totals_by_beat_category$projected22/totals_by_beat_category$total21*100-100,1)
totals_by_beat_category$inc_3yearto22sofar <- round(totals_by_beat_category$projected22/((totals_by_beat_category$total19+totals_by_beat_category$total20+totals_by_beat_category$total21)/3)*100-100,0)
# add population for beats
totals_by_beat_category <- full_join(beats,totals_by_beat_category,by="beat") 
# calculate the beat by beat rates PER 1K people
totals_by_beat_category$rate19 <- round(totals_by_beat_category$total19/totals_by_beat_category$population*100000,1)
totals_by_beat_category$rate20 <- round(totals_by_beat_category$total20/totals_by_beat_category$population*100000,1)
totals_by_beat_category$rate21 <- round(totals_by_beat_category$total21/totals_by_beat_category$population*100000,1)
totals_by_beat_category$rate22 <- round(totals_by_beat_category$projected22/totals_by_beat_category$population*100000,1)
# calculate a multiyear rate
totals_by_beat_category$rate_multiyear <- round(((totals_by_beat_category$total19+totals_by_beat_category$total20+totals_by_beat_category$total21)/3)/totals_by_beat_category$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
totals_by_beat_category <- totals_by_beat_category %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
totals_by_beat_category <- totals_by_beat_category %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# Calculate the total of each TYPE of crime/incident CITYWIDE
totals_by_beat_type <- houston_crime %>%
  group_by(beat,type,year) %>%
  summarise(count = sum(offense_count)) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
totals_by_beat_type <- totals_by_beat_type %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add projected22 column to project/forecast annual number
totals_by_beat_type$projected22 <- round(totals_by_beat_type$total22*days_so_far_2022,0)
# add zeros where there were no crimes tallied that year
totals_by_beat_type[is.na(totals_by_beat_type)] <- 0
# calculate increases
totals_by_beat_type$inc_19to21 <- round(totals_by_beat_type$total21/totals_by_beat_type$total19*100-100,1)
totals_by_beat_type$inc_19to22sofar <- round(totals_by_beat_type$projected22/totals_by_beat_type$total19*100-100,1)
totals_by_beat_type$inc_21to22sofar <- round(totals_by_beat_type$projected22/totals_by_beat_type$total21*100-100,1)
totals_by_beat_type$inc_3yearto22sofar <- round(totals_by_beat_type$projected22/((totals_by_beat_type$total19+totals_by_beat_type$total20+totals_by_beat_type$total21)/3)*100-100,0)
# add population for beats
totals_by_beat_type <- full_join(beats,totals_by_beat_type,by="beat") 
# calculate the beat by beat rates PER 1K people
totals_by_beat_type$rate19 <- round(totals_by_beat_type$total19/totals_by_beat_type$population*100000,1)
totals_by_beat_type$rate20 <- round(totals_by_beat_type$total20/totals_by_beat_type$population*100000,1)
totals_by_beat_type$rate21 <- round(totals_by_beat_type$total21/totals_by_beat_type$population*100000,1)
totals_by_beat_type$rate22 <- round(totals_by_beat_type$projected22/totals_by_beat_type$population*100000,1)
# calculate a multiyear rate
totals_by_beat_type$rate_multiyear <- round(((totals_by_beat_type$total19+totals_by_beat_type$total20+totals_by_beat_type$total21)/3)/totals_by_beat_type$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
totals_by_beat_type <- totals_by_beat_type %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
totals_by_beat_type <- totals_by_beat_type %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# output various basic tables with crime totals
totals_by_beat_detailed %>% st_drop_geometry() %>% write_csv("totals_by_beat_detailed.csv")
totals_by_beat_category %>% st_drop_geometry() %>% write_csv("totals_by_beat_category.csv")
totals_by_beat_type %>% st_drop_geometry() %>% write_csv("totals_by_beat_type.csv")
totals_by_crime_detailed %>% write_csv("totals_by_crime_detailed.csv")
totals_by_crime_category %>% write_csv("totals_by_crime_category.csv")
totals_by_crime_type %>% write_csv("totals_by_crime_type.csv")


# Isolate three categories of crimes by beat by year
murders_by_beat <- totals_by_beat_detailed %>% filter(nibrs_class=="09A")
sexualassaults_by_beat <- totals_by_beat_category %>% filter(category_name=="Sexual Assault")
autothefts_by_beat <- totals_by_beat_detailed %>% filter(nibrs_class=="240")

# MURDER MAP
# Set bins for numbers of crimes for murders map
murderbins <- c(0,median(murders_by_beat$rate_multiyear,na.rm = TRUE)/2,median(murders_by_beat$rate_multiyear,na.rm = TRUE),median(murders_by_beat$rate_multiyear,na.rm = TRUE)*2,median(murders_by_beat$rate_multiyear,na.rm = TRUE)*3,200)
murderpal <- colorBin(c("#99a0a5","#667f99","#00318b","#0058f6","#ffba00"), murders_by_beat$rate_multiyear, bins = murderbins)
# Create quick labels for murders map
murderlabel <- paste(sep="","<style>
table {
  font-family: roboto;
  border-collapse: collapse;
  width: 100%;
}

tr {
  border-bottom: thin solid #f2f2f2;
}

h5 {
  font-size: 24px;
  margin-top: 0;
  margin-bottom: 0;
  color: #0058f6;
}
  
td, th {
  text-align: right;
  padding: 6px;
}

h6 {
  text-align: left;
  font-size: 15px;
    margin-top: 0;
  margin-bottom: 0;
}

h4 {
  text-align: left;
  font-size: 10px;
  margin-top: 0;
  margin-bottom: 2;
}

</style>
<table>
<caption><h6>Houston P.D. Beat #",murders_by_beat$beat,"</h6><h4>Est. Pop. ",murders_by_beat$population,"</h4>
      <tr>
				<th>Year</th>
				<th>Total</th>
				<th>Rate</th>
				<th></th>
			</tr>
			<tr>
				<td>2019</td>
				<td>",
murders_by_beat$total19,
"</td>
				<td>",
murders_by_beat$rate19,
"</td>
				<!-- Insert cell to cross/span 
				multi rows -->
				<td rowspan='4'>2022<br>projection is<br><h5>",
murders_by_beat$inc_3yearto22sofar,
				"%</h5>compared<br>to 3-year<br>average</td>
			</tr>
			<tr>
				<td>2020</td>
				<td>",
murders_by_beat$total20,
"</td>
				<td>",
murders_by_beat$rate20,
"</td>
			</tr>
						<tr>
				<td>2021</td>
				<td>",
murders_by_beat$total21,
"</td>
				<td>",
murders_by_beat$rate21,
"</td>
			</tr>
			</tr>
						<tr>
				<td>2022</td>
				<td>",
murders_by_beat$total22,
"</td>
				<td>",
murders_by_beat$rate22,
"</td>
			</tr>
</table>")

# Create rapid prototype of murders map
# Issue to fix here is that we merged the data points to the df first, instead of the spatial file
# Go back and rework that part
houston_murder_map <- leaflet(murders_by_beat, options = leafletOptions(zoomControl = FALSE)) %>%
  htmlwidgets::onRender("function(el, x) {
L.control.zoom({ position: 'topright' }).addTo(this)
}") %>%
  setView(-95.45, 29.75, zoom = 11) %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(color = "white", popup = murderlabel, weight = 0.5, smoothFactor = 0.5,
              opacity = 0.6, fillOpacity = 0.5,
              fillColor = ~murderpal(rate_multiyear)) %>% 
  addLegend(pal = murderpal, 
            opacity = 0.6,
            values = murders_by_beat$rate_multiyear, 
            position = "bottomright", 
            title = "Homicides Per 100K people<br>") %>%
  addLegend(position = "topleft", 
            labels = NULL,
            values = NULL,
            colors = NULL,
            title = "<big>World's Best Map of Houston Homicides</big><br>
            <small>Please insert some chatter & other html/tools/stuff here<br>
See also: <a href='https://abcotvdata.github.io/safetytracker_houston/sexualassault_rate.html'>Sexual Assault</a> |
 <a href='https://abcotvdata.github.io/safetytracker_houston/autothefts_rate.html'>Auto Thefts</a> |
              <a href='https://abcotvdata.github.io/safetytracker_houston/autothefts_rate.html'>Burglaries</a>  |
              <a href='https://abcotvdata.github.io/safetytracker_houston/autothefts_rate.html'>Theft</a></small>")
houston_murder_map

names(providers)

# SEXUAL ASSAULTS MAP
# Set bins for numbers of crimes for murders map
sexualassaultbins <- c(0,median(sexualassaults_by_beat$rate_multiyear,na.rm = TRUE)/2,median(sexualassaults_by_beat$rate_multiyear,na.rm = TRUE),median(sexualassaults_by_beat$rate_multiyear,na.rm = TRUE)*2,median(sexualassaults_by_beat$rate_multiyear,na.rm = TRUE)*3,300)
sexualassaultpal <- colorBin(c("#99a0a5","#667f99","#00318b","#0058f6","#ffba00"), sexualassaults_by_beat$rate_multiyear, bins = sexualassaultbins)
# Create quick labels for sexualassaults map
sexualassaultlabel <- paste(sep="","<style>
table {
  font-family: roboto;
  border-collapse: collapse;
  width: 100%;
}

tr {
  border-bottom: thin solid #f2f2f2;
}

h5 {
  font-size: 24px;
  margin-top: 0;
  margin-bottom: 0;
  color: #0058f6;
}
  
td, th {
  text-align: right;
  padding: 6px;
}

h6 {
  text-align: left;
  font-size: 15px;
    margin-top: 0;
  margin-bottom: 0;
}

h4 {
  text-align: left;
  font-size: 10px;
  margin-top: 0;
  margin-bottom: 2;
}

</style>
<table>
<caption><h6>Houston P.D. Beat #",sexualassaults_by_beat$beat,"</h6><h4>Est. Pop. ",sexualassaults_by_beat$population,"</h4>
      <tr>
				<th>Year</th>
				<th>Total</th>
				<th>Rate</th>
				<th></th>
			</tr>
			<tr>
				<td>2019</td>
				<td>",
sexualassaults_by_beat$total19,
"</td>
				<td>",
sexualassaults_by_beat$rate19,
"</td>
				<!-- Insert cell to cross/span 
				multi rows -->
				<td rowspan='4'>2022<br>projection is<br><h5>",
sexualassaults_by_beat$inc_3yearto22sofar,
				"%</h5>compared<br>to 3-year<br>average</td>
			</tr>
			<tr>
				<td>2020</td>
				<td>",
sexualassaults_by_beat$total20,
"</td>
				<td>",
sexualassaults_by_beat$rate20,
"</td>
			</tr>
						<tr>
				<td>2021</td>
				<td>",
sexualassaults_by_beat$total21,
"</td>
				<td>",
sexualassaults_by_beat$rate21,
"</td>
			</tr>
			</tr>
						<tr>
				<td>2022</td>
				<td>",
sexualassaults_by_beat$total22,
"</td>
				<td>",
sexualassaults_by_beat$rate22,
"</td>
			</tr>
</table>")

# Now create the SEXUAL ASSAULTS MAP
houston_sexualassault_map <- leaflet(sexualassaults_by_beat) %>%
  setView(-95.45, 29.75, zoom = 10) %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(color = "white", popup = sexualassaultlabel, weight = 0.5, smoothFactor = 0.5,
              opacity = 0.6, fillOpacity = 0.5,
              fillColor = ~sexualassaultpal(rate_multiyear)) %>% 
  addLegend(pal = sexualassaultpal, 
            values = sexualassaults_by_beat$rate_multiyear, 
            position = "bottomleft", 
            title = "Sexual Assaults Per 100K people<br>
            Over Last Three Years (2019-2021)<br>
See also:<br><a href='https://abcotvdata.github.io/safetytracker_houston/murder_rate.html'>Homicides</a><br>
<a href='https://abcotvdata.github.io/safetytracker_houston/autothefts_rate.html'>Auto Thefts</a>")
houston_sexualassault_map


# AUTO THEFTS MAP
# Set bins for numbers of crimes for murders map
autotheftbins <- c(0,median(autothefts_by_beat$rate_multiyear,na.rm = TRUE)/2,median(autothefts_by_beat$rate_multiyear,na.rm = TRUE),median(autothefts_by_beat$rate_multiyear,na.rm = TRUE)*2,median(autothefts_by_beat$rate_multiyear,na.rm = TRUE)*3,3000)
autotheftpal <- colorBin(c("#99a0a5","#667f99","#00318b","#0058f6","#ffba00"), autothefts_by_beat$rate_multiyear, bins = autotheftbins)
# Create quick labels for autothefts map
autotheftlabel <- paste(sep="","<style>
table {
  font-family: roboto;
  border-collapse: collapse;
  width: 100%;
}

tr {
  border-bottom: thin solid #f2f2f2;
}

h5 {
  font-size: 24px;
  margin-top: 0;
  margin-bottom: 0;
  color: #0058f6;
}
  
td, th {
  text-align: right;
  padding: 6px;
}

h6 {
  text-align: left;
  font-size: 15px;
    margin-top: 0;
  margin-bottom: 0;
}

h4 {
  text-align: left;
  font-size: 10px;
  margin-top: 0;
  margin-bottom: 2;
}

</style>
<table>
<caption><h6>Houston P.D. Beat #",autothefts_by_beat$beat,"</h6><h4>Est. Pop. ",autothefts_by_beat$population,"</h4>
      <tr>
				<th>Year</th>
				<th>Total</th>
				<th>Rate</th>
				<th></th>
			</tr>
			<tr>
				<td>2019</td>
				<td>",
autothefts_by_beat$total19,
"</td>
				<td>",
autothefts_by_beat$rate19,
"</td>
				<!-- Insert cell to cross/span 
				multi rows -->
				<td rowspan='4'>2022<br>projection is<br><h5>",
autothefts_by_beat$inc_3yearto22sofar,
"%</h5>compared<br>to 3-year<br>average</td>
			</tr>
			<tr>
				<td>2020</td>
				<td>",
autothefts_by_beat$total20,
"</td>
				<td>",
autothefts_by_beat$rate20,
"</td>
			</tr>
						<tr>
				<td>2021</td>
				<td>",
autothefts_by_beat$total21,
"</td>
				<td>",
autothefts_by_beat$rate21,
"</td>
			</tr>
			</tr>
						<tr>
				<td>2022</td>
				<td>",
autothefts_by_beat$total22,
"</td>
				<td>",
autothefts_by_beat$rate22,
"</td>
			</tr>
</table>")

# Now create the SEXUAL ASSAULTS MAP
houston_autotheft_map <- leaflet(autothefts_by_beat) %>%
  setView(-95.45, 29.75, zoom = 10) %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(color = "white", popup = autotheftlabel, weight = 0.5, smoothFactor = 0.5,
              opacity = 0.6, fillOpacity = 0.5,
              fillColor = ~autotheftpal(rate_multiyear)) %>% 
  addLegend(pal = autotheftpal, 
            values = autothefts_by_beat$rate_multiyear, 
            position = "bottomleft", 
            title = "Auto Thefts Per 100K people<br>
            Over Last Three Years (2019-2021)<br>
See also:<br><a href='https://abcotvdata.github.io/safetytracker_houston/murder_rate.html'>Homicides</a><br>
<a href='https://abcotvdata.github.io/safetytracker_houston/sexualassault_rate.html'>Sexual Assaults</a>")

houston_autotheft_map

#setwd("/Volumes/Jarvis/R/safetytracker_houston")
#setwd("/Volumes/Jarvis/R/safetytracker_houston/docs")

# saveWidget(houston_autotheft_map, 'autothefts_rate.html', title = "ABC13 Neighborhood Safety Tracker", selfcontained = TRUE, libdir=NULL)
#saveWidget(houston_murder_map, 'docs/murder_rate.html', title = "ABC13 Neighborhood Safety Tracker", selfcontained = TRUE)
#saveWidget(houston_sexualassault_map, 'docs/sexualassaults_rate.html', title = "ABC13 Neighborhood Safety Tracker", selfcontained = TRUE)
