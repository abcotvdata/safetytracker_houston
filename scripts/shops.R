library(tidyverse)
library(sf)
library(readxl)
library(zoo)

# Extract the last 12 months into a separate file
houston_crime_last12 <- houston_crime %>% filter(date>(max(houston_crime$date)-31536000))

# Using premise to identify the kinds of places where murders happen
where_crimes_happen <- houston_crime %>%
  group_by(year,premise) %>%
  summarise(count=n()) %>%
  pivot_wider(names_from=year, values_from=count)
# Using premise to identify the kinds of places where murders happen
where_crimes_happen_last12 <- houston_crime_last12 %>%
  group_by(premise) %>%
  summarise(last12=n())
# merge last 12 into the table
where_crimes_happen <- full_join(where_crimes_happen,where_crimes_happen_last12,by="premise")
# add zeros where there were no crimes tallied that year
where_crimes_happen[is.na(where_crimes_happen)] <- 0
rm(where_crimes_happen_last12)




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
when_murders_happen$time <- case_when(when_murders_happen$hour == "0" ~ "12 a.m.",
                                      when_murders_happen$hour %in% c("1","2","3","4","5","6","7","8","9","10","11") ~ paste0(when_murders_happen$hour," a.m."),
                                      when_murders_happen$hour %in% c("12") ~ paste0(when_murders_happen$hour," p.m."),
                                      when_murders_happen$hour %in% c("13","14","15","16","17","18","19","20","21","22","23") ~ paste0((as.numeric(when_murders_happen$hour)-12)," p.m."),
                                      TRUE ~ "Other")
when_murders_happen$timeframe <- case_when(when_murders_happen$hour %in% c("0","1","2","3","4","21","22","23") ~ "Overnight from 9 p.m. to 5 a.m.",
                                           when_murders_happen$hour %in% c("5","6","7","8","9","10","11") ~ "Morning from 5 a.m. to 12 p.m.",
                                           when_murders_happen$hour %in% c("12","13","14","15","16","17","18","19","20")  ~ "Afternoon/Evening from 12 p.m. to 9 p.m.",
                                           TRUE ~ "Other")
when_murders_happen <- when_murders_happen %>%
  group_by(timeframe) %>%
  summarise(total=sum(count))