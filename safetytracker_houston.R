library(tidyverse)
library(tidycensus)
library(readxl)

download.file("https://www.houstontx.gov/police/cs/xls/NIBRSPublicViewDec21.xlsx","houston_NIBRS2021.xlxs")
download.file("https://www.houstontx.gov/police/cs/xls/NIBRSPublicView.Jan1-Dec31-2020.xlsx","houston_NIBRS2020.xlxs")
download.file("https://www.houstontx.gov/police/cs/xls/2019_NIBRSPublicView.Jan1-Dec31.xlsx","houston_NIBRS2019.xlxs")

houston21 <- read_excel("houston_NIBRS2021.xlxs", 
                                col_types = c("text", "date", "numeric", 
                                              "text", "text", "numeric", "text", 
                                              "text", "text", "text", "text", "text", 
                                              "text", "text")) %>% 
  janitor::clean_names()

houston20 <- read_excel("houston_NIBRS2020.xlxs", 
                        col_types = c("text", "date", "numeric", 
                                      "text", "text", "numeric", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text")) %>% 
  janitor::clean_names()

houston19 <- read_excel("houston_NIBRS2019.xlxs", 
                        col_types = c("text", "date", "numeric", 
                                      "text", "text", "numeric", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text")) %>% 
  janitor::clean_names()

totals_by_beat21 <- houston21 %>%
  group_by(beat,nibrs_description,nibrs_class) %>%
  summarise(count = sum(offense_count))

totals_by_crime21 <- houston21 %>%
  group_by(nibrs_description,nibrs_class) %>%
  summarise(count = sum(offense_count))

totals_by_beat20 <- houston20 %>%
  group_by(beat,nibrs_description,nibrs_class) %>%
  summarise(count = sum(offense_count))

totals_by_crime20 <- houston20 %>%
  group_by(nibrs_description,nibrs_class) %>%
  summarise(count = sum(offense_count))

totals_by_beat19 <- houston19 %>%
  group_by(beat,nibrs_description,nibrs_class) %>%
  summarise(count = sum(offense_count))

totals_by_crime19 <- houston19 %>%
  group_by(nibrs_description,nibrs_class) %>%
  summarise(count = sum(offense_count))


