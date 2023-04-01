library(tidyverse)
library(janitor)
library(readxl)

# priority: Harris, Montgomery, Liberty, Waller, Fort Bend, Brazoria, Galveston and Chambers
# whole dma includes: Austin, Brazoria, Calhoun, Chambers, Colorado, Fort Bend, Galveston, Grimes, Harris, Jackson, Liberty, Matagorda, Montgomery, Polk, San Jacinto, Trinity, Walker, Waller, Washington, Wharton

# Collect statewide data for previous years
download.file("https://txucr.nibrs.com/Report/DownloadSummaryIndexCrimesReportExcel?year=2018&reportType=2&startdate=2018/1/01&enddate=2018/12/31",
              "data/source/state/tx_crime_2018.xlsx")
download.file("https://txucr.nibrs.com/Report/DownloadSummaryIndexCrimesReportExcel?year=2019&reportType=2&startdate=2019/1/01&enddate=2019/12/31",
              "data/source/state/tx_crime_2019.xlsx")
download.file("https://txucr.nibrs.com/Report/DownloadSummaryIndexCrimesReportExcel?year=2020&reportType=2&startdate=2020/1/01&enddate=2020/12/31",
              "data/source/state/tx_crime_2020.xlsx")
download.file("https://txucr.nibrs.com/Report/DownloadSummaryIndexCrimesReportExcel?year=2021&reportType=2&startdate=2021/1/01&enddate=2021/12/31",
              "data/source/state/tx_crime_2021.xlsx")
download.file("https://txucr.nibrs.com/Report/DownloadSummaryIndexCrimesReportExcel?year=2022&reportType=2&startdate=2022/1/01&enddate=2022/12/31",
              "data/source/state/tx_crime_2022.xlsx")
# Collect statewide data for this year SO FAR
download.file("https://txucr.nibrs.com/Report/DownloadSummaryIndexCrimesReportExcel?year=2022&reportType=2&startdate=2023/1/01&enddate=2023/12/31",
              "data/source/state/tx_crime_2023.xlsx")

# import and merge into single file
tx_crime_2018 <- read_excel("data/source/state/tx_crime_2018.xlsx", 
                            sheet = "IndexCrimes", skip = 3) %>% clean_names() %>% remove_empty("rows") %>% mutate(year="2018")
tx_crime_2019 <- read_excel("data/source/state/tx_crime_2019.xlsx", 
                            sheet = "IndexCrimes", skip = 3) %>% clean_names() %>% remove_empty("rows") %>% mutate(year="2019")
tx_crime_2020 <- read_excel("data/source/state/tx_crime_2020.xlsx", 
                            sheet = "IndexCrimes", skip = 3) %>% clean_names() %>% remove_empty("rows") %>% mutate(year="2020")
tx_crime_2021 <- read_excel("data/source/state/tx_crime_2021.xlsx", 
                            sheet = "IndexCrimes", skip = 3) %>% clean_names() %>% remove_empty("rows") %>% mutate(year="2021")
tx_crime_2022 <- read_excel("data/source/state/tx_crime_2022.xlsx", 
                            sheet = "IndexCrimes", skip = 3) %>% clean_names() %>% remove_empty("rows") %>% mutate(year="2022")
tx_crime_2023 <- read_excel("data/source/state/tx_crime_2023.xlsx", 
                            sheet = "IndexCrimes", skip = 3) %>% clean_names() %>% remove_empty("rows") %>% mutate(year="2023")

# combine into a single file
tx_crime <- bind_rows(tx_crime_2018,tx_crime_2019,tx_crime_2020,tx_crime_2021,tx_crime_2022)
tx_crime$county <- ifelse(tx_crime$county_total=="County Total",tx_crime$agency_name,NA) 
tx_crime$county <- sub(" County","",tx_crime$county)

# Fill empty cells upward
tx_crime <- tx_crime %>% tidyr::fill(county, .direction = "up")

# Separate by counties & agencies
tx_crime_counties <- tx_crime %>% filter(county_total == "County Total")
tx_crime_agency <- tx_crime %>% filter(is.na(county_total))

# save reference files for temporary use and commit
write_csv(tx_crime_counties,"data/output/state/tx_crime_counties.csv")
write_csv(tx_crime_agency,"data/output/state/tx_crime_agency.csv")


