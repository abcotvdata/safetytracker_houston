library(tidyverse)
library(readxl)

### IMPORT AND FORMAT ANNUAL DATA FROM HOUSTON PD EXCEL FILES IN ARCHIVE

# Define column names and column types variables once for all 3 files
column_names <- c("incident", "date", "hour", 
                  "nibrs_class", "offense_type", "offense_count", "beat", 
                  "premise", "street_no", "street_name", "street_type", "street_suffix", 
                  "city", "zip")
column_types <- c("text", "date", "numeric", 
                  "text", "text", "numeric", "text", 
                  "text", "text", "text", "text", "text", 
                  "text", "text")

# Read in annual files for 2019, 2020, 2021, 2022 and 2023

houston23 <- read_excel("data/source/annual/houston_NIBRS2023.xlsx", 
                        col_types = c("text", "date", "numeric", 
                                      "text" , "text", "numeric", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text","numeric","numeric"))
names(houston23) <- c("incident", "date", "hour", 
                      "nibrs_class", "offense_type", "offense_count", "beat", 
                      "premise", "street_no", "street_name", "street_type", "street_suffix", 
                      "city", "zip","longitude","latitude")

# 2022
houston22 <- read_excel("data/source/annual/houston_NIBRS2022.xlsx", 
                        col_types = c("text", "date", "numeric", 
                                      "text" , "text", "numeric", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text","numeric","numeric"))
names(houston22) <- c("incident", "date", "hour", 
                      "nibrs_class", "offense_type", "offense_count", "beat", 
                      "premise", "street_no", "street_name", "street_type", "street_suffix", 
                      "city", "zip","longitude","latitude")
# 2021
houston21 <- read_excel("data/source/annual/houston_NIBRS2021.xlsx", 
                        col_types = c(column_types))
names(houston21) <- c(column_names)
# 2020
houston20 <- read_excel("data/source/annual/houston_NIBRS2020.xlsx", 
                        col_types = c(column_types))
names(houston20) <- c(column_names)
# 2019
houston19 <- read_excel("data/source/annual/houston_NIBRS2019.xlsx", 
                        col_types = c(column_types))
names(houston19) <- c(column_names)

# Bind years into single file
houston_annual <- bind_rows(houston19,houston20,houston21,houston22,houston23)

# Save as RDS for use in other tracker scripts; results in 10.9MB RDS
saveRDS(houston_annual,"scripts/rds/houston_annual.rds")

# Clean up
rm(houston_annual,houston19,houston20,houston21,houston22,houston23)
