library(tidyverse)
library(readxl)
# library(data.table)
# library(fs)
library(rio)
install_formats() # installs some suggested packages related to rio
# library(tibble)
library(lubridate)


# DOWNLOAD ARCHIVED DATA
# See separate script: download_houston_archives.R
# Collects archived monthly crime-by-beat files for 2010-2018 from Houston PD
# Should not need to re-run but saved in separate script just in case
# source("download_houston_archives.R")

# PROCESS THE FILES IN TWO GROUPS
# There are a set of xls files and xlsx files with significant format differences
# We are going to batch process each separately, then reformat and clean separately

# XLSX files: create list, bind and clean
# These files are the newest batch from 2018 from a file format change
# Creates list of files in data dir; full.names ensures list includes dir path
filelist_xlsx <- list.files(path="./data/xlsx",pattern='*.xlsx',full.names = T)
# Applies read_excel to each file in the list created above; creates list of dataframes
dflist_xlsx <- lapply(filelist_xlsx, read_excel)
# Binds each dataframe in list into a single df, keeping list id for tracking/redundancy
df_xlsx <- bind_rows(dflist_xlsx, .id = "id")

# interim cleanup: remove some obviously stray columns
df_xlsx <- df_xlsx %>% select(1,2,4,6,8:11,13,16,18)
# name the columns remaining to the proper names; as consistent as possible to the earlier archive files
names(df_xlsx) <- c("id", "date", "hour", 
                      "offense_type", "offense_count", "beat", 
                      "premise", "block_range", "street_name", "street_type", "street_suffix")
# eliminate rows where there are file headers
# this also eliminates the blank/white space rows as a side effect
df_xlsx <- df_xlsx %>% filter(date!="Occurrence Date")
# change date to date format
df_xlsx$date <- ymd(df_xlsx$date)
# change offense count to a number format
df_xlsx$offense_count <- as.numeric(df_xlsx$offense_count)
# change the file id to a traceback to the year and month of HPD file, rather than number in our batch list
# first we need a median date for each original batch list file id, then turn that into a new file_id field
df_xlsx_idcreate <- df_xlsx %>%
  group_by(id) %>%
  summarise(median=median(date))
df_xlsx_idcreate$file_id <- substr(df_xlsx_idcreate$median,1,7)
# now merge that file_id into the df
df_xlsx <- left_join(df_xlsx,df_xlsx_idcreate %>% select(1,3),by="id")
rm(df_xlsx_idcreate)
# then drop id in favor of file_id
df_xlsx <- df_xlsx %>% select(2:12)

#######

# XLS files: requires batch conversion to csv first because of inconsistencies, invalid formats in HPD files
# IMPORTANT: If not done during download, xls for Oct17 on HPD site is corrupt; MANUAL OPEN, RESAVE/REPLACE
# Creates list of files in data dir; full.names ensures list includes dir path
# Can comment out once done first time
filelist_xls <- dir(path="./data",pattern = ".xls",full.names = TRUE)
# Converts every xls file in the directory to a csv, eliminating excel formatting problems
# Can comment out once done first time
mapply(convert, filelist_xls, gsub("xls", "csv", filelist_xls))
rm(filelist_xls)
# new file list of csvs we just created/converted
filelist_csv <- list.files(path="./data",pattern='*.csv',full.names = T)
# importing the files with all col types set to character to solve for inconsistencies
dflist_csv <- lapply(filelist_csv, read_csv, col_types = "c")

##### Just to save this code snippet for converting a single xls file to csv without importing
# convert("data/houston_NIBRS_sep10.xls", "data/september10.csv")

# First attempt to bind rows into one clean table
# Creates some consistency/cleanup issues and format issues
df_csv <- bind_rows(dflist_csv, .id = "id") %>% janitor::clean_names()
# Merges data from two block_range fields with inconsistent names
df_csv$block_range <- ifelse(is.na(df_csv$block_range),df_csv$block_range_2,df_csv$block_range)
df_csv$street_name <- ifelse(is.na(df_csv$street_name),df_csv$street_name_2,df_csv$street_name)
# Merges data from five number/count of offenses fields with inconsistent names
df_csv$number_of_offenses <- ifelse(is.na(df_csv$number_of_offenses),df_csv$number_offenses,df_csv$number_of_offenses)
df_csv$number_of_offenses <- ifelse(is.na(df_csv$number_of_offenses),df_csv$offenses,df_csv$number_of_offenses)
df_csv$number_of_offenses <- ifelse(is.na(df_csv$number_of_offenses),df_csv$number_offenses_2,df_csv$number_of_offenses)
df_csv$number_of_offenses <- ifelse(is.na(df_csv$number_of_offenses),df_csv$number_of,df_csv$number_of_offenses)
# rename some fields to be consistent with field names in later data
df_csv <- df_csv %>% rename("offense_count"="number_of_offenses","street_type"="type","street_suffix"="suffix")
# interim cleanup: keep only the columns we still need
df_csv <- df_csv %>% select(1:11)
# DATES: resolving and standardizing multiple date formats in date column
# Strip character/date field to first 10 to capture just dates from 2 formats
df_csv$date <- substr(df_csv$date,1,10)
# Use lubridate to create two new fields convert character dates to proper format
# This will prompt a warning message and insert NA in columns for 'wrong' format; merge will fix later
df_csv$date1 <- mdy(df_csv$date) 
df_csv$date2 <- ymd(df_csv$date)
# Merge two new date fields into a single, properly formatted date field
df_csv$date3 <- as_date(ifelse(is.na(df_csv$date1),df_csv$date2,df_csv$date1))
# Eliminate the original character date field from the file; rename date3 to date
df_csv$date <- NULL
df_csv <- df_csv %>% dplyr::rename("date"="date3")
# Cleanup: reduce to just the fields we will need from here, reorder columns
df_csv <- df_csv %>% select(1,13,2,3,10,4:9)
# Filter out invalid dates, outside range of period HPD says data represents
# see notes at end of file, if desired, for rationale, process and separate testing file
df_csv <- df_csv %>% filter(date>=as.Date("2012-01-01")&date<=as.Date("2018-12-31"))
# change the file id to a traceback to the year and month of HPD file, rather than number in our batch list
# first we need a median date for each original batch list file id, then turn that into a new file_id field
df_csv_idcreate <- df_csv %>%
  group_by(id) %>%
  summarise(median=median(date))
df_csv_idcreate$file_id <- substr(df_csv_idcreate$median,1,7)
# now merge that file_id into the df
df_csv <- left_join(df_csv,df_csv_idcreate %>% select(1,3),by="id")
rm(df_csv_idcreate)
# then drop id in favor of file_id
df_csv <- df_csv %>% select(2:12)

# merge the archived files into one clean file
houston_crimes_archive <- bind_rows(df_csv,df_xlsx)
houston_crimes_archive$year <- substr(houston_crimes_archive$date,1,4)
rm(df_csv,df_xlsx,dflist_csv,dflist_xlsx)

# some quick cleanup of offense_types
houston_crimes_archive$offense_type <- case_when(houston_crimes_archive$offense_type == "AutoTheft" ~ "Motor vehicle theft",
                                                 houston_crimes_archive$offense_type == "Auto Theft" ~ "Motor vehicle theft",
                                                 houston_crimes_archive$offense_type == "Burglary" ~ "Burglary, Breaking and Entering",
                                                 houston_crimes_archive$offense_type == "1" ~ "All other offenses",
                                                 TRUE ~ houston_crimes_archive$offense_type
                                    )
# change hour to numeric for marrying the more recent years data
houston_crimes_archive$hour <- as.numeric(houston_crimes_archive$hour)

# once again, rename some fields to be consistent with field names in later data
houston_crimes_archive <- houston_crimes_archive %>% rename("street_no"="block_range")


# quick look crimes by year
houston_crimes_archiveyears <- houston_crimes_archive %>%
  group_by(year,offense_type) %>%
  summarise(count=n()) %>% 
  pivot_wider(names_from=year,values_from = count)
houston_crimes_archiveyears <- left_join(houston_crimes_archiveyears,list,by="offense_type")

classcodes <- houston_crimes_archiveyears %>% select(offense_type,nibrs_class)
classcodes$category_code <- substr(classcodes$nibrs_class,1,2)

# add two rows to class codes
# classcodes <- classcodes %>% add_row(offense_type = "Incest",
#                       nibrs_class = "36A",
#                       category_code = "36",
#                       category_name = "Other")
#classcodes <- classcodes %>% add_row(offense_type = "Justifiable homicide",
#                                     nibrs_class = "09C",
#                                     category_code = "09",
#                                     category_name = "Murder")

# some quick cleanup on classing crime names and codes
classcodes$category_code <- case_when(classcodes$offense_type == "Murder" ~ "09",
                                  classcodes$offense_type == "Rape" ~ "11",
                                  classcodes$offense_type == "Theft" ~ "23",
                                                 TRUE ~ classcodes$category_code)
classcodes$category_name <- case_when(classcodes$category_code == "09" ~ "Murder",
                                  classcodes$category_code == "10" ~ "Kidnapping",
                                  classcodes$category_code == "11" ~ "Sexual Assault",
                                  classcodes$category_code == "12" ~ "Robbery",
                                  classcodes$category_code == "13" ~ "Assault",
                                  classcodes$category_code == "20" ~ "Arson",
                                  classcodes$category_code == "21" ~ "Other",
                                  classcodes$category_code == "22" ~ "Burglary",
                                  classcodes$category_code == "23" ~ "Theft",
                                  classcodes$category_code == "24" ~ "Auto Theft",
                                  classcodes$category_code == "25" ~ "Other",
                                  classcodes$category_code == "26" ~ "Other",
                                  classcodes$category_code == "27" ~ "Other",
                                  classcodes$category_code == "28" ~ "Theft",
                                  classcodes$category_code == "29" ~ "Other",
                                  classcodes$category_code == "35" ~ "Drug offenses",
                                  classcodes$category_code == "36" ~ "Other",
                                  classcodes$category_code == "37" ~ "Other",
                                  classcodes$category_code == "39" ~ "Other",
                                  classcodes$category_code == "40" ~ "Other",
                                  classcodes$category_code == "51" ~ "Other",
                                  classcodes$category_code == "52" ~ "Other",
                                  classcodes$category_code == "64" ~ "Other",
                                  classcodes$category_code == "72" ~ "Other",
                                  classcodes$category_code == "90" ~ "Other",
                                  TRUE ~ "")

######
## NOTES ABOUT DATE VALIDATION/TESTING
# Original merger of csv files dating to 2010 had 1.05M records
# Of those records, 2,986 apparently invalid dates prior to 2010; many far prior to time period
# That is 0.2% (2/10ths of 1%) of entire archive datasets; 99.8% are valid going back to 2010
# Did some testing to see which files were most to blame; vast majority of invalid dates in 2010 & 2011
# DECISION: Use 2012-present, which will give us 10 full years of data, 2012-2021, plus 2022 to date
# This brings us down to 789,424 records for the xls-to-csv files covering 2012 through mid-2018
# This reduced the share of records with invalid dates to 613 or 0.07%. Meaning 99.94% remaining dates are valid
# DECISION: Eliminate invalid/outside-time-range dates
# This reduces our total number of records with valid dates to 788,811
# Code for this testing is saved in testing_dates_houston_archives.R
# Running it to re-test requires not running the last line of code (date filter) in processing csvs
