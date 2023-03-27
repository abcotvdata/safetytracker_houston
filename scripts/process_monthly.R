library(tidyverse)
library(readxl)
library(rvest)

### IMPORT AND FORMAT 2022 DATA TO DATE FROM HOUSTON PD EXCEL UPDATED MONTHLY HERE
webpage <- read_html("https://www.houstontx.gov/police/cs/Monthly_Crime_Data_by_Street_and_Police_Beat.htm")
# extract all a tags
a_tags <- html_nodes(webpage, "a")
# extract all links from a tags
links <- html_attr(a_tags, "href")
# xlsx links contain xls, so filtering to just those
links <- grep("xls", links, value = TRUE)
# the first xls file will always be the file we want
# download the file to the source data directory; building the url in process
try(download.file(paste0("https://www.houstontx.gov/police/cs/",links[[1]]),
              "data/source/annual/houston_NIBRS2023.xlsx"))

houston23 <- read_excel("data/source/annual/houston_NIBRS2023.xlsx", 
                        col_types = c("text", "date", "numeric", 
                                      "text" , "text", "numeric", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text","numeric","numeric"))
names(houston23) <- c("incident", "date", "hour", 
                      "nibrs_class", "offense_type", "offense_count", "beat", 
                      "premise", "street_no", "street_name", "street_type", "street_suffix", 
                      "city", "zip","longitude","latitude")

# Save as RDS for use in other tracker scripts; results in 4.2MB RDS
saveRDS(houston23,"scripts/rds/houston_monthly.rds")

# Clean up
rm(houston23,webpage,a_tags,links)