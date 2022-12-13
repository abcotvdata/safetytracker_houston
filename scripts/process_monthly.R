library(tidyverse)
library(readxl)

### IMPORT AND FORMAT 2022 DATA TO DATE FROM HOUSTON PD EXCEL UPDATED MONTHLY HERE
# OPEN WORK TO WRITE CODE THAT WOULD CHECK FOR NEW FILE AND THEN ONLY RUN IF NEW FILE EXISTS

try(download.file("https://www.houstontx.gov/police/cs/xls/NIBRSPublicViewJan-Nov_2022.xlsx","data/source/monthly/houston_NIBRS2022.xlsx"))

houston22 <- read_excel("data/source/monthly/houston_NIBRS2022.xlsx", 
                        col_types = c("text", "date", "numeric", 
                                      "text" , "text", "numeric", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text","numeric","numeric"))
names(houston22) <- c("incident", "date", "hour", 
                      "nibrs_class", "offense_type", "offense_count", "beat", 
                      "premise", "street_no", "street_name", "street_type", "street_suffix", 
                      "city", "zip","longitude","latitude")

# Save as RDS for use in other tracker scripts; results in 4.2MB RDS
saveRDS(houston22,"scripts/rds/houston_monthly.rds")

# Clean up
rm(houston22)