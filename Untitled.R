library(tidycensus)
library(tidyverse)

# state agencies list 2023, scrapeable
# https://www.dps.texas.gov/section/crime-records/incident-based-reporting-approved-agencies

# downloadable version of same, texas arresting agencies also with ORI codes
# https://www.dps.texas.gov/sites/default/files/documents/administration/crime_records/docs/cjis/arrestingagencyoris.xls

# nbirs monthly crime summary report api address; detailed by month with clearance
# https://txucr.nibrs.com/Report/DownloadNIBRSMonthlyCrimeSummaryReportExcel?reportby=Agency&reportids=889,1012&year=2023

