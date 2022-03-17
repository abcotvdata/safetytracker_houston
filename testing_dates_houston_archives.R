## THIS CAN BE COMMENTED OUT/REMOVED LATER
# This merger of csv files includes 1.05M records
# There are 2,986 apparently invalid dates prior to 2010; many far prior
# That is 0.2% or 2/10ths of 1 percent of archive dataset; 99.8% are valid
# We did some testing to see which files were most to blame; and to devise plan
# DECISION: Drop invalid dates; after using only 10 years 2012-present, we're up 99.94% are valid

# Identify which files contained the dates before 2010 and how many
datetest1 <- df_csv %>%
  filter(date<=as.Date("2010-01-01")|date>=as.Date("2018-07-01")) %>%
  group_by(id) %>%
  summarise(count_invalid = n())
# Identify median date in each file to assess 'month' of each file id
datetest2 <- df_csv %>%
  group_by(id) %>%
  summarise(count_all = n(),median=median(date, na.rm=TRUE))
# Combine into single file for analysis
datetest <- full_join(datetest1,datetest2,by="id")
rm(datetest1,datetest2)
datetest$year <- substr(datetest$median,1,4)
datetest$year_month <- substr(datetest$median,1,7)
datetest$month <- substr(datetest$median,6,7)
# Pivot to determine number/share of invalid dates by year
datetest_byyear <- datetest %>%
  group_by(year) %>%
  summarise(invalid_dates = sum(count_invalid,na.rm=TRUE),
            count_all = sum(count_all,na.rm=TRUE))
datetest_byyear$pct_of_bad_dates <- datetest_byyear$invalid_dates/datetest_byyear$count_all*100
# 70% of all bad dates are in 2010 files, 80% are in 2010 and 2011 files
# Reducing to 10 years of data would cut invalid dates to several hundred, <0.1% of all
# This would give us 10 full years of historic data, plus the evolving 2022 to date data
# Reprocess with this fix brings down to 621 total invalid dates out of 789,424 records, <0.08%
# If calculated as a percentage in addition to modern 2018 records, it is .06%
# Some are fixable, but probably not worth it and would require assumptions about typos
# DECISION: Eliminate invalid dates outside range

# Save but don't use; for re-evaluating later whether to fix small number of fixable invalid dates
# Which crimes have invalid dates
# df_csv_check <- df_csv %>% filter(date<=as.Date("2009-12-31")|date>=as.Date("2019-01-01"))
# df_csv_check$month <- substr(df_csv_check$date,6,7)
# df_csv_check2 <- inner_join(df_csv_check,datetest,by=c("id"="id","month"="month"))