library("rmarkdown")

# clearance <- filter(clearance, year > 1997)
# clearance18 <- filter(clearance, year == 2018)
# meanviol <- round(mean(clearance18$violent_pct, na.rm = TRUE),1)
# meanprop <- round(mean(clearance18$property_pct, na.rm = TRUE),1)
# agencyclearance <- filter(clearance, ncic_code == agency)

# in a single for loop
#  1. define subgroup
#  2. render output
for (beat in unique(sexualassault_by_beat$beat)){
  subgroup <- sexualassault_by_beat[sexualassault_by_beat$beat == beat,]
  render("neighborhood_report.rmd",
         output_file = paste0('report.', beat, '.html'),
         output_dir = 'beat_sexualassuault_reports')
}

