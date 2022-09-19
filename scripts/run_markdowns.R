library(rmarkdown)


rmarkdown::render('scripts/Houston_Safety_Tracker.Rmd', 
                  output_dir = "docs",
                  output_file = 'Houston_Safety_Tracker.html')

rmarkdown::render('scripts/Houston_Safety_Tracker_Burglaries.Rmd', 
                  output_dir = "docs",
                  output_file = "Houston_Safety_Tracker_Burglaries.html")

rmarkdown::render('scripts/Houston_Safety_Tracker_Thefts.Rmd', 
                  output_dir = "docs",
                  output_file = 'Houston_Safety_Tracker_Thefts.html')

rmarkdown::render('scripts/Houston_Safety_Tracker_AutoThefts.Rmd', 
                  output_dir = "docs",
                  output_file = 'Houston_Safety_Tracker_AutoThefts.html')

rmarkdown::render('scripts/Houston_Safety_Tracker_Robberies.Rmd', 
                  output_dir = "docs",
                  output_file = 'Houston_Safety_Tracker_Robberies.html')

rmarkdown::render('scripts/Houston_Safety_Tracker_Assaults.Rmd', 
                  output_dir = "docs",
                  output_file = 'Houston_Safety_Tracker_Assaults.html')

rmarkdown::render('scripts/Houston_Safety_Tracker_SexualAssaults.Rmd', 
                  output_dir = "docs",
                  output_file = 'Houston_Safety_Tracker_SexualAssaults.html')

rmarkdown::render('scripts/Houston_Safety_Tracker_DrugCrimes.Rmd',
                  output_dir = "docs",
                  output_file = 'Houston_Safety_Tracker_DrugCrimes.html')

