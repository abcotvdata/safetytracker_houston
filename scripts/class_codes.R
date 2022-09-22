# Build a class-code table to classify offense types and categories
classcodes <- houston_crime %>%
  group_by(offense_type,nibrs_class) %>%
  summarise(count=n())

classcodes$category_code <- substr(classcodes$nibrs_class,1,2)
classcodes$category_name <- case_when(classcodes$category_code == "09" ~ "Murder",
                                      classcodes$category_code == "10" ~ "Kidnapping",
                                      classcodes$category_code == "11" ~ "Sexual Assault",
                                      classcodes$category_code == "12" ~ "Robbery",
                                      classcodes$category_code == "13" ~ "Assault",
                                      classcodes$category_code == "20" ~ "Arson",
                                      classcodes$category_code == "22" ~ "Burglary",
                                      classcodes$category_code == "23" ~ "Theft",
                                      classcodes$category_code == "24" ~ "Auto Theft",
                                      classcodes$category_code == "28" ~ "Theft",
                                      classcodes$category_code == "35" ~ "Drug Offenses",
                                      TRUE ~ "Other")

# add violent and property flags
classcodes$type <- case_when(classcodes$category_code == "09" ~ "Violent",
                             classcodes$category_code == "11" ~ "Violent",
                             classcodes$category_code == "12" ~ "Violent",
                             classcodes$category_code == "20" ~ "Property",
                             classcodes$category_code == "22" ~ "Property",
                             classcodes$category_code == "23" ~ "Property",
                             classcodes$category_code == "24" ~ "Property",
                             classcodes$category_code == "28" ~ "Property",
                             classcodes$nibrs_class == "13A" ~ "Violent",
                             classcodes$nibrs_class == "13B" ~ "Violent",
                             TRUE ~ "Other")

# add flag for 'major' crimes, generally considered 'Part I' crimes in FBI data
classcodes$major <- case_when(classcodes$category_code == "09" ~ "Major",
                              classcodes$category_code == "11" ~ "Major",
                              classcodes$category_code == "12" ~ "Major",
                              classcodes$category_code == "20" ~ "Major",
                              classcodes$category_code == "22" ~ "Major",
                              classcodes$category_code == "23" ~ "Major",
                              classcodes$category_code == "24" ~ "Major",
                              classcodes$category_code == "28" ~ "Major",
                              classcodes$nibrs_class == "13A" ~ "Major",
                              TRUE ~ "Other")

# save csv for backup
write_csv(classcodes,"data/output/reference/classcodes.csv")
# save rds to preserve df/format for other scripts
saveRDS(classcodes,"scripts/rds/classcodes.rds")
