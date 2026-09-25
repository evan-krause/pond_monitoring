#author: Evan Krause
#last update: 2026-09-24
# 


pkg <- c("tidyverse")

installed_packages <- pkg %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkg[!installed_packages])
}


clean_path <- paste0("data/lp_cleaned.rds")

if (file.exists(clean_path)) {
  lp_dat <- readRDS(clean_path) #load previously cleaned data
} else {
  source('src/pond_clean.R') #if no cached data; run "pond_clean.R" for ETL
}

#Summary stats----
lp_means <- lp_dat |>
  dplyr::summarize(
    .by = c(year, date, station),
    mean_temp = mean(temp_c),
    mean_do = mean(do),
    mean_chla = mean(chla),
    mean_spc = mean(spc),
    mean_ph = mean(pH)
  )

