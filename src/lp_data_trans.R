library(tidyverse)
library(visdat)

source("src/lp_clean.R")

lp_dat <- read_rds("data/lp_cleaned.rds")

#Summary stats----

lp_means <- lp_dat |>
  summarize(
    .by = c(year, date, station),
    mean_temp = mean(temp_c),
    mean_do = mean(do),
    mean_chla = mean(chla),
    mean_spc = mean(spc),
    mean_ph = mean(pH)
  )



#for by-year comparison----

