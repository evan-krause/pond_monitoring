###
# Holding ponds monitoring data prep
# Author: Evan Krause
# Created: 7/15/2026
# Last updated: 9/15/2026
# data cleaning script for Holding pond monitoring
# data obtained from DCR_DWSP MS Access database in csv or xlsx format
###


library(tidyverse)
library(readxl)
library(visdat)

 #import data from source #from csv
file <- file.choose()

raw_dat <- if (str_detect(file, ".csv") == TRUE) {
  read_csv(file)
} else {
  read_excel(file)
} 


hpm_dat <- raw_dat |> pivot_wider(
  #pivot data into wide format, creating col names from Parameter
  names_from = Parameter,
  values_from = FinalResult,
  id_cols = c(Depth_m, DateTimeET, Station)
) |>
  mutate(
    station = as.factor(Station),
    date = as.Date(DateTimeET),
    year = as.factor(year(DateTimeET)),
    month = as.factor(month(DateTimeET)),
    across(where(is.numeric), ~ if_else(. < 0, 0, .))
  ) |> #factor and date coercion
  rename(
    #rename cols to more usable formats
    depth_m = "Depth_m",
    datetime = "DateTimeET",
    temp_c = "Water Temperature",
    o2_sat = "Oxygen Saturation",
    do = "Dissolved Oxygen",
    spc = "Specific Conductance",
    turbid_fnu = "Turbidity FNU",
    bga_rfu = "Blue Green Algae RFU",
    bga = "Blue Green Algae",
    chla_rfu = "Chlorophyll RFU",
    chla = "Chlorophyll"
  ) |>
  relocate(where(is.numeric), .after = last_col()) |> # move identifier cols to front
  select(-Station) |> #remove old station col
  filter(station %in% c("301", "302")) #filter by station



vis_miss(hpm_dat) # visualize missing data (if applicable)

#replace negatives with zeros in applicable columns 

#Summary stats----
# 
# pond_means <- hpm_dat |>
#   # filter(station == 'lp1')|>
#   # group_by(date, station)|>
#   summarize(.by = c(date, station),
#             mean_temp = mean(temp_c),
#             mean_do = mean(do),
#             mean_chla = mean(chla),
#             mean_spc = mean(spc),
#             mean_ph = mean(pH))
# 

