###
# Holding ponds monitoring data prep
# Author: Evan Krause
# Created: 7/15/2026
# Last updated: 9/16/2026
# data cleaning script for Holding pond monitoring
# data obtained from DCR_DWSP MS Access database in csv or xlsx format
###


library(tidyverse)
library(readxl)
library(visdat)

# data import ----
#choose file from dialog
file <- file.choose()

raw_dat <- if (stringr::str_detect(file, ".csv") == TRUE) {
  dplyr::read_csv(file)
} else {
  readxl::read_excel(file)
} 

# data cleaning ----
hpm_dat <- raw_dat |> tidyr::pivot_wider(
  #pivot data into wide format, creating col names from Parameter
  names_from = Parameter,
  values_from = FinalResult,
  id_cols = c(Depth_m, DateTimeET, Station)
) |>
  dplyr::mutate(  #factor and date coercion
    station = as.factor(Station),
    date = as.Date(DateTimeET),
    year = as.factor(lubridate::year(DateTimeET)),
    month = as.factor(lubridate::month(DateTimeET)),
    dplyr::across(dplyr::where(is.numeric), ~ dplyr::if_else(. < 0, 0, .)) 
    # for all numeric cols, if result is less than zero, become zero. Else, no change
  ) |>
  dplyr::rename(
    #rename cols to more tidy format
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
  dplyr::relocate(dplyr::where(is.numeric), .after = last_col()) |> # move identifier cols to front
  dplyr::select(-Station) |> #remove old station col
  dplyr::filter(station %in% c("301", "302")) #filter by station

hpm_dat$station <- droplevels(hpm_dat$station) #drop unused factor levels from remaining dataset

visdat::vis_miss(hpm_dat) # visualize for missing data (if applicable)

saveRDS(hpm_dat, file = "data/hpm_cleaned.rds") #save cleaned data to rds for analysis/viz

