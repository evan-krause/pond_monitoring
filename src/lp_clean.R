#Author: Evan Krause
#created: August 2025
#data cleaning script for Long Pond monitoring
#data obtained from DRC_DWSP MS Access database in csv or xlsx format

library(tidyverse)
library(readxl)
library(visdat)

# pond_dat <- read_csv('data/pond_data.csv') #import data from source #from csv
pond_dat <- read_excel("data/long_pond_parameters.xlsx") # from DB xlsx

lp_dat <- pond_dat |> pivot_wider(
  #pivot data into wide format, creating col names from Parameter
  names_from = Parameter,
  values_from = FinalResult,
  id_cols = c(Depth_m, DateTimeET, Station)
) |>
  filter(Station %in% c("lp1", "LP1", "LP2", "LP3")) |> # keep only longPond sites
  mutate(date = as.Date(DateTimeET), station = as.factor(Station)) |> #factor and date coercion
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
  select(-Station)  #remove old station col 

lp_dat$station <- fct_collapse(
  lp_dat$station,
  #condense and lower site names
  lp1 = c("lp1", "LP1"),
  lp2 = "LP2",
  lp3 = "LP3"
)

vis_miss(lp_dat) # visualize missing data (if applicable)

#Summary stats----

lp_means <- lp_dat |>
  # filter(station == 'lp1')|>
  group_by(date, station)|>
  summarize(mean_temp = mean(temp_c),
            mean_do = mean(do),
            mean_chla = mean(chla),
            mean_spc = mean(spc),
            mean_ph = mean(pH))


