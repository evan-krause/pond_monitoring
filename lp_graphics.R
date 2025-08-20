#Author: Evan Krause
#created: August 2025
#data vis script for Long Pond monitoring
#data obtained from DRC_DWSP MS Access database in csv format

library(tidyverse)
library(gtsummary)
library(GGally)

source('src/lp_clean.R')


# summary statistics + tables ----


lp_dat |>
  select(3:length(lp_dat),
         -depth_m,
         -bga_rfu,
         -bga,
         -chla_rfu,
         -o2_sat) |>
  tbl_summary(by = station,
              statistic = list(all_continuous() ~ "{median} ({p25} - {p75})",
                               all_categorical() ~ "{n} {p}")) |>
  bold_labels() |>
  add_overall()
  

# graphics + plots ----

## pairplots ----

lp_dat |>
  filter(station == 'lp2',
         date == '2025-05-08') |>
  select(c(5,7,8,9,12,14)) |>
  ggpairs()

## point-plots ----

lp_dat |>
  ggplot() +
  geom_point(aes(Depth_m, pH)) +
  geom_line(aes(Depth_m, pH)) +
  # geom_point(aes(Depth_m, `Dissolved Oxygen`)) +
  geom_point(aes(Depth_m, I(Chlorophyll), colour = 1)) +
  facet_grid(~date) +
  coord_flip()
