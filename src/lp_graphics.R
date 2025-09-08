#Author: Evan Krause
#created: August 2025
#data vis script for Long Pond monitoring
#data obtained from DRC_DWSP MS Access database in csv format

library(tidyverse)
library(gtsummary)
library(GGally)
library(gt)
library(gtsummary)

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
  


lp_dat |>
  select(3:length(lp_dat)) |>
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


#column profile?
lp_dat |>
  ggplot(aes(color = c(Depth_m))) +
  geom_point(aes(Depth_m, pH)) +
  geom_line(aes(Depth_m, pH)) +
  # geom_point(aes(Depth_m, `Dissolved Oxygen`)) +
  geom_point(aes(Depth_m, Chlorophyll)) +
  geom_line(aes(Depth_m, Chlorophyll)) + 
  geom_point(aes(Depth_m, `Dissolved Oxygen`)) +
  geom_line(aes(Depth_m, `Dissolved Oxygen`)) +
  geom_point(aes(Depth_m, pH)) +
  geom_line(aes(Depth_m, pH)) +
  facet_wrap(~date, scales = 'free_x') +
  coord_flip() +
  scale_x_reverse()

##barplots----

###lp_means by month plots----
#mean temp
lp_means |>
  ggplot() +
  geom_col(aes(x = month(date), y = mean_temp, fill = station), position = "dodge") +
  xlab("month") +
  ylab("Temperature (deg-C)")

#mean chla
lp_means |>
  ggplot() +
  geom_col(aes(x = month(date), y = mean_chla, fill = station), position = "dodge") +
  xlab("month") +
  ylab("Chla (mg/L)")

#mean DO

#mean DO
lp_means |>
  ggplot() +
  geom_col(aes(x = month(date), y = mean_do, fill = station), position = "dodge") +
  geom_hline(yintercept = 5) +
  xlab("month") +
  ylab("DO (mg/L)") +
  scale_fill_viridis_d(option = 'D') +
  theme(
    legend.position = 'bottom',
    legend.key = element_rect(color = 'black'),
    legend.background = element_rect(fill = 'white'),
    panel.background = element_rect(fill = "gray")
  )