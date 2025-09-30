#Author: Evan Krause
#created: August 2025
#data vis script for Long Pond monitoring
#data obtained from DRC_DWSP MS Access database in csv format

pkg <- c('tidyverse', 'gtsummary', 'GGally', 'gt', 'gtsummary', 'kableExtra')

installed_packages <- pkg %in% rownames(installed.packages()) #check if necessary packages are installed
if (any(installed_packages == FALSE)) {
  install.packages(pkg[!installed_packages])#install if not in installed_packages list
}

source('src/lp_clean.R')

library(tidyverse)
library(gtsummary)
library(GGally)
library(gt)
library(gtsummary)
library(kableExtra)


report_theme <- theme(
  legend.position = 'top',
  legend.key = element_rect(color = 'black'),
  legend.background = element_rect(fill = 'white'),
  panel.background = element_rect(fill = "gray"),
  panel.grid = element_line(colour = 'white'),
  axis.text.x = element_text(angle = 0,
                             face = "bold")
)

# summary statistics + tables ----

## Kable tables----
sum_stats <- function(x) {
  c(
    "Min" = round(min(x), 3),
    "Median" = round(median(x), 3),
    "Max" = round(max(x), 3),
    "Mean" = round(mean(x), 3),
    "St.dev" = round(sd(x), 3)
  )
}

lp1_dat <- lp_dat |>
  filter(station == "lp1")
lp2_dat <- lp_dat |>
  filter(station == "lp2")
lp3_dat <- lp_dat |>
  filter(station == "lp3")

lp1_tbl <- sapply(lp1_dat[c(5, 7, 8, 9, 12, 14)], sum_stats)
lp2_tbl <- sapply(lp2_dat[c(5, 7, 8, 9, 12, 14)], sum_stats)
lp3_tbl <- sapply(lp3_dat[c(5, 7, 8, 9, 12, 14)], sum_stats)



kable_lp1 <- kable(
  lp1_tbl,
  col.names = c(
    "Temperature (Deg-C)",
    "Dissolved Oxygen (mg/L)",
    "Specific Conductivity (uS/cm)",
    "pH",
    "Phycocyanin (ug/L)",
    "Chlorophyll-a (ug/L)"
  )
) |>
  kable_styling("striped")

kable_lp2 <- kable(
  lp2_tbl,
  col.names = c(
    "Temperature (Deg-C)",
    "Dissolved Oxygen (mg/L)",
    "Specific Conductivity (uS/cm)",
    "pH",
    "Phycocyanin (ug/L)",
    "Chlorophyll-a (ug/L)"
  )
) |>
  kable_styling("striped")

kable_lp3 <- kable(
  lp3_tbl,
  col.names = c(
    "Temperature (Deg-C)",
    "Dissolved Oxygen (mg/L)",
    "Specific Conductivity (uS/cm)",
    "pH",
    "Phycocyanin (ug/L)",
    "Chlorophyll-a (ug/L)"
  )
) |>
  kable_styling("striped")

## Gtsummary tables ----
param_sum <- lp_dat |>
  select(3,4,5,7,14,8,9,
         -bga_rfu,
         -bga,
         -chla_rfu,
         -turbid_fnu,
         -o2_sat) |>
  tbl_summary(
    by = station,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      depth_m ~ "{max}",
      temp_c ~ "{median} ({min} - {max}) \n {mean} ({sd})",
      do ~ "{median} ({min} - {max}) \n {mean} ({sd})",
      chla ~ "{median} ({min} - {max}) \n {mean} ({sd})"
    ),
    label = list(
      depth_m = "Max depth (Meters)",
      temp_c = "Water Temperature (Deg-C)",
      do = "Dissolved Oxygen (mg/L)",
      chla = "Chlorophyll-a (ug/L)",
      spc = "Specific Conductance (uS/cm)"
    )
  ) |>
  add_overall(last = TRUE) |>
  modify_header(label = "**Parameters**",
                stat_1 = "**LP1**  \nN = 36",
                stat_2 = "**LP2**  \nN = 60",
                stat_3 = "**LP3**  \nN = 34")|>
  bold_labels() 
  

# graphics + plots ----

## pairplots ----

# lp_dat |>
#   # filter() |>
#   select(c(5,7,8,9,12,14)) |>
#   ggpairs()

## point-plots ----


#column profile?
# lp_dat |>
#   ggplot(aes(color = c(depth_m))) +
#   geom_point(aes(depth_m, pH)) +
#   geom_line(aes(depth_m, pH)) +
#   # geom_point(aes(Depth_m, `Dissolved Oxygen`)) +
#   geom_point(aes(depth_m, Chlorophyll)) +
#   geom_line(aes(depth_m, Chlorophyll)) + 
#   geom_point(aes(depth_m, `Dissolved Oxygen`)) +
#   geom_line(aes(depth_m, `Dissolved Oxygen`)) +
#   geom_point(aes(depth_m, pH)) +
#   geom_line(aes(depth_m, pH)) +
#   facet_wrap(~date, scales = 'free_x') +
#   coord_flip() +
#   scale_x_reverse()

## barplots----

###lp_means by month plots----
#mean temp
lp_means |>
  ggplot() +
  geom_col(aes(x = month(date), y = mean_temp, fill = station), position = "dodge") +
  labs(x = "Month",
       y = "Temperature (deg-C)",
       title = "Mean Temperature by Site|Month") +
  scale_fill_viridis_d(labels = c("LP1", "LP2", "LP3")) +
  report_theme

#mean chla
lp_means |>
  ggplot() +
  geom_col(aes(x = month(date), y = mean_chla, fill = station), position = "dodge") +
  labs(x = "Month",
       y = "Chla (mg/L)",
       title = "Mean Chlorophyll-a by Site|Month") +
  scale_fill_viridis_d(labels = c("LP1", "LP2", "LP3")) +
  report_theme

#mean DO
lp_means |>
  ggplot() +
  geom_col(aes(x = month(date), y = mean_do, fill = station), position = "dodge") +
  geom_hline(yintercept = 6) +
  labs(
    x = "Month",
    y = "DO (mg/L)",
    title = "Mean Dissolved Oxygen by Site|Month",
    subtitle = "6 mg/L reference"
  ) +
  scale_fill_viridis_d(labels = c("LP1", "LP2", "LP3")) +
  report_theme +
  theme(axis.title.y = element_text(size = rel(1), angle = 90),
        legend.text = element_text())

#mean SPC
lp_means |>
  ggplot() +
  geom_col(aes(x = month(date), y = mean_spc, fill = station), position = "dodge", ) +
  labs(
    x = "Month",
    y = "SPC (uS/cm)",
    title = "Mean Specific conductivity by Site|Month",
  ) +
  scale_fill_viridis_d(labels = c("LP1", "LP2", "LP3")) +
  report_theme

#mean SPC
lp_means |>
  ggplot() +
  geom_col(aes(x = month(date), y = mean_ph, fill = station), position = "dodge", ) +
  labs(
    x = "Month",
    y = "SPC (uS/cm)",
    title = "Mean Specific Conductivity by Site|Month",
  ) +
  scale_fill_viridis_d(labels = c("LP1", "LP2", "LP3")) +
  ylim(limits = c(0,7.5))+
  report_theme

