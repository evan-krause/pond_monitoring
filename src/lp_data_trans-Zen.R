#author: Evan Krause
#last update: 2026-09-28
#purpose: to summarize 


pkg <- c("readxl", "tidyverse")

installed_packages <- pkg %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkg[!installed_packages])
}

sapply(pkg, library, character.only = TRUE)

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
    mean_temp = round(mean(temp_c, na.rm = TRUE), 3),
    mean_do = round(mean(do, na.rm = TRUE), 3),
    mean_chla = round(mean(chla, na.rm = TRUE), 3),
    mean_spc = round(mean(spc, na.rm = TRUE), 3),
    mean_ph = round(mean(pH, na.rm = TRUE), 3)
  )  #drop NA rows

pond_sum <- function(data, group.by) {
  data |>
    group_by({{group.by}}) |>
    summarize(
      max_depth = max(.data$depth_m),
      ph_range = round(max(.data$pH) - min(.data$pH), 2),
      avg_temp = mean(.data$temp_c),
      avg_do_pct = mean(.data$do),
      avg_chl_ugl = mean(.data$chla),
      n = n()
    ) |>
    arrange(desc(n))
}

summary_table_data <- lp_dat |>
  filter(station != "lp1", depth_m > 0.1, year == 2026) |>
  group_by(station) |>
  summarize(
    min = across(where(is.numeric), \(x) min(x, na.rm = TRUE)),
    mean = across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
    sd = across(where(is.numeric), \(x) sd(x, na.rm = TRUE)),
    med = across(where(is.numeric), \(x) median(x, na.rm = TRUE)),
    max = across(where(is.numeric), \(x) max(x, na.rm = TRUE))
  ) |>
  dplyr::ungroup()
