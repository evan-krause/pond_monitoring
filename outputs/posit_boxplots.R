#Author: Evan Krause
#created: September 2026
#boxplots comparing parameter distributions across site and year for
#Long Pond monitoring data

pkg <- c('tidyverse')

installed_packages <- pkg %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkg[!installed_packages])
}

library(tidyverse)

# data ----
# paths are relative to the project root; run this script from there
# (or with that as the working directory) rather than from outputs/

lp_cleaned_path <- "data/lp_cleaned.rds"

if (file.exists(lp_cleaned_path)) {
  lp_dat <- readRDS(lp_cleaned_path) #load previously cleaned data
} else {
  source('src/lp_clean.R') #no cached data; run cleaning script (prompts for source file)
}

# boxplot function ----

## one boxplot per parameter: station on x-axis, year as dodged fill.
## only station/year groups with at least min_n non-missing observations are
## kept, and a NULL is returned (no plot produced) if that leaves no data at
## all for the parameter -- avoids "boxplots" built from a handful of points
plot_param_boxplot <- function(param, data = lp_dat, min_n = 5) {
  df <- data |>
    filter(!is.na(.data[[param]])) |>
    group_by(station, year) |>
    filter(n() >= min_n) |>
    ungroup()

  if (nrow(df) == 0) return(NULL)

  ggplot(df, aes(x = station, y = .data[[param]], fill = year)) +
    geom_boxplot(position = position_dodge2(preserve = "single")) +
    scale_fill_viridis_d(name = "Year") +
    labs(
      x = "Station", y = param,
      title = paste("Distribution of", param, "by site and year")
    ) +
    theme_minimal()
}

# generate + save boxplots for every parameter ----

params <- c("temp_c", "o2_sat", "do", "spc", "pH", "turbid_fnu", "bga_rfu", "bga", "chla_rfu", "chla")

boxplot_plots <- purrr::map(params, plot_param_boxplot) |>
  purrr::set_names(params) |>
  purrr::compact() #drop any parameter with no plot-worthy data

## save each plot to disk ----
boxplot_out_dir <- "outputs/boxplots"
if (!dir.exists(boxplot_out_dir)) {
  dir.create(boxplot_out_dir, recursive = TRUE)
}

walk2(
  boxplot_plots, names(boxplot_plots),
  ~ ggsave(
    filename = file.path(boxplot_out_dir, paste0(.y, ".png")),
    plot = .x,
    width = 7, height = 5, dpi = 150
  ))

# presentation figures ----
# one-off, cleaned-up versions of specific plots for use in slide decks,
# restricted to the sites/years relevant to a given presentation

## spc, LP2 + LP3 only ----
spc_presentation_plot <- lp_dat |>
  filter(
    station %in% c("lp2", "lp3"),
    !is.na(spc),
    # excludes a single known-bad sensor cast (lp3, 2026-07-20, surface),
    # which also produced an implausible pH reading of 17.7 at the same cast
    !(station == "lp3" & date == as.Date("2026-07-20") & depth_m < 0.1)
  ) |>
  mutate(station = fct_drop(station)) |>
  ggplot(aes(x = station, y = spc, fill = year)) +
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  scale_x_discrete(labels = c(lp2 = "LP2", lp3 = "LP3")) +
  scale_fill_viridis_d(name = "Year") +
  labs(
    x = "Station", y = "Specific conductance (\u00b5S/cm)",
    title = "Specific conductance by site and year"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  filename = file.path(boxplot_out_dir, "spc_lp2_lp3_presentation.png"),
  plot = spc_presentation_plot,
  width = 7, height = 5, dpi = 150
)