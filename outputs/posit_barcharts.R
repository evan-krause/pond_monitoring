#Author: Evan Krause
#created: September 2026
#bar chart (mean +/- SE) comparisons of 2025 vs 2026 at LP2/LP3 for
#Long Pond monitoring data -- same parameters/sites as the presentation
#boxplots in outputs/posit_boxplots.R

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

# bar chart function ----

## one bar chart per parameter, restricted to LP2/LP3: station on x-axis,
## year as dodged fill, bar height = mean, error bar = +/- 1 standard error.
## set exclude_bad_cast = TRUE for parameters affected by the known-bad
## sensor cast at lp3, 2026-07-20, surface (currently just spc; that cast's
## temp_c/do/chla readings were checked and are not anomalous)
plot_param_barchart <- function(param, data = lp_dat, exclude_bad_cast = FALSE) {
  df <- data |>
    filter(station %in% c("lp2", "lp3"), !is.na(.data[[param]])) |>
    mutate(station = fct_drop(station))

  if (exclude_bad_cast) {
    df <- df |>
      filter(!(station == "lp3" & date == as.Date("2026-07-20") & depth_m < 0.1))
  }

  summ <- df |>
    group_by(station, year) |>
    summarise(
      n = n(),
      mean = mean(.data[[param]]),
      se = sd(.data[[param]]) / sqrt(n),
      .groups = "drop"
    )

  ggplot(summ, aes(x = station, y = mean, fill = year)) +
    geom_col(position = position_dodge2(preserve = "single"), width = 0.7) +
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      position = position_dodge2(padding = 0.5, preserve = "single"),
      width = 0.2
    ) +
    scale_x_discrete(labels = c(lp2 = "LP2", lp3 = "LP3")) +
    scale_fill_viridis_d(name = "Year") +
    labs(
      x = "Station", y = paste0("Mean ", param, " (\u00b1 SE)"),
      title = paste("Mean", param, "by site and year")
    ) +
    theme_minimal(base_size = 14)
}

# generate + save bar charts for the presentation parameter set ----

barchart_params <- list(
  temp_c = FALSE,
  do = FALSE,
  spc = TRUE, #excludes the known-bad lp3 2026-07-20 surface cast
  chla = FALSE
)

barchart_plots <- purrr::imap(
  barchart_params,
  ~ plot_param_barchart(.y, exclude_bad_cast = .x)
)

## save each plot to disk ----
barchart_out_dir <- "outputs/barcharts"
if (!dir.exists(barchart_out_dir)) {
  dir.create(barchart_out_dir, recursive = TRUE)
}

walk2(
  barchart_plots, names(barchart_plots),
  ~ ggsave(
    filename = file.path(barchart_out_dir, paste0(.y, "_lp2_lp3.png")),
    plot = .x,
    width = 7, height = 5, dpi = 150
  )
)

# median +/- IQR alternative for right-skewed parameters ----
# chla and do (particularly chla) are right-skewed, so a mean-based bar
# chart can overstate central tendency relative to most observations;
# these medians/IQR give a more robust alternative view of the same
# comparison

## one bar chart per parameter: station on x-axis, year as dodged fill,
## bar height = median, error bar = IQR (25th-75th percentile)
plot_param_median_barchart <- function(param, data = lp_dat, exclude_bad_cast = FALSE) {
  df <- data |>
    filter(station %in% c("lp2", "lp3"), !is.na(.data[[param]])) |>
    mutate(station = fct_drop(station))

  if (exclude_bad_cast) {
    df <- df |>
      filter(!(station == "lp3" & date == as.Date("2026-07-20") & depth_m < 0.1))
  }

  summ <- df |>
    group_by(station, year) |>
    summarise(
      n = n(),
      median = median(.data[[param]]),
      q1 = quantile(.data[[param]], 0.25),
      q3 = quantile(.data[[param]], 0.75),
      .groups = "drop"
    )

  ggplot(summ, aes(x = station, y = median, fill = year)) +
    geom_col(position = position_dodge2(preserve = "single"), width = 0.7) +
    geom_errorbar(
      aes(ymin = q1, ymax = q3),
      position = position_dodge2(padding = 0.5, preserve = "single"),
      width = 0.2
    ) +
    scale_x_discrete(labels = c(lp2 = "LP2", lp3 = "LP3")) +
    scale_fill_viridis_d(name = "Year") +
    labs(
      x = "Station", y = paste0("Median ", param, " (IQR)"),
      title = paste("Median", param, "by site and year")
    ) +
    theme_minimal(base_size = 14)
}

median_barchart_params <- c("chla", "do")

median_barchart_plots <- purrr::map(median_barchart_params, plot_param_median_barchart) |>
  purrr::set_names(median_barchart_params)

walk2(
  median_barchart_plots, names(median_barchart_plots),
  ~ ggsave(
    filename = file.path(barchart_out_dir, paste0(.y, "_lp2_lp3_median.png")),
    plot = .x,
    width = 7, height = 5, dpi = 150
  )
)
