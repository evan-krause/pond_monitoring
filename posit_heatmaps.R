#created: September 2026
#depth-by-date (limnology profile) heatmaps for Long Pond monitoring
#interpolates depth x date grids per station/year so stratification patterns
#(e.g. thermocline, chlorophyll layers) are visible for each parameter

pkg <- c('tidyverse', 'interp')

installed_packages <- pkg %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkg[!installed_packages])
}

library(tidyverse)
library(interp)

# data ----

lp_cleaned_path <- "data/lp_cleaned.rds"

if (file.exists(lp_cleaned_path)) {
  lp_dat <- readRDS(lp_cleaned_path) #load previously cleaned data
} else {
  source('src/lp_clean.R') #no cached data; run cleaning script (prompts for source file)
}

# heatmap functions ----

## interpolate a single station/year onto a regular depth x date grid ----
make_profile_grid <- function(df, param, n_grid = 80) {
  df <- df |>
    filter(!is.na(depth_m), !is.na(.data[[param]])) |>
    distinct(date, depth_m, .keep_all = TRUE)

  empty <- tibble(date = as.Date(character()), depth_m = numeric(), value = numeric())

  # need enough distinct sampling dates/points for a stable interpolation
  if (n_distinct(df$date) < 3 || nrow(df) < 5) return(empty)

  fit <- interp(
    x = as.numeric(df$date),
    y = df$depth_m,
    z = df[[param]],
    duplicate = "mean",
    nx = n_grid,
    ny = n_grid,
    linear = TRUE
  )

  expand_grid(xi = seq_along(fit$x), yi = seq_along(fit$y)) |>
    mutate(
      date = as.Date(fit$x[xi]),
      depth_m = fit$y[yi],
      value = fit$z[cbind(xi, yi)]
    ) |>
    select(date, depth_m, value)
}

## build one heatmap for a given parameter + station ----
## years are faceted separately rather than interpolated across, since
## sampling seasons are separated by multi-month gaps with no data
plot_profile_heatmap <- function(param, station_id, data = lp_dat) {
  data <- data |>
    filter(station == station_id) |>
    mutate(yr = year(date))

  grid_dat <- data |>
    group_by(yr) |>
    group_modify(~ make_profile_grid(.x, param)) |>
    ungroup() |>
    filter(!is.na(value))

  points_dat <- data |> filter(!is.na(.data[[param]]))

  ggplot(grid_dat, aes(date, depth_m)) +
    geom_raster(aes(fill = value), interpolate = TRUE) +
    geom_point(data = points_dat, size = 0.6, color = "white", alpha = 0.6) +
    scale_y_reverse() +
    scale_x_date(date_labels = "%b '%y", date_breaks = "1 month") +
    scale_fill_viridis_c(name = param) +
    facet_wrap(~ yr, scales = "free_x", nrow = 1) +
    labs(
      x = "Date", y = "Depth (m)",
      title = paste0("Depth profile heatmap - ", param, " (", toupper(station_id), ")")
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# generate + save heatmaps for every parameter/station combination ----

params <- c("temp_c", "o2_sat", "do", "spc", "pH", "turbid_fnu", "bga_rfu", "bga", "chla_rfu", "chla")
stations <- levels(lp_dat$station)

combos <- expand_grid(param = params, station_id = stations)

profile_plots <- purrr::map2(
  combos$param, combos$station_id,
  ~ plot_profile_heatmap(.x, .y)
) |>
  purrr::set_names(paste(combos$param, combos$station_id, sep = "_"))

## save each plot to disk ----
heatmap_out_dir <- "outputs/heatmaps"
if (!dir.exists(heatmap_out_dir)) {
  dir.create(heatmap_out_dir, recursive = TRUE)
}

walk2(
  profile_plots, names(profile_plots),
  ~ ggsave(
    filename = file.path(heatmap_out_dir, paste0(.y, ".png")),
    plot = .x,
    width = 8, height = 5, dpi = 150
  )
)
