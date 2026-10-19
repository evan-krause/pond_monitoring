# ==============================================================================
# buoy_heatmaps.R
# Depth x date heatmaps from buoy profile data: one PNG per station x parameter,
# one panel per year (seasons are never interpolated across).
#
# Expects long-format raw data with columns:
#   Parameter, Result, Depth_m, DateTimeET (POSIXct with the ET zone attached), station
#
# Usage:
#   source("buoy_heatmaps.R")
#   raw_dat <- readr::read_csv("your_export.csv")   # however you load it
#   plots   <- make_all_heatmaps(raw_dat)           # builds, saves, returns plots
#   plots[["chla_Buoy_5"]]                          # view one
#
# Options (passed through make_all_heatmaps(...)):
#   bin_m            depth bin used to average readings (match your profiler's
#                    depth step, ~1 m here)                          (default 1)
#   depth_step       row height of the plotted grid, in metres       (default 0.5)
#   max_gap_days     largest gap between sampled dates that is bridged;
#                    wider gaps stay blank                           (default 7)
#   min_levels_frac  drop casts that reached fewer than this fraction of the
#                    usual number of depth levels                    (default 0.5)
#   clip_q           quantile limits for the color scale, NULL=none  (default c(.01,.99))
#   show_rug         tick marks along the bottom at each sampled date (default TRUE)
#   show_points      overlay every raw reading (hides the heatmap)   (default FALSE)
# ==============================================================================

# ---- packages ----------------------------------------------------------------
pkgs <- c("tidyverse", "scales")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
library(tidyverse)   # scales:: and lubridate:: are called with prefixes


# ---- settings ----------------------------------------------------------------
default_params <- c("temp_c", "o2_sat", "do", "spc", "pH", "turbid_fnu", "bga", "chla")

# tidy name = raw name. Raw columns missing from a file are skipped, not an error.
col_lookup <- c(
  depth_m    = "Depth_m",
  datetime   = "DateTimeET",
  temp_c     = "Temp (C)",
  o2_sat     = "ODO (%sat)",
  do         = "ODO (mg/L)",
  spc        = "SpCond (uS/cm)",
  turbid_fnu = "Turbidity (NTU)",
  bga        = "BGA-PC (ug/L)",
  chla       = "Chlorophyll (ug/L)",
  orp_mv     = "ORP (mV)"
)

# Negative readings here are sensor noise and get floored at 0.
# ORP is deliberately NOT listed: negative ORP is real (anoxic bottom water).
floor_at_zero <- c("depth_m", "o2_sat", "do", "spc", "turbid_fnu", "bga", "chla")

param_labels <- c(
  temp_c     = "Temperature (\u00b0C)",
  o2_sat     = "O2 saturation (%)",
  do         = "Dissolved oxygen (mg/L)",
  spc        = "Specific conductance (\u00b5S/cm)",
  pH         = "pH",
  turbid_fnu = "Turbidity",
  bga        = "BGA-PC (\u00b5g/L)",
  chla       = "Chlorophyll (\u00b5g/L)",
  orp_mv     = "ORP (mV)"
)


# ---- 1. clean ----------------------------------------------------------------
clean_profiles <- function(raw_dat) {
  needed <- c("Parameter", "Result", "Depth_m", "DateTimeET", "station")
  absent <- setdiff(needed, names(raw_dat))
  if (length(absent)) {
    stop("raw_dat is missing column(s): ", paste(absent, collapse = ", "))
  }

  raw_dat |>
    pivot_wider(
      names_from  = Parameter,
      values_from = Result,
      id_cols     = c(Depth_m, DateTimeET, station),
      values_fn   = mean            # duplicate timestamp/depth rows are averaged
    ) |>
    rename(any_of(col_lookup)) |>
    mutate(
      station = as.factor(station),
      # as_date() uses the timestamp's own time zone; base as.Date() uses UTC,
      # which pushes evening casts onto the next day
      date    = lubridate::as_date(datetime),
      year    = as.factor(lubridate::year(datetime)),
      month   = as.factor(lubridate::month(datetime)),
      across(any_of(floor_at_zero), ~ pmax(.x, 0))
    ) |>
    relocate(where(is.numeric), .after = last_col())
}


# ---- 2. grid ONE station-year onto a regular depth x date grid ---------------
# No triangulation: profile data sit on a near-regular lattice (one column per
# date, readings at fixed depth steps), which is exactly the geometry that makes
# Delaunay triangulation degenerate. Two 1-D linear interpolations are exact for
# that geometry and cannot collapse.
make_profile_grid <- function(df, param,
                              bin_m = 1, depth_step = 0.5,
                              max_gap_days = 7, min_levels_frac = 0.5) {
  empty <- tibble(date = as.Date(character()), depth_m = numeric(), value = numeric())

  # (a) one value per date x depth bin (averages repeat casts and jittery depths)
  prof <- df |>
    filter(!is.na(depth_m), !is.na(.data[[param]])) |>
    mutate(depth_grp = round(depth_m / bin_m) * bin_m) |>
    group_by(date, depth_grp) |>
    summarise(value = mean(.data[[param]]), .groups = "drop")
  if (nrow(prof) == 0) return(empty)

  # (b) QC: drop casts that reached far fewer depth levels than usual. Their few
  #     points would otherwise be stretched into a full-depth stripe.
  n_lev <- prof |> count(date, name = "n_lev")
  good  <- n_lev$date[n_lev$n_lev >= min_levels_frac * median(n_lev$n_lev)]
  prof  <- filter(prof, date %in% good)
  if (n_distinct(prof$date) < 2) return(empty)

  # (c) vertical: linear interpolation within each cast onto a fine depth grid.
  #     Nothing is extrapolated beyond the depths a cast actually reached (NA).
  z_grid <- seq(min(prof$depth_grp), max(prof$depth_grp), by = depth_step)

  by_cast <- prof |>
    group_by(date) |>
    filter(n() >= 2) |>
    group_modify(~ tibble(
      depth_m = z_grid,
      value   = approx(.x$depth_grp, .x$value, xout = z_grid, rule = 1)$y
    )) |>
    ungroup()

  # (d) horizontal: linear interpolation in time at each depth onto a daily grid,
  #     but only inside gaps between sampled dates no wider than max_gap_days.
  obs      <- sort(unique(as.numeric(by_cast$date)))
  day_grid <- seq(min(obs), max(obs), by = 1)
  idx      <- findInterval(day_grid, obs)              # last sampled day <= this day
  prev_obs <- obs[idx]
  next_obs <- obs[pmin(idx + 1L, length(obs))]
  gap_days <- ifelse(day_grid %in% obs, 0, next_obs - prev_obs)
  day_grid <- day_grid[gap_days <= max_gap_days]       # wider gaps stay blank

  by_cast |>
    filter(!is.na(value)) |>
    group_by(depth_m) |>
    filter(n() >= 2) |>
    group_modify(~ tibble(
      date  = as.Date(day_grid, origin = "1970-01-01"),
      value = approx(as.numeric(.x$date), .x$value, xout = day_grid, rule = 1)$y
    )) |>
    ungroup() |>
    filter(!is.na(value)) |>
    select(date, depth_m, value)
}


# ---- 3. plot one parameter x station (years as stacked panels) ---------------
plot_profile_heatmap <- function(clean_dat, param, station_id,
                                 bin_m = 1, depth_step = 0.5,
                                 max_gap_days = 7, min_levels_frac = 0.5,
                                 clip_q = c(0.01, 0.99),
                                 show_rug = TRUE, show_points = FALSE) {
  dat <- clean_dat |>
    filter(station == station_id, !is.na(.data[[param]]))

  if (nrow(dat) == 0) {
    message("skip ", param, " / ", station_id, ": no data")
    return(NULL)
  }

  # grid each year on its own so nothing is drawn across the off-season
  grid_dat <- dat |>
    group_by(year) |>
    group_modify(~ make_profile_grid(
      .x, param,
      bin_m = bin_m, depth_step = depth_step,
      max_gap_days = max_gap_days, min_levels_frac = min_levels_frac
    )) |>
    ungroup()

  if (nrow(grid_dat) == 0) {
    message("skip ", param, " / ", station_id, ": too few usable casts per year")
    return(NULL)
  }

  # one color scale shared by all year panels; clipped so spikes don't wash it out
  lims <- if (is.null(clip_q)) NULL else unname(quantile(grid_dat$value, clip_q, na.rm = TRUE))
  lab  <- if (param %in% names(param_labels)) param_labels[[param]] else param

  p <- ggplot(grid_dat, aes(date, depth_m)) +
    geom_tile(aes(fill = value)) +
    facet_wrap(~ year, ncol = 1, scales = "free_x") +
    scale_y_reverse() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_fill_viridis_c(name = lab, limits = lims, oob = scales::squish) +
    labs(
      x = NULL, y = "Depth (m)",
      title = paste0("Depth profile heatmap: ", lab, " - ", station_id)
    ) +
    theme_minimal()

  if (show_rug) {
    # ticks along the bottom edge: which dates were actually sampled
    p <- p + geom_rug(
      data = distinct(dat, year, date), aes(x = date),
      inherit.aes = FALSE, sides = "b", colour = "grey30", alpha = 0.5
    )
  }
  if (show_points) {
    p <- p + geom_point(data = dat, size = 0.4, colour = "white", alpha = 0.4)
  }
  p
}


# ---- 4. save ------------------------------------------------------------------
save_heatmap <- function(p, name, out_dir, width = 8, height_per_year = 2.4, dpi = 150) {
  n_years <- n_distinct(p$data$year)
  ggsave(
    filename = file.path(out_dir, paste0(name, ".png")),
    plot     = p,
    width    = width,
    height   = 1.6 + height_per_year * n_years,
    dpi      = dpi
  )
}


# ---- 5. run everything ---------------------------------------------------------
make_all_heatmaps <- function(raw_dat,
                              params   = default_params,
                              stations = NULL,
                              out_dir  = "outputs/buoy_heatmaps",
                              save_png = TRUE,
                              ...) {
  clean_dat <- clean_profiles(raw_dat)

  absent <- setdiff(params, names(clean_dat))
  if (length(absent)) {
    warning("Parameters not found in data, skipped: ", paste(absent, collapse = ", "))
    params <- intersect(params, names(clean_dat))
  }
  if (is.null(stations)) stations <- levels(droplevels(clean_dat$station))

  combos <- expand_grid(param = params, station_id = stations)
  nm <- gsub("[^A-Za-z0-9_.-]+", "_", paste(combos$param, combos$station_id, sep = "_"))

  
  plots <- map2(
    combos$param, combos$station_id,
    \(p, s) plot_profile_heatmap(clean_dat, p, s, ...)
  ) |>
    set_names(nm) |>
    compact()          # drop skipped (NULL) combos

  if (save_png && length(plots)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    iwalk(plots, \(p, n) save_heatmap(p, n, out_dir))
  }

  invisible(plots)
}
