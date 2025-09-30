#Author: Evan Krause
#date:

# Package installation----
pkg <- c(
  "tidyverse",
  "zoo",
  "DBI",
  "duckdb"
)

installed_packages <- pkg %in% rownames(installed.packages()) #check if necessary packages are installed
if (any(installed_packages == FALSE)) {
  install.packages(pkg[!installed_packages]) #install if not in installed_packages list
}
invisible(sapply(pkg, library, character.only = TRUE)) #call library across pkg list



# csv cleaning function----
clean_csv <- function(file) {
  raw <- readr::read_csv(here::here("data", file))
  
  raw <- raw |> dplyr::mutate(dplyr::across(tidyselect::where(is.character), as.factor),
                              date = lubridate::mdy(Date)) |>
    dplyr::relocate(date, .before = tidyselect::everything()) |>
    dplyr::select(-Date) |>
    dplyr::rename(
      time = 2,
      site = 3,
      unit = 4,
      user = 5,
      temp = 6,
      do_pct = 7,
      do_mgl = 8,
      spc_us = 9,
      ph = 10,
      fnu = 11,
      pcya_rfu = 12,
      pcya_ugl = 13,
      chl_rfu = 14,
      chl_ugl = 15,
      vpos_m = 16
    ) |>
    relocate(vpos_m, .after = user)
  
}

# Stat summary function----
pond_sum <- function(data, group.by) {
  data |>
    group_by({{group.by}}) |>
    summarize(
      max_depth = max(4),
      ph_range = round(max(9) - min(9), 2),
      avg_temp = mean(5),
      avg_do_pct = mean(7),
      avg_chl_ugl = mean(14),
      n = n()
    ) |>
    arrange(desc(n))
}
