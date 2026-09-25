#author: evan Krause
#last updated: 2026-09-25


# function for transforming watershed pond data into wide format for ease of 
# analysis. This function accepts .xlsx files downloaded from MS Access DB 
# in the schema of table, "Quabbin_tblPondFieldParameters' with column headers
# matching the 'names' vector below.

# names <- c(
#   "ID",
#   "Station",
#   "DateTimeET",
#   "Parameter",
#   "Depth_m",
#   "FinalResult",
#   "Units",
#   "Probe_Type",
#   "UniqueID",
#   "DataSource",
#   "DataSourceID",
#   "ImportDate"
# )

pkg <- c("readxl", "tidyverse")

installed_packages <- pkg %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkg[!installed_packages])
}
 sapply(pkg, library, character.only= TRUE)
 map(pkg, library)
## functions----
pond_clean <- function(x) {
  raw_dat <- if (file.exists(paste0("data/", {{x}})) == TRUE) {
    readxl::read_excel(paste0("data/", {{x}}))
  } else {
    path <- file.choose()
    raw_dat <- readxl::read_excel(path)
  }
  
  clean_dat <- raw_dat |> tidyr::pivot_wider(
    #pivot data into wide format, creating col names from Parameter
    names_from = Parameter,
    values_from = FinalResult,
    id_cols = c(Depth_m, DateTimeET, Station)
  ) |>
    dplyr::mutate(
      date = as.Date(DateTimeET),
      station = as.factor(Station),
      year = as.factor(lubridate::year(DateTimeET)),
      month = as.factor(lubridate::month(DateTimeET)),
      dplyr::across(dplyr::where(is.numeric), ~ dplyr::if_else(. < 0, 0, .))
    ) |> #factor and date coercion
    dplyr::rename(
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
    dplyr::relocate(where(is.numeric), .after = last_col()) |> # move identifier cols to front
    dplyr::select(-Station)  #remove old station col
  data <- clean_dat
}

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
# function call----
pond_dat <- pond_clean("pond_data.xlsx")

## post-processing----
pond_dat$station <- forcats::fct_collapse(
  pond_dat$station,
  #condense and lower site names
  lp1 = c("lp1", "LP1"),
  lp2 = c("lp2", "LP2"),
  lp3 = c("lp3", "LP3")
)

#filter long pond sites
lpm_dat <- pond_dat |>
  dplyr::filter(station %in% c("lp2", "lp3"),
                depth_m > 0.04)

lpm_dat$station <- droplevels(lp_dat$station)

#filter holding ponds sites
hpm_dat <- pond_dat |>
  dplyr::filter(station %in% c("301", "302"),
                depth_m > 0.4)

hpm_dat$station <- droplevels(hpm_dat$station)



#save rds for analysis/viz----
saveRDS(hpm_dat, "data/hpm_cleaned.rds")
saveRDS(lpm_dat, "data/lp_cleaned.rds")
  