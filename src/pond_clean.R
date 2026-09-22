


# fucntion for transforming watershed pond data for ease of analysis

pond_clean <- function(x) {
  raw_dat <- readxl::read_excel(paste0("data/", x)) # from DB xlsx
  clean_dat <- raw_dat |> tidyr::pivot_wider(
    #pivot data into wide format, creating col names from Parameter
    names_from = Parameter,
    values_from = FinalResult,
    id_cols = c(Depth_m, DateTimeET, Station)
  ) |>
    dplyr::mutate(date = as.Date(DateTimeET), station = as.factor(Station)) |> #factor and date coercion
    dplyr::rename( #rename cols to more usable formats
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
  data<- clean_dat
}

d_clean <- function(x) {
  raw_dat <- readxl::read_excel(paste0("data/", x))}
