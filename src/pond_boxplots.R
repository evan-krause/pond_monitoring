#
#
#
#

pkg <- c("tidyverse")

installed_packages <- pkg %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkg[!installed_packages])
}

library(tidyverse)

## Data ----

## Boxplot functions ----

plot_param_boxplot <- function(param, data = pond_dat, min_n = 5) {
  df <- data |>
    filter(!is.na(.data[[param]])) |>
    group_by(station, year) |>
    filter(n() >= min_n) |>
    ungroup()
  
  if (nrow(df) == 0)
    return(NULL)
  
  ggplot(df, aes(x = station, y = .data[[param]], fill = year)) +
    geom_boxplot(position = position_dodge2(preserve = "single")) +
    scale_fill_viridis_d(name = "Year") +
    labs(
      x = "Station",
      y = param,
      title = paste("Distribution of", param, "by site and year")
    ) +
    theme_minimal()
}


## Generate and save for each parameter ----

params <- c(
  "temp_c",
  "o2_sat",
  "do",
  "spc",
  "pH",
  "turbid_fnu",
  "bga_rfu",
  "bga",
  "chla_rfu",
  "chla"
)
stations <- levels(pond_dat$station)

boxplot_plots <- purrr::map(params, plot_param_boxplot) |>
  purrr::set_names(paste0(params, "_", stations, sep = "_")) |>
  purrr::compact() #drop any parameter with no plot-worthy data

## Create directory and save each plot to disk
boxplot_out_dir <- "outputs/boxplots"
if (!dir.exists(boxplot_out_dir)) {
  dir.create(boxplot_out_dir, recursive = TRUE)
}

walk2(
  boxplot_plots,
  names(boxplot_plots),
  ~ ggsave(
    filename = file.path(boxplot_out_dir, paste0(.y, ".png")),
    plot = .x,
    width = 7,
    height = 5,
    dpi = 150
  )
)

ggsave(
  filename = file.path(boxplot_out_dir, "spc_lp2_lp3_presentation.png"),
  plot = spc_presentation_plot,
  width = 7,
  height = 5,
  dpi = 150
)

