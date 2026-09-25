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
    dplyr::filter(!is.na(.data[[param]])) |>
    dplyr::group_by(station, year) |>
    dplyr::filter(dplyr::n() >= min_n) |>
    dplyr::ungroup()
  
  if (nrow(df) == 0)
    return(NULL)
  
  ggplot2::ggplot(df, ggplot2::aes(x = station, y = .data[[param]], fill = year)) +
    ggplot2::geom_boxplot(position = ggplot2::position_dodge2(preserve = "single")) +
    ggplot2::scale_fill_viridis_d(name = "Year") +
    ggplot2::labs(
      x = "Station",
      y = param,
      title = paste("Distribution of", param, "by site and year")
    ) +
    ggplot2::theme_minimal()
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

means_params <- names(lp_means[,4:7])

stations <- levels(hpm_dat$station)

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


