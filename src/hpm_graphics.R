###############################  HEADER  ######################################
#  TITLE: hpm_graphics.R
#  DESCRIPTION: generate visualizations from cleaned holding ponds monitoring data
#  AUTHOR(S): Evan Krause
#  DATE LAST UPDATED: 2026-10-20
##############################################################################



hpm_dat<-  readRDS("data/hpm_cleaned.rds")


#Theme----
report_theme <- theme(
  legend.position = 'top',
  legend.key = element_rect(color = 'black'),
  legend.background = element_rect(fill = 'white'),
  panel.background = element_rect(fill = "white"),
  panel.grid = element_line(colour = 'gray'),
  axis.text.x = element_text(angle = 0,
                             face = "bold")
)



#Packages----
  pkg <- c('tidyverse', 'gtsummary', 'GGally', 'gt', 'gtsummary', 'kableExtra')

  installed_packages <- pkg %in% rownames(installed.packages()) #check if necessary packages are installed
  if (any(installed_packages == FALSE)) {
  install.packages(pkg[!installed_packages])#install if not in installed_packages list
  }
  
#boxplots
## temp by month  
  hpm_dat |>
    filter(month %in% c(5,6,7,8)) |>
    ggplot2::ggplot(aes(datetime,temp_c, color = year)) +
    geom_point() +
    geom_line(aes(group = station)) +
    facet_grid(~year)
    # coord_flip()
    
  














rm(pkg)