#Holding ponds graphics
#Author: Evan Krause
#created: August 2026

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
    filter(month %in% c(6,7,8)) |>
    ggplot(aes(temp_c, color = year)) +
    geom_boxplot() +
    facet_grid(~month) +
    coord_flip()
    
  














rm(pkg)