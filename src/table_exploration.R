library(GGally)
library(tidyverse)
library(DataExplorer)
library(summarytools)

lp_dat |>
ggpairs(columns = c(5,7,8,9), aes(color = station))

DataExplorer::create_report(lp_dat[,3:length(lp_dat)])

dfSummary(lp_dat[,c(3,5,7,8,9,14)])|> summarytools::stview()
