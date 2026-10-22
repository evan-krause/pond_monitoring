#


# Last modified 3/23/2026 by SS

#   R-script to generate descriptive statistics for Quabbin Res for 2025
#
################################################################################
# Load packages and read in data 
################################################################################
#
# Clean workspace
#rm(list=ls())

# Set the working directory 

##### Open and run the script titled "AR_NonDetects.R" <- this will load functions. 

# Create a string of the packages you will need; Load to library
pkg <- c("dplyr", "tidyverse", "lubridate", "ggplot2", "lemon", "openxlsx", "rstatix",
         "magrittr", "psych", "patchwork", "NADA")
sapply(pkg, install.packages, character.only = TRUE)  #  Install, if needed.
sapply(pkg, library, character.only = TRUE)

#    Read in Data - from csv files
# for the ysi data download only 2021 and after to gets stats only from EXO and not Eureka probe (DO was very high)
#data<- read.csv('Quabbin_tblMWRAResults_2018-2023.csv',header=TRUE)           # lab
#ysi <-  read.csv('Quabbin_tblResFieldParameters_2020-2023.csv', header = TRUE)  # field
#    Read in Data - from RDS files
#ysi <- readRDS("C:/Users/Shasten.Sherwell/OneDrive - Commonwealth of Massachusetts/R/data/exo.rds")
#data <- readRDS("C:/Users/Shasten.Sherwell/OneDrive - Commonwealth of Massachusetts/R/data/mwra.rds")
#Use "Import_data_Quabbin" script to upload data directly form the database - changed in 04/26/25
################################################################################
# Some pre-processing of data
################################################################################
#

##### Date Formatting  #####
# This section adds variables for different variations of date (helpful filters)
# We also select only the columns that we want here. 

data <- data %>%
  mutate(Date = as.Date(data$DateTimeET, format = "%m/%d/%Y"),
         Year = year(Date),
         Month = month(Date),
         Season = if_else(Month %in% c(3, 4, 5), 'Spring',
                  if_else(Month %in% c(6, 7, 8), 'Summer',
                  if_else(Month %in% c(9, 10, 11), 'Fall', 'Winter'))),
         JDN = yday(Date)) %>%
  select(Location, Parameter, Units, FinalResult, ResultReported, Method, DetectionLimit,
         Date, Year, Month, Season, JDN, UniqueID)

# clean data to remove Ammonia measurements from 10/04/2023, TKN results from 05/07/2024, and 202S TKN results from 07/09/2024
data <- data[!( 
  (data$Parameter == "Ammonia-N" & data$Date == "2023-10-04") |
    (data$Parameter == "Total Kjeldahl Nitrogen" & data$Date == "2024-05-07") |
    (data$Parameter == "Total Kjeldahl Nitrogen" & data$Location == "202S" & data$Date == "2024-07-09")
), ]

#Filter YSI data to only use from 2021 to current year (change in sonde)
ysi <- ysi %>%
  filter(DateTimeET >= as.POSIXct("2021-01-01 00:00:00") & 
           DateTimeET <= as.POSIXct("2025-12-31 23:59:59"))

# Repeat for YSI data
ysi <- ysi %>%
  mutate(Date = as.Date(ysi$DateTimeET, format = "%m/%d/%Y"),
         Year = year(Date),
         Month = month(Date),
         Season = if_else(Month %in% c(3, 4, 5), 'Spring',
                  if_else(Month %in% c(6, 7, 8), 'Summer',
                  if_else(Month %in% c(9, 10, 11), 'Fall', 'Winter'))),
         JDN = yday(Date),
         Location = Station, 
         Method = 'Gibs et al., 2012',
         DetectionLimit = 'NA') %>%
  select(Location, Parameter, Depth_m, Units, FinalResult, Method, DetectionLimit,
         Date, Year, Month, Season, JDN)

##### Subsetting, rename parameters #####
# This section of code subsets data for just the sites that you want to examine
# It also removes any data from 2024 
# (helpful if trying to run stats for previous year and database export covers part of current year)
# This portion of the script also drops parameters that we are less interested in
# Next, we rename some existing parameters so they're easier to call out later.
# Lastly, we drop TP data via Marine Chemistry from 2020 (diff methods, see 2020 WQR)

all_sites <- c('202S','206S', 'DENS',
               '202M','206M', 'DENM',
               '202D','206D', 'DEND') #  list of sites to include

data <- data  %>%
  filter(Location %in% all_sites) %>%
  filter(Year <= 2025) %>%
  filter(!Parameter %in% c('Discharge', 'Nitrite', 'Nitrite-N', 'Nitrate/Nitrite',
                           'Hardness', 'Staff Gauge Height', 'Water Temperature', 'pH', 
                           'Chlorophyll Extracted', 'Total Suspended Solids',
                           'Fecal Coliform', 'Depth', 'Chlorophyll A',  'Initial pH')) %>%
  mutate(Parameter = case_when(Parameter == "Nitrate-N" ~ "Nitrate",
                               Parameter == "Ammonia-N" ~ "Ammonia",
                               Parameter == "Mean UV254" ~ "UV254", 
                               Parameter == "Turbidity NTU" ~ "Turbidity", 
                               Parameter == "Dissolved Silica" ~ "Silica",
                               Parameter == "Total Nitrogen - Measured" ~ "Total Nitrogen",
                               Parameter == "Water Temperature" ~ "Temperature",
                               TRUE ~ as.character(Parameter))) %>%
  filter(!((Method == 'Marine Chemistry 10,1981' & Parameter == 'Total Phosphorus')))



# Convert Station to Location and create a new variable for depth
data <- data  %>%
        mutate(Depth = str_remove_all(Location, "202"),
               Depth = str_remove_all(Depth, "206"),
               Depth = str_remove_all(Depth, "DEN")) %>%
        mutate(Depth = case_when(Depth == "S" ~ "Surface",
                                 Depth == "M" ~ "Mid",
                                 Depth == "D" ~ "Deep", 
                                 TRUE ~ as.character(Depth))) %>%
        mutate(Location = case_when(Location %in% c("202S", "202M", "202D") ~ "202",
                                    Location %in% c("206S", "206M", "206D") ~ "206",
                                    Location %in% c("DENS", "DENM", "DEND") ~ "Den Hill", 
                                    TRUE ~ as.character(Parameter)),
         ID = str_c(Location,'.', Season, '-', Depth),) %>%
        select(ID, Location, Depth, Parameter, Units, 
               ResultReported, FinalResult, Method, DetectionLimit,
               Date, Year, Month, Season, JDN, UniqueID)


# Drop unneeded variables from field data, add IsCensored (see next section),
  # Adjust depth column

sites <- c('202','206', 'DEN') #  list of sites to include

ysi <- ysi  %>%
       filter(Location %in% sites) %>%
       filter(!Parameter == 'Turbidity FNU') %>%
       filter(Year <= 2025 & Depth_m >= 0.24 & FinalResult >= 0) %>%
       filter(!Parameter == 'Oxygen Saturation') %>%
       mutate(Parameter = case_when(Parameter == "Water Temperature" ~ "Temperature",
                               TRUE ~ as.character(Parameter)),
               IsCensored = as.logical('FALSE')) %>%
       mutate(Depth_m = if_else(Depth_m >= 0.8, round(Depth_m, 0), 0.5)) 


##### Handling Censored Data #####

# First, create a column for IsCensored and reorder columns
data <- data %>%
        mutate(ResultReported = ifelse(is.na(ResultReported), "", ResultReported),
               Method = ifelse(is.na(Method), "", Method),
               FinalResult = ifelse(is.na(FinalResult), "", FinalResult)) %>%
        group_by(Parameter) %>% 
        mutate(IsCensored = as.logical(ifelse(str_starts(ResultReported, "<"), 'TRUE',
                                       ifelse(FinalResult <= 0.0049 & 
                                              Parameter == 'Nitrate', 'TRUE',
                                       ifelse(FinalResult <= 0.0049 & 
                                              Parameter == 'Ammonia', 'TRUE', 
                                       ifelse(FinalResult <= 0.0049 & 
                                              Parameter == 'Total Phosphorus', 'TRUE',
                                       ifelse(FinalResult <= 0.29 &
                                              Parameter %in% c('Total Organic Carbon','Dissolved Organic Carbon'), 'TRUE',
                                       ifelse(FinalResult <= 0.099 &
                                              Parameter == 'Total Kjeldahl Nitrogen', 'TRUE', 
                                       ifelse(FinalResult == 0, 'TRUE', 
                                       ifelse(FinalResult <=0.049 &
                                              Parameter == 'Alkalinity', 'TRUE', 
                                              'FALSE')))))))))) %>%
        select(ID, Location, Depth, Date, Parameter, Units,
               FinalResult, IsCensored, Method, Year, Month, Season, JDN, UniqueID)


# Next, replace all censored data with the detection limit for that parameter
# Censored data after 2010 are stored in the FinalResult column as the DL
# Censored data before 2010 are stored as half the DL or 0 (NH3N, NO3N, TP, Si, E. Coli) 

# This section of code replaces the censored data with the detection limit, 
# taking into account each of these specific scenarios for individual parameters.

data <- data %>%
  mutate(FinalResult = ifelse(IsCensored == 'TRUE' & Parameter == 'Nitrate',
                              0.005,
                       ifelse(IsCensored == 'TRUE' & Parameter ==  'Ammonia',
                              0.005,
                       ifelse(IsCensored == 'TRUE' & Parameter == 'Total Phosphorus',
                              0.005,
                       ifelse(IsCensored == 'TRUE' & Parameter == 'Total Kjeldahl Nitrogen',
                              0.10,
                       ifelse(IsCensored == 'TRUE' & Parameter == 'Silica',
                              200,
                       ifelse(IsCensored == 'TRUE' & Parameter == 'E. coli',
                              10, 
                       ifelse(IsCensored == 'TRUE' & Parameter == 'Alkalinity',
                              0.05, 
                       ifelse(IsCensored == 'TRUE' & Parameter == 'Calcium',
                              20,
                              as.numeric(FinalResult))))))))))

# End of DL subbing and censored data flagging

#
################################################################################
# Calculate TKN for post 2020 data and convert Ca and Na to mg/L 
################################################################################
#
# Separate into analytes, for ease of descriptive stats calcs and plotting
# Address any issues with multiple DLs by normalizing to the greatest DL (e.g., 5 for E. coli)

# Calculate TKN and TN for each sample

# First, pull out just the data you want to work with
N_calcs  <- data %>%
            filter(Parameter %in% c("Total Kjeldahl Nitrogen","Nitrate")) %>%
            mutate(Parameter = case_when(Parameter == "Total Kjeldahl Nitrogen" ~ "TN",
                                         Parameter == "Nitrate" ~ "NO3")) %>%
            na.omit() %>% distinct()


# Just copy the TN data and date and unique ID (will use for sorting later)
TN_Uniques <- data %>%
              filter(Parameter %in% "Total Kjeldahl Nitrogen") %>%
              mutate(ID = str_replace(UniqueID, "_TNM.*", '_TKN')) %>%
              select(ID, Location, Depth, Date, Parameter) %>%
              na.omit() %>% distinct()


# Next, take the average of duplicate measurements 
N_calcs  <- N_calcs %>%
            group_by(ID, Location, Depth, Date, Parameter, Units, IsCensored, Year, Month, Season, JDN) %>%
            summarise(FinalResult = mean(FinalResult, na.rm = TRUE)) %>%
            na.omit() %>% distinct()


# Lastly, create a new variable that represents calculated TN
N_calcs <- N_calcs %>%
  pivot_wider(id_cols = c(Location, Depth, Date), 
              values_from= FinalResult, names_from = Parameter) %>%
  mutate(FinalResult = as.numeric(TN - NO3 - 0.005),
         Parameter = "Total Kjeldahl Nitrogen",
         IsCensored = as.logical("FALSE"),
         Units = "mg/L",
         Year = year(Date),
         Month = month(Date),
         Season = if_else(Month %in% c(3, 4, 5), 'Spring',
                  if_else(Month %in% c(6, 7, 8), 'Summer',
                  if_else(Month %in% c(9, 10, 11), 'Fall', 'Winter'))),
         JDN = yday(Date),
         Method = "Calculated") %>% 
  filter(!is.na(FinalResult)) %>%
  select(!c(NO3,TN)) %>%
  select(Location, Depth, Date, Parameter, Units, 
         FinalResult, IsCensored, Method,
         Year, Month, Season, JDN)

# Bind the UniqueID field back to the TN calculated data
N_calcs <- merge(N_calcs, TN_Uniques, by=c("Location", "Depth", "Date"), all.x = TRUE) 

# First, pull out just the data you want to work with (Na and Ca)
NaCa  <- data %>%
         filter(Parameter %in% c("Sodium","Calcium", "Silica")) %>%
         mutate(FinalResult = FinalResult/1000,
                Units = "mg/L")

# Drop Na and Ca from main dataframe 
data <- data %>%
        filter(!Parameter %in% c("Sodium","Calcium", "Silica"))

#    Rename variables in N_calcs so they merge nicely
N_calcs <- N_calcs %>%
           rename("Parameter" = "Parameter.x") %>%
           select(!Parameter.y)

# Insert the TKN calculated and Na and Ca converted data into the main database
data <- rbind(data, N_calcs, NaCa)

# Remove duplicates, take average of repeat measurements
data <- data %>% 
        distinct() %>%
        filter(!Parameter == 'Total Nitrogen') %>%
        group_by(ID, Location, Depth, Date, Parameter, Units, IsCensored, Year, Month, Season, JDN) %>%
        summarise(FinalResult = mean(FinalResult)) 
       

#
################################################################################
# Calculate descriptive stats for each parameter for each site
################################################################################  
#

# Add a column to differentiate period of records stats from 2024 stats
# Call the historical data "POR" for period of record; call the 2025 data "2025"
  
data <- data %>%
        mutate(Period = case_when(Year == "2025" ~ "2025",
                                  TRUE ~ "POR"))
# Repeat for YSI data
ysi <- ysi %>%
       mutate(Period = case_when(Year == "2025" ~ "2025",
                                 TRUE ~ "POR"))

# Calculate statistics for each site/season/parameter for the different period
  # Perform statistics on laboratory data and field data separately (due to complications with NADA package functions)

Stats_ysi <- ysi %>%
             group_by(Location, Parameter,Season,Period) %>%
             summarise("n" = n(),
                   "Min" = round(min(FinalResult, na.rm = T),3),
                   "Med" = round(median(FinalResult, na.rm = T),3),
                   "Avg" = round(mean(FinalResult, na.rm = T),3),
                   "Max" = round(max(FinalResult, na.rm = T),3)) 

Stats_lab <- data %>%
             group_by(Location, Parameter,Depth,Period) %>%
             summarise("n" = n(),
                       "Min" = round(min(FinalResult, na.rm = T),3),
                       "Med" = round(CEN_ROS(results = as.numeric(FinalResult), nd_col = IsCensored, stat ="median"),3),
                       "Avg" = round(CEN_ROS(results = FinalResult, nd_col = IsCensored, stat ="mean"),3),
                       "Max" = round(max(FinalResult, na.rm = T),3)) 


  # Pivot the table so that it's in the report format
  Stats_lab<-pivot_wider(Stats_lab, 
                          id_cols = c(Location, Depth, Parameter),
                          names_from = Period,
                          values_from = c(n, Min, Med, Avg, Max))
  
  Stats_ysi<-pivot_wider(Stats_ysi, 
                         id_cols = c(Location, Parameter, Season),
                         names_from = Period,
                         values_from = c(n, Min, Med, Avg, Max))

  # Rearrange the order of the results so they mirror the report format

  Stats_lab <- Stats_lab %>%
                arrange(Parameter, Location, .by_group = FALSE)
  
  Stats_ysi <- Stats_ysi %>%
                arrange(Parameter, Location, Season, .by_group = FALSE) %>%
                select(Location:Season, n_2025, n_POR, Min_2025, Min_POR,
                       Med_2025, Med_POR, Avg_2025, Avg_POR, Max_2025, Max_POR)

  # Add parentheses around historical data 
  #Stats_lab$n_POR <- paste0("(", Stats_lab$n_POR, ")")
  #Stats_lab$Min_POR <- paste0("(", Stats_lab$Min_POR, ")")
  #Stats_lab$Med_POR <- paste0("(", Stats_lab$Med_POR, ")")
  #Stats_lab$Avg_POR <- paste0("(", Stats_lab$Avg_POR, ")")
  #Stats_lab$Max_POR <- paste0("(", Stats_lab$Max_POR, ")")
  
  
  #Stats_ysi$n_POR <- paste0("(", Stats_ysi$n_POR, ")")
  #Stats_ysi$Min_POR <- paste0("(", Stats_ysi$Min_POR, ")")
  #Stats_ysi$Med_POR <- paste0("(", Stats_ysi$Med_POR, ")")
  #Stats_ysi$Avg_POR <- paste0("(", Stats_ysi$Avg_POR, ")")
  #Stats_ysi$Max_POR <- paste0("(", Stats_ysi$Max_POR, ")")
  
  
# Write stats into workbook ###################################################

# Create a list using the two data tables 
list<- list(Stats_lab,
            Stats_ysi)

# Create a list of names (for Excel file tabs)
names<- list("Stats_Res_2025","Stats_Profiles_2025")

# Write stats to workbook   
  write.xlsx(list, 
             file = "Stats_Res_2025_PROVISIONALDATA_2.xlsx", 
             sheetName = names, append = FALSE)
  
#
################################################################################
#

# END

  
    