######################################################################################################################################################################
#
#   LOAD LIBRARIES
#
######################################################################################################################################################################


library(tidyverse)
library(dataRetrieval)
library(lubridate)
library(cowplot)
library(sf)
library(mapview)
library(Kendall)

######################################################################################################################################################################
#
#   IMPORT DATA
#
######################################################################################################################################################################


gages <- read.csv("./data/raw/gages.csv", header = TRUE)

discharge <- readNWISdv(siteNumbers = "09361500", parameterCd = "00060", startDate = "", endDate = "")
write.csv(discharge, "./data/raw/discharge_data.csv")

snotel <- read.csv("./data/raw/snotel_data.csv")

######################################################################################################################################################################
#
#   WRANGLE DATA
#
######################################################################################################################################################################

# convert to date class 
snotel$Date <- mdy(snotel$Date)

# wrangle discharge data
discharge_wrangled <- discharge %>%
  filter(Date >= "1987-01-01" & Date <= "2021-12-31") %>% # filter for complete years
  rename("Discharge" = "X_00060_00003") %>% # rename cols
  select(c(Date, Discharge)) # select relevant cols

write.csv(discharge_wrangled, "./data/processed/discharge_wrangled.csv")

snotel_wrangled <- snotel %>%
  filter(Date >= "1987-01-01" & Date <= "2021-12-31") %>% # filter for complete years
  mutate(Year = year(Date)) %>% # add year col
  select(Date, SWE..in., Year) %>% # select relevant cols
  rename("SWE" = "SWE..in.") # rename cols

write.csv(snotel_wrangled, "./data/processed/snotel_wrangled.csv")