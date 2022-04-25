######################################################################################################################################################################
#
#   IMPORT DATA AND LIBRARIES
#
######################################################################################################################################################################

# load packages

library(tidyverse)
library(dataRetrieval)
library(lubridate)
library(cowplot)
library(sf)
library(mapview)
library(Kendall)

# set theme

mytheme <-
  theme_gray(base_size = 12) +
  theme(legend.background = element_rect(fill = "gray"), legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14, color = "black", hjust = 0.5),
        plot.subtitle = element_text(size = 10, color = "black", hjust = 0.5))

theme_set(mytheme)

# import data

discharge_wrangled <- read.csv("./data/processed/discharge_wrangled.csv") %>%
  select(Date, Discharge)

discharge_wrangled$Date <- ymd(discharge_wrangled$Date)

snotel_wrangled <- read.csv("./data/processed/snotel_wrangled.csv") %>%
  select(Date, SWE, Year)

snotel_wrangled$Date <- ymd(snotel_wrangled$Date)

gages <- read.csv("./data/raw/gages.csv", header = TRUE)

######################################################################################################################################################################
#
#   CREATE PEAK DATAFRAME
#
######################################################################################################################################################################

snotel_max <- snotel_wrangled %>%
  group_by(Year) %>%
  mutate(max = max(SWE)) %>% # calculate annual max
  filter(max == SWE) %>% # find date where max occurs
  group_by(Year) %>%
  filter(Date == max(Date)) # choose latest date in year

discharge_max <- discharge_wrangled %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  mutate(Max = max(Discharge)) %>% # calculate annual max
  filter(Max == Discharge) %>% # find date where max occurs
  group_by(Year) %>%
  filter(Date == max(Date)) # chose latest date in year

# merge swe and discharge data

df_max <- merge(discharge_max, snotel_max, by = "Year") %>%
  rename(c("max_discharge_date" = "Date.x", "max_snowpack_date" = "Date.y"), "max_discharge_cfs" = "Max", "max_swe_in" = "max") %>%
  select(-(c(Discharge, SWE)))

df_max$max_discharge_date[df_max$max_discharge_date == "2006-10-07"] <- "2006-06-06" # replace late runoff dates
df_max$max_discharge_cfs[df_max$max_discharge_cfs == 7000] <- 2050
df_max$max_discharge_date[df_max$max_discharge_date == "2002-09-12"] <- "2002-05-21"
df_max$max_discharge_cfs[df_max$max_discharge_cfs == 947] <- 777

df_max <- df_max %>%
  mutate(lag = as.numeric(difftime(max_discharge_date, max_snowpack_date, units = "days")),
         max_discharge_date_daynum = yday(max_discharge_date))

######################################################################################################################################################################
#
#   EXPLORE
#
######################################################################################################################################################################

# chose to plot in excel for report so I could do 2 axes

plot_discharge <- ggplot(discharge_wrangled, aes(x = Date, y = Discharge)) +
  geom_line() + 
  labs(x = "Year", y = "Discharge (cfs)")
plot_discharge

plot_snotel <- ggplot(snotel_wrangled, aes(x = Date, y = SWE)) +
  geom_line() +
  labs(x = "Year", y = "SWE (in)")

plot_grid(plot_discharge, plot_snotel, nrow = 2)

# has discharge been increasing or decreasing?

discharge_ts <- ts(discharge_wrangled[[2]], frequency = 1)
discharge_model <- MannKendall(discharge_ts)
summary(discharge_model)

ggplot(data = discharge_wrangled, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(title = "Daily Discharge on the Animas River", subtitle = "1987-2021", y = "Discharge (cfs)") + 
  geom_smooth(method = lm)

# has SWE been increasing or decreasing?

snowpack_ts <- ts(snotel_wrangled[[2]], frequency = 1)
snowpack_model <- MannKendall(snowpack_ts)
summary(snowpack_model)

ggplot(data = snotel_wrangled, aes(x = Date, y = SWE)) +
  geom_line() +
  labs(title = "Daily SWE at Molas Lake", subtitle = "1987 - 2021", y = "SWE (in)") + 
  geom_smooth(method = lm)

# map gages

mapviewOptions(basemaps = c("Stamen.TerrainBackground"))

gages_sf <- st_as_sf(gages, coords = c("Long", "Lat"), crs = 4326)

mapview(gages_sf, col.regions = c("red", "blue"))

######################################################################################################################################################################
#
#   ANALYZE
#
######################################################################################################################################################################

# is there a relationship between peak SWE and peak discharge?

SWE_discharge_model <- lm(data = df_max, max_discharge_cfs ~ max_swe_in)
summary(SWE_discharge_model)

ggplot(df_max, aes(x = max_discharge_cfs, y = max_swe_in)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)


# does peak SWE determine timing of peak discharge?

SWE_discharge_timing_model <- lm(data = filter(df_max, max_discharge_date_daynum < 200), max_discharge_date_daynum ~ max_swe_in)
summary(SWE_discharge_timing_model)

ggplot(filter(df_max, max_discharge_date_daynum < 200), aes(x = max_discharge_date_daynum, y = max_swe_in)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) + 
  labs(title = "Peak SWE and Peak Discharge Timing", subtitle = "1987-2021", x = "Day of Year for Peak Discharge", y = "Peak SWE (in)")

# has lag time been trending up or down?

# has lag time been trending up or down?

lag_ts <- ts(df_max[[8]], start = c(1987,1),frequency = 365)

lag_trend <- MannKendall(lag_ts)
summary(lag_trend) # p > 0.05, no monotonic trend

ggplot(df_max, aes(x = Year, y = lag)) +
  geom_line() + 
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Peak SWE-Peak Discharge Lag Time", subtitle = "1987-2021", y = "Lag Time (# of days)")

# does SWE influence lag time?

SWE_lag_model <- lm(data = df_max, max_swe_in ~ lag)
summary(SWE_lag_model)

ggplot(df_max, aes(x = lag, y = max_swe_in)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) + 
  labs(title = "Peak SWE and Lag Time", subtitle = "1987-2021", x = "Lag Time (days)", y = "Peak SWE (in)")
