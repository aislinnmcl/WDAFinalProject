library(tidyverse)
library(dataRetrieval)
library(lubridate)
library(cowplot)
library(sf)
library(mapview)
library(Kendall)

gages <- read.csv("./data/raw/gages.csv", header = TRUE)

discharge <- readNWISdv(siteNumbers = "09361500", parameterCd = "00060", startDate = "", endDate = "")

snotel <- read.csv("./data/raw/snotel_data.csv")

snotel$Date <- mdy(snotel$Date)

discharge_wrangled <- discharge %>%
  filter(Date >= "1987-01-01" & Date <= "2021-12-31") %>% 
  rename("Discharge" = "X_00060_00003") %>%
  select(c(Date, Discharge))

snotel_wrangled <- snotel %>%
  filter(Date >= "1987-01-01" & Date <= "2021-12-31") %>%
  mutate(Year = year(Date)) %>%
  select(Date, SWE..in., Year) %>%
  rename("SWE" = "SWE..in.")

snotel_max <- snotel_wrangled %>%
  group_by(Year) %>%
  mutate(max = max(SWE)) %>%
  filter(max == SWE) %>%
  group_by(Year) %>%
  filter(Date == max(Date)) # chose latest date in year

discharge_max <- discharge_wrangled %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  mutate(Max = max(Discharge)) %>%
  filter(Max == Discharge) %>%
  group_by(Year) %>%
  filter(Date == max(Date)) # chose latest date in year

df_max <- merge(discharge_max, snotel_max, by = "Year") %>%
  rename(c("max_discharge_date" = "Date.x", "max_snowpack_date" = "Date.y"), "max_discharge_cfs" = "Max", "max_swe_in" = "max") %>%
  mutate(lag = as.numeric(difftime(max_discharge_date, max_snowpack_date, units = "days")))

df_daily <- merge(discharge, snotel, by = "Date")

ggplot(df_max, aes(x = Year, y = lag)) +
  geom_line()

plot_discharge <- ggplot(discharge, aes(x = Date, y = Discharge)) +
  geom_line() + 
  labs(x = "Year", y = "Discharge (cfs)")

plot_snotel <- ggplot(snotel, aes(x = Date, y = SWE..in.)) +
  geom_line() +
  labs(x = "Year", y = "SWE (in)")

plot_grid(plot_discharge, plot_snotel, nrow = 2)

# make sure to choose a USGS gage that does not have withdrawals occurring between snowpack gage and USGS discharge gage

# map gages

gages_sf <- st_as_sf(gages, coords = c("Long", "Lat"), crs = 4326)

mapview(gages_sf, col.regions = c("red", "blue"))

mapviewOptions(basemaps = c("Stamen.TerrainBackground"))

# STATS

# is there a relationship between peak SWE and peak discharge?

SWE_discharge_model <- lm(data = df_max, max_discharge_cfs ~ max_swe_in)
summary(SWE_discharge_model)

ggplot(df_max, aes(x = max_discharge_cfs, y = max_swe_in)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)


# has lag time been trending up or down?

lag_ts <- ts(df_max[[8]], start = c(1987,1),frequency = 365)

lag_trend <- MannKendall(lag_ts)
summary(lag_trend) # p > 0.05, no monotonic trend

# correlation between max lag time and peak SWE?

SWE_lag_model <- lm(data = df_max, max_swe_in ~ lag)
summary(SWE_lag_model)
