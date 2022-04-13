library(tidyverse)
library(dataRetrieval)
library(lubridate)

discharge <- readNWISdv(siteNumbers = "09361500", parameterCd = "00060", startDate = "", endDate = "") %>%
  filter(Date >= "1986-08-06") %>%
  rename("Discharge" = "X_00060_00003") %>%
  select(c(Date, Discharge))
ggplot(discharge, aes(x = Date, y = X_00060_00003)) +
  geom_line() + 
  labs(x = "Year", y = "Discharge (cfs)")

snotel <- read.csv("./data/raw/snotel_data.csv")

snotel$Date <- mdy(snotel$Date)

snotel_wrangled <- snotel %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  mutate(max = max(SWE..in.)) %>%
  filter(max == SWE..in.) %>%
  group_by(Y)

discharge_wrangled <- discharge %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  mutate(Max = max(Discharge)) %>%
  filter(Max == Discharge) %>%
  group_by(Year) %>%
  filter(Date == max(Date))
  #summarise(Max = max(X_00060_00003),
   #         X = Date)

df <- merge(discharge_wrangled, snotel_wrangled, by = "Year") %>%
  rename(c("max_discharge_date" = "Date.x", "max_snowpack_date" = "Date.y")) %>%
  mutate(lag = as.numeric(difftime(max_discharge_date, max_snowpack_date, units = "days")))

ggplot(df, aes(x = Year, y = lag)) +
  geom_line()


ggplot() +
  geom_line(data = snotel, aes(x = Date, y = SWE..in.)) +
  geom_line(data = discharge, aes(x = Date, y = X_00060_00003))


# make sure to choose a USGS gage that does not have withdrawals occurring between snowpack gage and USGS discharge gage
# filter for last date with max
