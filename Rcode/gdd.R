#Summarize GDD for grid cells

#libraries
library(tidyverse)
library(lubridate)
library(readxl)

#load raw data of monthly gdd (average daily GDD by month, year, grid cell)
gdd.data<-read_excel("data/regional_gdd.xlsx", sheet="Avg_Monthly_GDD")

#calculate total GDD from January - June)
gdd<-gather(gdd.data, year, gdd, '2014':'2018') %>%
  mutate(year=as.numeric(year), lat_bin=(Lat1+Lat2)/2, lon_bin=(Lon1+Lon2)/2, 
         month=match(Month, month.abb), monthgdd=gdd*days_in_month(month)) %>%
  group_by(lat_bin, lon_bin, year) %>%
  arrange(month) %>%
  mutate(sumgdd=cumsum(monthgdd), log.gdd=log(sumgdd)) %>%
  filter(month==6)

rm(gdd.data)
