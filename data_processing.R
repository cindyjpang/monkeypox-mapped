library(plyr)
library(readr)
library(dplyr)
library(plotly)
library(tidyr)
library(sf)

gh_data <- read.csv("https://raw.githubusercontent.com/globaldothealth/monkeypox/main/latest.csv")

countries_sf <- read_sf("C:\\Users\\Cindy Pang\\monkeypox-mapped\\countries sf\\World_Countries__Generalized_.shp")

country_confirmed_case_dat <- gh_data %>%
  filter(Status == 'confirmed')%>%
  dplyr::count(Status, Date_confirmation, Country)%>%
  spread(Status, n)



  
  