library(plyr)
library(readr)
library(dplyr)
library(plotly)
library(tidyr)

gh_data <- read.csv("https://raw.githubusercontent.com/globaldothealth/monkeypox/main/latest.csv")

country_dat <- gh_data %>%
  filter(Status == 'confirmed')%>%
  dplyr::count(Status, Date_confirmation, Country)%>%
  spread(Status, n)


  
  