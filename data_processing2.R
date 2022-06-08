library(plyr)
library(readr)
library(dplyr)
library(plotly)
library(tidyr)
library(sf)
library(stats)
library(zoo)
library(httr)
gh_data <- read.csv(text=content(GET("https://raw.githubusercontent.com/globaldothealth/monkeypox/main/latest.csv")))


world_pop <- read.csv("world_pop_2022.csv")%>%
  select(name, pop2022)
total_world_pop <- sum(world_pop$pop2022)

world_pop <- rbind(world_pop, c("World", as.numeric(total_world_pop)))
world_pop$pop2022 <- as.numeric(world_pop$pop2022)*1000
class(world_pop$pop2022)


country_confirmed_case_dat <- gh_data %>%
  dplyr::count(Status, Date_confirmation, Country)
  # spread(Status, n)%>%
  # mutate(cumulative_confirmed = ave(confirmed, Country, FUN=cumsum))%>%
  # arrange(Country, .by_group = TRUE)

hospitalizations <- gh_data %>%
  filter(Status == "confirmed" | Status == "suspected")%>%
  select(Status, Country, Date_confirmation, Hospitalised..Y.N.NA., Date_hospitalisation, Date_entry, Date_last_modified)%>%
  mutate(Date_hospitalisation = ifelse(Date_hospitalisation == '', Date_entry, Date_hospitalisation))%>%
  dplyr::count(Hospitalised..Y.N.NA., Date_hospitalisation, Country)%>%
  filter(Hospitalised..Y.N.NA. == "Y")