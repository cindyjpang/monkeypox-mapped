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
  mutate(Country = ifelse(Country == "England" | Country == "Wales" | Country == "Northern Ireland"| Country == "Scotland" | Country == "United Kingdom","United Kingdom", Country ),
         Date_confirmation = ifelse(Status == "suspected", Date_entry, Date_confirmation))%>%
  filter(Status == "confirmed" | Status == "suspected")%>%
  dplyr::count(Status, Date_confirmation, Country)%>%
  spread(Status, n)%>%
  mutate(Date_confirmation = as.Date(Date_confirmation))

# aggregate all of the world data
world_confirmed_case_dat <- country_confirmed_case_dat %>%
  group_by(Date_confirmation)%>%
  summarize(confirmed = sum(confirmed, na.rm = TRUE), 
            suspected = sum(suspected, na.rm = TRUE))%>%
  mutate(Country = "World")%>%
  relocate(Country, .after = Date_confirmation)

countries_all <-  rbind(country_confirmed_case_dat, world_confirmed_case_dat) %>%
  group_by(Country)%>%
  mutate(confirmed = replace_na(confirmed, 0),
         suspected =replace_na(suspected, 0),
          cumulative_confirmed = ave(confirmed, Country, FUN = cumsum),
         cumulative_suspected = ave(suspected, Country, FUN = cumsum))

countries_all$confirmed_suspected <- rowSums(countries_all[, c("confirmed", "suspected")])
countries_all$cumulative_confirmed_suspected <- ave(countries_all$confirmed_suspected, countries_all$Country, FUN = cumsum)

countries_all <- merge(countries_all,
                       world_pop,
                       by.x = "Country",
                       by.y = "name",
                       all = FALSE)%>%
  mutate(daily_confirmed_per_1M = round((confirmed/pop2022)*1000000, digits = 2),
         daily_suspected_per_1M = round((suspected/pop2022)*1000000, digits = 2),
         daily_confirmed_suspected_per_1M = round((confirmed_suspected/pop2022)*1000000, digits = 2),
         cumulative_confirmed_per_1M = round((cumulative_confirmed/pop2022)*1000000, digits = 2),
         cumulative_suspected_per_1M = round((cumulative_suspected/pop2022)*1000000, digits = 2),
         cumulative_confirmed_suspected_per_1M = round((cumulative_confirmed_suspected/pop2022)*1000000, digits = 2),
         daily_confirmed_07d = rollmean(confirmed, k = 7, fill = NA, align = 'center'),
         daily_suspected_07d = rollmean(suspected, k = 7, fill = NA, align = "center"),
         daily_confirmed_suspected_07d = rollmean(confirmed_suspected, k = 7, fill = NA, align = 'center'))


hospitalizations <- gh_data %>%
  filter(Status == "confirmed" | Status == "suspected")%>%
  select(Status, Country, Date_confirmation, Hospitalised..Y.N.NA., Date_hospitalisation, Date_entry, Date_last_modified)%>%
  mutate(Date_hospitalisation = ifelse(Date_hospitalisation == '', Date_entry, Date_hospitalisation),
         Country = ifelse(Country == "England" | Country == "Wales" | Country == "Northern Ireland"| Country == "Scotland" | Country == "United Kingdom","United Kingdom", Country ))%>%
  dplyr::count(Hospitalised..Y.N.NA., Date_hospitalisation, Country)%>%
  filter(Hospitalised..Y.N.NA. == "Y")%>%
  mutate(cumulative_hospitalizations = ave(n, Country, FUN = cumsum))







# export all countries case files 
write.csv(countries_all, "C:\\Users\\Cindy Pang\\monkeypox-mapped\\exported data\\mpx_country_case_dat_v2.csv")
write.csv(hospitalizations, "C:\\Users\\Cindy Pang\\monkeypox-mapped\\exported data\\mpx_gbl_hospitalizations.csv")