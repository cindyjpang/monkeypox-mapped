library(plyr)
library(readr)
library(dplyr)
library(plotly)
library(tidyr)
library(sf)
library(stats)
library(zoo)
gh_data <- read.csv("https://raw.githubusercontent.com/globaldothealth/monkeypox/main/latest.csv")

countries_sf <- read_sf("C:\\Users\\Cindy Pang\\monkeypox-mapped\\countries sf\\World_Countries__Generalized_.shp")
world_pop <- read.csv("world_pop_2022.csv")%>%
  select(name, pop2022)
#world_pop$pop2022 <- as.numeric(world_pop$pop2022)
total_world_pop <- sum(world_pop$pop2022)

world_pop <- rbind(world_pop, c("World", as.numeric(total_world_pop)))
world_pop$pop2022 <- as.numeric(world_pop$pop2022)*1000
class(world_pop$pop2022)
##great_britain_sf <- read_sf("C:\\Users\\Cindy Pang\\monkeypox-mapped\\countries sf\\CTRY_DEC_2021_UK_BFC.shp")


country_confirmed_case_dat <- gh_data %>%
  filter(Status == 'confirmed')%>%
  dplyr::count(Status, Date_confirmation, Country)%>%
  spread(Status, n)%>%
  mutate(cumulative_confirmed = ave(confirmed, Country, FUN=cumsum))%>%
  arrange(Country, .by_group = TRUE)

# aggregate all the UK regions into one country
uk_confirmed_case_dat <- country_confirmed_case_dat %>%
  filter(Country == "England" | Country == "Wales" | Country == "Northern Ireland"| Country == "Scotland" | Country == "United Kingdom")%>%
  group_by(Date_confirmation)%>%
  summarize(United_Kingdom = sum(confirmed))%>%
  gather(key = "Country", value = "confirmed", United_Kingdom)%>%
  mutate(cumulative_confirmed = ave(confirmed, Country, FUN=cumsum))

# aggregate all of the world data
world_confirmed_case_dat <- country_confirmed_case_dat %>%
  group_by(Date_confirmation)%>%
  summarize(World = sum(confirmed))%>%
  gather(key = "Country", value = "confirmed", World)%>%
  mutate(cumulative_confirmed = ave(confirmed, Country, FUN=cumsum))

countries_all <- rbind(country_confirmed_case_dat, uk_confirmed_case_dat, world_confirmed_case_dat)%>%
  subset(Country != "England" & Country != "Wales" & Country != "Northern Ireland" & Country != "Scotland" & Country != "United Kingdom")%>%
  group_by(Country)%>%
  mutate(case_03d = rollmean(confirmed, k = 3, fill = NA, align = 'center'),
         Country = ifelse(Country == 'United_Kingdom', "United Kingdom", Country))%>%
  merge(world_pop, by.x = "Country", by.y = "name", all = FALSE)%>%
  mutate(daily_per_1M = (confirmed/pop2022)*1000000,
         cumulative_per_1M = (cumulative_confirmed/pop2022)*1000000)
  
  



# view by country plot

country_cumulative <- plot_ly(countries_all, x = ~Date_confirmation, y = ~cumulative_confirmed, type = 'scatter',mode = "lines", color = ~Country)
country_cumulative 

country_daily <- plot_ly(countries_all, x = ~Date_confirmation, y = ~confirmed, type = 'bar',mode = "lines", color = ~Country)%>%
  add_trace(y = ~case_03d, type = 'scatter', mode = "lines")
country_daily

country_cum_case_rate<- plot_ly(countries_all, x = ~Date_confirmation, y = ~cumulative_per_1M, type = 'scatter',mode = "lines", color = ~Country)
country_cum_case_rate

# no point in really looking at a daily case rate cuz it looks nasty LOL
country_daily_case_rate<- plot_ly(countries_all, x = ~Date_confirmation, y = ~daily_per_1M, type = 'scatter',mode = "lines", color = ~Country)
country_daily_case_rate

# export all countries case files 
write.csv(countries_all, "C:\\Users\\Cindy Pang\\monkeypox-mapped\\exported data\\mpx_country_case_dat")