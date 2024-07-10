#ANALYZING THE MALARIA DATA SET
install.packages('tidyverse')
library('tidyverse')
install.packages('Scale')
install.packages('maps')
library(maps)
# importing data
malaria_inc <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-11-13/malaria_inc.csv")

View(malaria_inc)

#chaing the names of the titles
malaria_inc_processed <- malaria_inc %>%
  set_names(c('Countries', 'code', 'year', 'incidence')) %>%
  mutate(incidence = incidence/1000)

View(malaria_inc_processed)

# looking at 2015 levels and the chane for, 2000 - 2015
malaria_inc_processed %>% 
  filter(Countries %in% sample(unique(Countries), 6)) %>%
  ggplot(aes(year, incidence, color = Countries)) + geom_line() + 
  scale_y_continuous(labels = scales::percent_format())

malaria_spread <- malaria_inc_processed %>%
  mutate(year = paste0("Y", year)) %>%
  spread(year, incidence)

View(malaria_spread)

malaria_spread %>%
  filter(Countries != 'Turkey',
         !is.na(code)) %>%
  mutate(current = Y2015,
         change = Y2015 - Y2000) %>%
  ggplot(aes(current, change)) + 
  geom_point() + 
  geom_text(aes(label = code), vjust = 1, hjust = 2) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format())


malaria_ven <- malaria_inc_processed %>% 
  filter(Countries == 'Venezuela') 
View(malaria_ven)


ggplot(malaria_inc_processed, aes(x = year, y = incidence)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  scale_y_continuous(labels = scales::percent_format())

world <- map_data('world') %>% 
  filter(region != "Antarctica")

View(world)

maps::iso3166 %>%
  select(a3, mapname)

world

# we are going to merge malaria with maps using the country code

malaria_inc_processed %>%
  filter(incidence < 1) %>%
  inner_join(maps::iso3166 %>%
               select(a3, mapname), by = c(code = 'a3')) %>%
  inner_join(world, by = c(mapname = 'region')) %>%
  ggplot(aes(long, lat, group = group, fill = incidence)) +
  geom_polygon() +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0.20, labels = scales::percent_format()) +
  facet_wrap('~year') +
  theme_void() +
  labs(title = "malaria incidence over time around the world")
  
  
table <- malaria_inc_processed %>%
  filter(incidence < 1) %>%
  inner_join(maps::iso3166 %>%
               select(a3, mapname), by = c(code = 'a3')) %>%
  inner_join(world, by = c(mapname = 'region'))
View(table)

#malaria death cases
malaria_death_cases <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-11-13/malaria_deaths.csv')
View(maps::iso3166)
View(malaria_death_cases)

malaria_deaths_processed <- malaria_death_cases %>%
  set_names(c('Countries', 'code', 'year', 'deaths'))
 
View(malaria_deaths_processed)
View(world)

malaria_deaths_processed %>%
  filter(Countries %in% sample(unique(Countries),6)) %>%
  ggplot(aes(year, deaths, color = Countries)) +
  geom_line()+
  labs(y = "deaths per 100 000") +
  scale_y_continuous(labels = scale::percent_format())

#malaria deaths over time

install.packages('fuzzyjoin')
library(fuzzyjoin)
install.packages('stringr')
library(stringr)

malaria_country_data <- malaria_deaths_processed %>%
  inner_join(maps::iso3166 %>%
               select(a3, mapname), by= c(code = 'a3')) %>%
  mutate(mapname = str_remove(mapname, "\\(.*"))
View(maps::iso3166)

malaria_map_data <- map_data("world") %>%
  filter(region != "Antartica") %>%
  tbl_df() %>%
  inner_join(malaria_country_data, by = c(region = "mapname"))

malaria_map_data %>%
  ggplot(aes(long, lat, group = group, fill = deaths)) +
  geom_polygon()+
  scale_fill_gradient2(low = 'blue', high = 'red', midpoint = 100)+
  theme_void()+
  labs(title = "Malaria deaths over time around the world",
       fill = "deaths per 100 000")
install.packages("countrycode")
library(countrycode)
install.packages("gganimate")
library(gganimate)

animation <- malaria_map_data %>%
  mutate(continent = countrycode(code, "iso3c", "continent")) %>%
filter(continent ==  "Africa") %>%
  ggplot(aes(long, lat, group = group, fill = deaths)) +
  geom_polygon()+
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 100) +
  theme_void()+
  transition_manual(year) +
  labs(title = "Malaria deatahs over time  in africa({current frame})",
       fill = "deaths per 100 000")

animate(animation, nframes = 300, fps = 10, renderer = gifski_renderer("test.gif"))
  

