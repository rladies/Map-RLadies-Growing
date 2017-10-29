# To cite please use the Twitter handle @d4tagirl, thanks!

library(rtweet)
library(dplyr)
library(lubridate)
library(stringr)
library(ggmap)
library(purrr)
library(tidyr)
library(tibble)
library(maps)
library(ggthemes)
library(plotly)
library(gganimate)

# me basé en la lista de gaby r-ladies-chapters (list_id = 783772667635564544)

# no está implementada aún la función list_members... así que lo hice con Python y me traje los users... :(

# ids <- c(897536218862411776,889225256031989761,887805154224766976,887716460667305985,886942615467425792,885519461301850113,881698125668331522,880743180483907585,879825421352583168,878671836590092290,874844585582895106,870664135943630853,869620600834318337,869287159035039744,868935258154127360,860988343516934144,852344970891710465,849793688901476356,844276053807976448,838620675971645440,833544882203418624,823623126563090436,822851102915891200,822124248177188865,820690440617951232,817133222484176896,809427007587155968,806895230863568896,804027517506093057,803525989364023296,801792750106578944,797899718630248452,791287437880360960,788647266059943936,785949670858121217,784888486813892608,784615577235128322,783366686833606656,783014080395276288,781271660766851072,777930207508434945,773118223512403968,772948414501969920,772203134269652992,771530276488908800,771203524872908800,770490229769789440,770377177296543745,747584164245864448,722588617231769601,3139240146,881530736)

screen_names <- c('RLadiesSarasota',
'RLadiesQuito',
'RLadiesPDX',
'RLadiesMNE',
'RLadiesBelgrade',
'RLadiesChicago',
'RLadiesSantiago',
'RladiesMilan',
'RLadiesCDMX',
'RLadiesAME',
'RLadiesSanDiego',
'RLadiesMVD',
'RLadiesMiami',
'RLadiesOrlando',
'RLadiesBuff',
'RLadiesSeattle',
'RLadiesMTL',
'RLadiesSR',
'RLadiesMunich',
'RLadiesCapeTown',
'RLadiesAdelaide',
'RLadiesBudapest',
'RLadiesDublin',
'RLadiesLdnOnt',
'RLadiesRio',
'RLadiesBA',
'RLadiesAustin',
'RLadiesDC',
'RLadiesAmes',
'RLadiesTbilisi',
'RLadiesCT',
'RLadiesValencia',
'RLadiesLx',
'RLadiesIzmir',
'RLadiesBCN',
'RLadiesManchest',
'RLadiesLima',
'RLadiesColumbus',
'RLadiesBerlin',
'RLadiesNash',
'RLadiesParis',
'RLadiesIstanbul',
'RLadiesBoston',
'RLadiesMAD',
'RLadiesAU',
'RLadiesNYC',
'RLadiesGlobal',
'RLadiesLA',
'RLadiesRTP',
'RLadiesLondon',
'RLadiesTC',
'RLadiesSF')

users <- lookup_users(users = screen_names)

# users <- search_users(q = 'RLadies',
#                       n = 10000,
#                       parse = TRUE)

# Lookup table to replace the cities Google Maps can't match
lookup <- tibble(screen_name = c('RLadiesLx','RLadiesMTL' ,'RLadiesSeattle', 'RLadiesMunich'),
                 location = c('Lisbon', 'Montreal', 'Seattle', 'Munich'))

rladies <- unique(users) %>%
  filter(str_detect(screen_name, '^(RLadies).*') &
           !screen_name %in% c('RLadies', 'RLadies_LF', 'RLadiesGlobal')) %>%
  add_row(                                 # add Taipei chapter (info from Meetup)
    screen_name = 'RLadiesTaipei',
    location = 'Taipei',
    account_created_at = as.Date('2014-11-15'),
    followers_count = 347) %>%
  add_row(                                 # add Warsow chapter (info from Meetup)
    screen_name = 'RLadiesWarsaw',
    location = 'Warsaw',
    account_created_at = as.Date('2016-11-15'),
    followers_count = 80) %>%
  mutate(age_days = difftime(today(), account_created_at, unit = 'days')) %>%
  left_join(lookup, by = 'screen_name') %>%
  mutate(location = ifelse(is.na(location.y), location.x, location.y)) %>%
  select(screen_name, location, account_created_at, followers = followers_count, age_days) %>%
  mutate(longlat = purrr::map(.$location, geocode)) %>%
  unnest()

#··················
# plotly

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map()

map <- world +
  geom_point(aes(x = lon, y = lat,
                 text = paste('city: ', location,
                              '<br /> created : ', account_created_at),
                 size = followers),
             data = rladies, colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 9), breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers')

ggplotly(map, tooltip = c('text', 'size'))

#··············
# static map 

map <- world +
  geom_point(aes(x = lon, y = lat,
                 size = followers),    # add the size aes for later gganimate
             data = rladies, 
             colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8), 
                        breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers')

#··············
# gganimate map 

# init point to show empty map in the beggining
ghost_point <- rladies %>%
  add_row(
    account_created_at = as.Date('2011-09-01'),
    followers = 0,
    lon = 0,
    lat = 0,
    .before = 1) %>%
  slice(1) %>% 
  mutate(date = format(account_created_at, format = '%Y-%m-%d'),
         est_followers = 0)

dates <- as_tibble(seq(floor_date(as.Date(min(rladies$account_created_at)), 
                                  unit = "month"),
                       today(),
                       by = 'days')) %>%
  filter(day(value) %in% c(1, 10, 20))

rladies_frames <- rladies %>%
  select(screen_name) %>%
  expand(screen_name, date = dates$value) %>%
  right_join(rladies, by = 'screen_name') %>%
  filter(date > account_created_at) %>%
  mutate(date = format(date, format = '%Y-%m-%d'),
         age_total = as.numeric(age_days, units = 'days'),
         age_at_date = as.numeric(difftime(date, account_created_at, units = 'days'),
                                  units = 'days'),
         est_followers = ((followers - 1) / age_total) * age_at_date)

rladies_less_frames <- rladies_frames %>%
  filter((day(as.Date(date)) == 1 & month(as.Date(date)) %% 6 == 0) |
           as.Date(date) >= rladies$account_created_at[rladies$screen_name == 'RLadiesLondon'])

map_less_frames <- world +
  geom_point(aes(x = lon, y = lat,
                 size = est_followers,
                 frame = date),
             data = rladies_less_frames, colour = 'purple', alpha = .5) +
  geom_point(aes(x = lon, y = lat,
                 size = est_followers,
                 frame = date),
             data = ghost_point, alpha = 0) +
  scale_size_continuous(range = c(1, 13), breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers')

# animation::ani.options(ani.width = 1125, ani.height = 675)
animation::ani.options(ani.width = 800, ani.height = 480)
gganimate(map_less_frames, interval = .15, "rladies_growth.gif")
