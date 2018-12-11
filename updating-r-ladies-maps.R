# To cite please use the Twitter handle @d4tagirl, thanks!

library(rtweet)
library(dplyr)
library(lubridate)
library(ggmap)
library(tidyr)
library(purrr)
library(ggthemes)
library(maps)
library(plotly)
# I'm using this version of gganimate: https://github.com/thomasp85/gganimate/releases/tag/v0.1.1
library(gganimate)
library(tibble)
library(stringr)

# Gaby's list of all the R-Ladies chapters
users <- lists_members(slug = "rladies-chapters", owner_user = "gdequeiroz", n = 5000)

# Lookup table to replace the cities Google Maps can't match
# (if you find more, please add them!)
lookup <- tibble(screen_name = c('RLadiesLx', 'RLadiesNatal', 'RLadiesLinz',
                                 'rladiestoronto', 'RLadiesXalapa',
                                 'RLadiesRemote'),
                 location = c('Lisbon', 'Natal, Brazil', 'Linz, Austria',
                              'Toronto, Canada', 'Xalapa, Mexico',
                              'Tristan da Cunha'))

rladies <- unique(users) %>%
  filter(!screen_name %in% c('RLadiesGlobal')) %>%
  add_row(                                 # add Taipei chapter (info from Meetup)
    screen_name = 'RLadiesTaipei',
    location = 'Taipei',
    created_at = as.Date('2014-11-15'),
    followers_count = 1091) %>%
  add_row(                                 # add Warsow chapter (info from Meetup, shared with RUG, estimating half users are R-Ladies)
    screen_name = 'RLadiesWarsaw',
    location = 'Warsaw',
    created_at = as.Date('2016-11-15'),
    followers_count = 900) %>%
  mutate(age_days = difftime(today(), created_at, unit = 'days')) %>%
  left_join(lookup, by = 'screen_name') %>%
  mutate(location = ifelse(is.na(location.y), location.x, location.y)) %>%
  select(screen_name, location, created_at, followers = followers_count, age_days) %>%
  mutate(longlat = purrr::map(.$location, geocode, source = "dsk")) %>%
  unnest()

# saveRDS(rladies, "2018-12-11_rladies.rds")
# rladies <- readRDS("2018-12-11_rladies.rds")

#··················
# plotly

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map()

map <- world +
  geom_point(aes(x = lon, y = lat,
                 text = paste('city: ', ifelse(screen_name == 'RLadiesRemote',
                                               'Remote (Worldwide)',
                                               location),
                              '<br /> created : ', created_at),
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
  labs(size = 'Followers') +
  annotate("text", x = 0, y = -80,
           color = "purple",
           label = "Made with love by R-Ladies")
ggsave("static2.png", width = 12, height = 7)

#··············
# gganimate map

# init point to show empty map in the beggining
ghost_point <- rladies %>%
  add_row(
    created_at = as.Date('2011-09-01'),
    followers = 0,
    lon = 0,
    lat = 0,
    .before = 1) %>%
  slice(1) %>%
  mutate(date = format(created_at, format = '%Y-%m-%d'),
         est_followers = 0)

dates <- as_tibble(seq(floor_date(as.Date(min(rladies$created_at)),
                                  unit = "month"),
                       today(),
                       by = 'days')) %>%
  filter(day(value) %in% c(1, 10, 20))

rladies_frames <- rladies %>%
  select(screen_name) %>%
  expand(screen_name, date = dates$value) %>%
  right_join(rladies, by = 'screen_name') %>%
  filter(date > created_at) %>%
  mutate(date = format(date, format = '%Y-%m-%d'),
         age_total = as.numeric(age_days, units = 'days'),
         age_at_date = as.numeric(difftime(date, created_at, units = 'days'),
                                  units = 'days'),
         est_followers = ((followers - 1) / age_total) * age_at_date)

rladies_less_frames <- rladies_frames %>%
  filter((day(as.Date(date)) == 1 & month(as.Date(date)) %% 6 == 0) |
           as.Date(date) >= rladies$created_at[rladies$screen_name == 'RLadiesLondon'])

map_less_frames <- world +
  geom_point(aes(x = lon, y = lat,
                 size = est_followers,
                 frame = date),
             data = rladies_less_frames, colour = 'purple', alpha = .5) +
  geom_point(aes(x = lon, y = lat,
                 size = est_followers,
                 frame = date),
             data = ghost_point, alpha = 0) +
  scale_size_continuous(range = c(1, 10), breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers') +
  labs(size = 'Seguidores') +
  annotate("text", x = 00, y = -80,
           color = "purple",
           # label = "Made with <3 by R-Ladies (code: dv.uy/mapas-rladies)",
           label = "Hecho con <3 por R-Ladies (código: dv.uy/mapas-rladies)",
           size = 5) +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position = c(0.05, 0.15),
        plot.title=element_text(size=14, hjust = 0.05, vjust=-0.12))

# animation::ani.options(ani.width = 1125, ani.height = 675)
animation::ani.options(ani.width = 800, ani.height = 480)
gganimate(map_less_frames, interval = .15, "rladies_growth.gif")
