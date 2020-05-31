# load twitter library - the rtweet library is recommended now over twitteR
library(rjson)
library(jsonlite)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
library(tidyr)

# animated maps
# to install: devtools::install_github("dgrtwo/gganimate")
# note this required imagemagick to be installed
library(leaflet)
library(gganimate)
library(lubridate)
library(maps)
library(ggthemes)

options(stringsAsFactors = FALSE)

# create file path
json_file <- "data/week-13/boulder_flood_geolocated_tweets.json"
# import json file line by line to avoid syntax errors
# this takes a few seconds
boulder_flood_tweets <- stream_in(file(json_file))
# explore the data

# create new df with just the tweet texts & usernames
tweet_data <- data.frame(date_time = boulder_flood_tweets$created_at,
                         username = boulder_flood_tweets$user$screen_name,
                         tweet_text = boulder_flood_tweets$text,
                         coords = boulder_flood_tweets$coordinates)

# flood start date sept 13 - 24 (end of incident)
start_date <- as.POSIXct('2013-09-13 00:00:00')
end_date <- as.POSIXct('2013-09-24 00:00:00')

# cleanup & and filter to just the time period around the flood
flood_tweets <- tweet_data %>%
  mutate(coords.coordinates = gsub("\\)|c\\(", "", coords.coordinates),
         date_time = as.POSIXct(date_time, format = "%a %b %d %H:%M:%S +0000 %Y")) %>%
  separate(coords.coordinates, c("long", "lat"), sep = ", ") %>%
  mutate_at(c("lat", "long"), as.numeric) %>%
  filter(date_time >= start_date & date_time <= end_date )
# create basemap of the globe
world_basemap <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80")

world_basemap

# create basemap of the globe
world_basemap <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map()

world_basemap


head(flood_tweets)
##             date_time       username
## 1 2013-09-23 20:27:25  CenturyLinkCO
## 2 2013-09-23 18:08:24 WxTrackerDaryl
## 3 2013-09-23 16:34:05    L_Parquette
## 4 2013-09-23 17:38:58   SausalitoSpy
## 5 2013-09-23 19:03:57      lulugrimm
## 6 2013-09-23 21:49:51   DenelleJarro
##                                                                                                                                tweet_text
## 1                              Download: “CenturyLink Community Flood Impact Report: September 23” --&gt; http://t.co/jhn3xSxs6r #coflood
## 2                                       There's a moose on the loose on Laurel Street @KDVR #MooseOnTheLoose #cowx http://t.co/zsrHeaUoHw
## 3 #Fall really knows how to make an entrance @KeystoneMtn. #Snow #cowx #wintercountdown. Opening Day is November 1 http://t.co/VmeRBBlTr7
## 4                From Boulder, Colorado: Notes on a Thousand-Year Flood - #BoulderFlood Jenny Shank - The Atlantic http://t.co/Ln8mmCOQuk
## 5                               I'd rather be working from here:) #boulder #colorado #latergram #mountains #monday http://t.co/t8cGzQDyVx
## 6              Teaching last minute #Hot #Yoga 6PM tonight yogapodcommunity #Boulder! Join me for this 26 posture… http://t.co/Q4E7e1Qe0U
##   coords.type   long   lat
## 1        <NA>     NA    NA
## 2       Point -104.8 39.59
## 3        <NA>     NA    NA
## 4        <NA>     NA    NA
## 5        <NA>     NA    NA
## 6       Point -105.3 40.02
# remove na values
tweet_locations <- flood_tweets %>%
  na.omit()

head(tweet_locations)
##              date_time       username
## 2  2013-09-23 18:08:24 WxTrackerDaryl
## 6  2013-09-23 21:49:51   DenelleJarro
## 12 2013-09-23 22:25:55  markpmeredith
## 28 2013-09-23 21:25:00   JapangoSushi
## 34 2013-09-23 16:51:29      Gr8_Beard
## 36 2013-09-23 21:26:28     willdayart
##                                
##    coords.type    long   lat
## 2        Point -104.83 39.59
## 6        Point -105.26 40.02
## 12       Point -105.02 39.74
## 28       Point -105.28 40.02
## 34       Point  -84.18 39.74
## 36       Point -105.27 40.05

world_basemap +
  geom_point(data = tweet_locations, aes(x = long, y = lat),
             colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8),
                        breaks = c(250, 500, 750, 1000)) +
  labs(title = "Tweet Locations During the Boulder Flood Event")
