library(readr)
library(ggplot2)
library(ggmap)
library(dplyr)

register_google(key = "AIzaSyAL4i-C_dr81Q5WTCBv6oklVH3bRXKId1A") 

search <- read_csv("/Users/johnfranchak/Documents/GitHub/lss/gps/searching_222.csv")
search$lat <- search$`33.973263`
search$long <- search$`-117.32558`
search <- search %>% filter(lat > 33.973973)

walk <- read_csv("/Users/johnfranchak/Documents/GitHub/lss/gps/walking_222.csv")
walk$lat <- walk$`33.9720035391`
walk$long <- walk$`-117.327640334`

ggmap(get_googlemap(center = c(-117.3262, max(walk$lat)-.0002),
                    color = "bw", scale = 2, zoom = 18, maptype = "roadmap",
                    style = c(feature = "all", element = "labels", visibility = "off")), darken = 0) +
  geom_path(aes(x = long, y = lat), data = walk, size = 1, color = "#D55E00") + 
  geom_path(aes(x = long, y = lat), data = search, size = 1, color =  "#0072B2")
ggsave("gps.pdf", units = "in", width = 5, height = 4)
