library("lubridate", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library(ggplot2)
library(data.table)
library(dbplyr)
library(tidyverse)

download.file("https://cran.r-project.org/src/contrib/magick_2.2.tar.gz")
devtools::install_github("ropensci/magick")

install.packages("ggmap")
library("ggmap", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
april = na.omit(april)
dim(april)

april <- read_csv("Desktop/Capstone Data/Uber-Raw-April2014.csv")

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)

AIzaSyD4ljQVChZ97nFVssMNrRDeV3VqEY8WbbE


ggmap::register_google(key='AIzaSyD4ljQVChZ97nFVssMNrRDeV3VqEY8WbbE')

thing = ggmap(get_googlemap(center = c(lon = mean(april$Lon), lat = mean(april$Lat)), 
                            zoom = 11, scale = 2, maptype = 'roadmap', color = 'color' ))

#thing + geom_point(aes(x = Lon, y = Lat), data = april, size = 0.5) 
sqrt(100)
thing + stat_density2d(aes(x = Lon, y = Lat, alpha = 0.01), 
                       size = 0.005, bins = round(sqrt(nrow(april))), data = april, geom = 'polygon') #



mymap = ggplot(april, aes(x = Lon, y = Lat)) + coord_map() +
  geom_polygon(color = 'grey') +
  transition_states(april$`Date/Time`,
                    transition_length = 7,
                    state_length = 7,
                    wrap = TRUE)
## July now?

thing = ggmap(get_googlemap(center = c(lon = mean(july$Lon), lat = mean(july$Lat)), 
                            zoom = 11, scale = 2, maptype = 'roadmap', color = 'color' ))

thing + stat_density2d(aes(x = Lon, y = Lat, alpha = 0.01), 
                       size = 0.005, bins = round(sqrt(nrow(july))), data = july, geom = 'polygon') #

mymap = ggplot(july, aes(x = Lon, y = Lat)) + coord_map() +
  geom_polygon(color = 'grey') +
  transition_states(july$`Date/Time`,
                    transition_length = 7,
                    state_length = 7,
                    wrap = TRUE)
