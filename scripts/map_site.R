#map sites

library(here)
library(tidyverse)
library(sp)
library(mapview)
library(htmlwidgets)
library(webshot)

#load data
gps<-read.csv(here("data/ai_scmu_sites.csv"))%>%
  separate(Site.Latitude.Longitude,
           into=c("site","ex1","lat","ex2","long"),
           sep=c(-23,-21,-12,-11))%>%
  select(-ex1,-ex2)%>%
  select(long,lat,site)%>%
  mutate(long=as.numeric(long),
         lat=as.numeric(lat))
map<-gps

#define coordinates
coordinates(map) <- map[, c('long', 'lat')]

#assign crs project
proj4string(map) <- CRS('+proj=longlat +datum=WGS84')

#plot
cntr_crds <- c(mean(coordinates(map)[, 1]),
               mean(coordinates(map)[, 2]))
gps@map %>% setView(cntr_crds[1], cntr_crds[2], zoom = 2)

mapviewOptions(fgb = FALSE) #have to run this to use mapshot, idk why
(m1<-mapview(map, map.types = c("OpenStreetMap.DE"),
        col.regions="#08308f", alpha=0.5,zoom=2))

#save map
mapshot(m1, url=paste0(getwd(),here("/figures/site.html")))
mapshot(m1, file=here("figures/site.png"),vwidth = 900, vheight = 600)
