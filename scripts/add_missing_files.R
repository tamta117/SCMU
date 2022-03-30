#add in missing acoustic files to acoustic csv
library(here)
library(dplyr)
library(data.table)
library(tidyverse)
library(lubridate)

#import acoustic csv
lava1_acoustic<-read.csv(here("odata/lava1_acoustic.csv"))
lava2_acoustic<-read.csv(here("odata/lava2_acoustic.csv"))
moss_acoustic<-read.csv(here("odata/moss_acoustic.csv"))
pinnacle_acoustic<-read.csv(here("odata/pinnacle_acoustic.csv"))%>%
  mutate(Murrelet=ifelse(Murrelet=="true",TRUE,FALSE))
refuge_acoustic<-read.csv(here("odata/refuge_acoustic.csv"))%>%
  mutate(Murrelet=ifelse(Murrelet=="true",TRUE,FALSE))

#join all acoustic files
all_acoustic<-rbindlist(list(lava1_acoustic,lava2_acoustic,moss_acoustic,
                        pinnacle_acoustic,refuge_acoustic))
#filter for ravens
raven_acoustic<-all_acoustic%>%
  subset(all_acoustic$Species=="Raven")

#change time format
raven_acoustic$Time<-gsub(":","",as.character(raven_acoustic$Time))
raven_acoustic$min1<-str_pad(raven_acoustic$Time, width=6, side="left", pad="0")
raven_acoustic<-raven_acoustic%>%
  separate(`min1`,
         into=c("time1","min","time2"),
         sep=c(2,3), remove=FALSE)%>%
  select(-time2)
raven_acoustic$min<-str_pad(raven_acoustic$min, width=4, side="right", pad="0")
raven_acoustic<-raven_acoustic%>%
  unite("time_mp3",time1:min,remove=FALSE)
raven_acoustic$time_mp3<-gsub("_","",as.character(raven_acoustic$time_mp3))

#change date format
raven_acoustic$date_mp3<-parse_date_time(raven_acoustic$Date, orders = c('ymd', 'mdy'),tz="")
raven_acoustic$date_mp3<-gsub("-","",as.character(raven_acoustic$date_mp3))

#merge time and date
raven_acoustic<-raven_acoustic%>%
  select(-time1,-min,-min1)%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE)

#filter for each site
lava1_det<-raven_acoustic%>%
  subset(Site=="Lava1")

#make it look like wav file format
lava1_det$datetime <- sub("^", "S4A04765_", lava1_det$datetime_mp3 )
lava1_det$datetime <- paste0(lava1_det$datetime, ".wav")

#create table of all wav
df<-unique(lava1_det$datetime)
lava1_det_mp3<-as.data.table(df,TRUE)