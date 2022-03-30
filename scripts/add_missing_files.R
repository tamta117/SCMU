#add in missing failure files to cam
library(here)
library(dplyr)
library(data.table)
library(tidyverse)
library(lubridate)
library(stringi)

#import cam csv
lava1_cam<-read.csv(here("data/camera/lava1_cam.csv"))
lava2_cam<-read.csv(here("data/camera/lava2_cam.csv"))
moss_cam<-read.csv(here("data/camera/moss_cam.csv"))
pinnacle_cam<-read.csv(here("data/camera/pinnacle_cam.csv"))%>%
  mutate(Murrelet=ifelse(Murrelet=="true",TRUE,FALSE))
refuge_cam<-read.csv(here("data/camera/refuge_cam.csv"))%>%
  mutate(Murrelet=ifelse(Murrelet=="true",TRUE,FALSE))

#join all cam files
all_cam<-rbindlist(list(lava1_cam, lava2_cam, moss_cam,
                        pinnacle_cam, refuge_cam))
#filter for ravens
raven_cam<-all_cam%>%
  subset(all_cam$Species=="Raven")

#change time format
raven_cam$Time<-gsub(":","",as.character(raven_cam$Time))
raven_cam$min1<-str_pad(raven_cam$Time, width=6, side="left", pad="0")
raven_cam<-raven_cam%>%
  separate(`min1`,
         into=c("time1","min","time2"),
         sep=c(2,3), remove=FALSE)%>%
  select(-time2)
raven_cam$min<-str_pad(raven_cam$min, width=4, side="right", pad="0")
raven_cam<-raven_cam%>%
  unite("time_mp3",time1:min,remove=FALSE, sep="")

#change date format
raven_cam$date_mp3<-parse_date_time(raven_cam$Date, orders = c('ymd', 'mdy'),tz="")
raven_cam$date_mp3<-gsub("-","",as.character(raven_cam$date_mp3))

#merge time and date
raven_cam<-raven_cam%>%
  select(-time1,-min,-min1)%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")

#filter for each site
lava1_det<-raven_cam%>%
  subset(Site=="Lava1")

#prepare wav files for lift off
df<-unique(lava1_det$datetime_mp3)
lava1_det_mp3<-as.data.table(df,TRUE)
lava1_det_num<-as.numeric(lava1_det_mp3$df)
lava1_det_mp3<-as.data.table(lava1_det_num,TRUE)

#find +-10 minutes of each wav file
lava1_det_plus<-lava1_det_mp3[,1]+1000
lava1_det_minus<-lava1_det_mp3[,1]-1000
lava1_det_mp3<-as.character(lava1_det_mp3$lava1_det_num)
lava1_det_mp3<-as.data.table(lava1_det_mp3,TRUE)
lava1_det_plus<-as.character(lava1_det_plus$lava1_det_num)
lava1_det_plus<-as.data.table(lava1_det_plus,TRUE)
lava1_det_minus<-as.character(lava1_det_minus$lava1_det_num)
lava1_det_minus<-as.data.table(lava1_det_minus,TRUE)

#combine all 3 data tables
colnames(lava1_det_mp3)<-'lava1'
colnames(lava1_det_plus)<-'lava1'
colnames(lava1_det_minus)<-'lava1'
lava1_det_all<-rbindlist(list(lava1_det_mp3,lava1_det_plus,
                              lava1_det_minus))
lava1_det_all<-unique(lava1_det_all$lava1)
lava1_det_all<-as.data.table(lava1_det_all,TRUE)
colnames(lava1_det_all)<-'lava1'

#make it look like wav file format
lava1_det_all$lava1 <- sub("^", "S4A04765_", lava1_det_all$lava1)
lava1_det_all$lava1 <- paste0(lava1_det_all$lava1, ".wav")
stri_sub(lava1_det_all$lava1, 18, 17) <- "_"
