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

#filter for Lava1
lava1_det<-raven_cam%>%
  subset(Site=="Lava1")

#merge time and date + filter for 00 and 50 minutes
lava1_det_0<-lava1_det%>%
  subset(lava1_det$min=="0000")%>%
  select(-min1,-time1,-min)%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")
lava1_det_5<-lava1_det%>%
  subset(lava1_det$min=="5000")%>%
  select(-min1,-time1,-min)%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")
lava1_det_n<-lava1_det%>%
  subset(lava1_det$min!="5000" & lava1_det$min!="0000")%>%
  select(-min1,-time1,-min)%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")

#prepare wav files for lift off
df<-unique(lava1_det_0$datetime_mp3)
lava1_det_0<-as.data.table(df,TRUE)
lava1_det_num<-as.numeric(lava1_det_0$df)
lava1_det_0<-as.data.table(lava1_det_num,TRUE)

#find +-10 minutes of each wav file
lava1_det_0p<-lava1_det_0[,1]+1000
lava1_det_0m<-lava1_det_0[,1]-5000
lava1_det_0<-as.character(lava1_det_0$lava1_det_num)
lava1_det_0<-as.data.table(lava1_det_0,TRUE)
lava1_det_0p<-as.character(lava1_det_0p$lava1_det_num)
lava1_det_0p<-as.data.table(lava1_det_0p,TRUE)
lava1_det_0m<-as.character(lava1_det_0m$lava1_det_num)
lava1_det_0m<-as.data.table(lava1_det_0m,TRUE)

#prepare wav files for lift off
df<-unique(lava1_det_5$datetime_mp3)
lava1_det_5<-as.data.table(df,TRUE)
lava1_det_num<-as.numeric(lava1_det_5$df)
lava1_det_5<-as.data.table(lava1_det_num,TRUE)

#find +-10 minutes of each wav file
lava1_det_5p<-lava1_det_5[,1]+5000
lava1_det_5m<-lava1_det_5[,1]-1000
lava1_det_5<-as.character(lava1_det_5$lava1_det_num)
lava1_det_5<-as.data.table(lava1_det_5,TRUE)
lava1_det_5p<-as.character(lava1_det_5p$lava1_det_num)
lava1_det_5p<-as.data.table(lava1_det_5p,TRUE)
lava1_det_5m<-as.character(lava1_det_5m$lava1_det_num)
lava1_det_5m<-as.data.table(lava1_det_5m,TRUE)

#prepare wav files for lift off
df<-unique(lava1_det_n$datetime_mp3)
lava1_det_n<-as.data.table(df,TRUE)
lava1_det_num<-as.numeric(lava1_det_n$df)
lava1_det_n<-as.data.table(lava1_det_num,TRUE)

#find +-10 minutes of each wav file
lava1_det_np<-lava1_det_n[,1]+1000
lava1_det_nm<-lava1_det_n[,1]-1000
lava1_det_n<-as.character(lava1_det_n$lava1_det_num)
lava1_det_n<-as.data.table(lava1_det_n,TRUE)
lava1_det_np<-as.character(lava1_det_np$lava1_det_num)
lava1_det_np<-as.data.table(lava1_det_np,TRUE)
lava1_det_nm<-as.character(lava1_det_nm$lava1_det_num)
lava1_det_nm<-as.data.table(lava1_det_nm,TRUE)

#combine all data tables
colnames(lava1_det_0)<-'lava1'
colnames(lava1_det_0p)<-'lava1'
colnames(lava1_det_0m)<-'lava1'
colnames(lava1_det_5)<-'lava1'
colnames(lava1_det_5p)<-'lava1'
colnames(lava1_det_5m)<-'lava1'
colnames(lava1_det_n)<-'lava1'
colnames(lava1_det_np)<-'lava1'
colnames(lava1_det_nm)<-'lava1'
lava1_det_0all<-rbindlist(list(lava1_det_0,lava1_det_0p,
                              lava1_det_0m))
lava1_det_5all<-rbindlist(list(lava1_det_5,lava1_det_5p,
                               lava1_det_5m))                          
lava1_det_nall<-rbindlist(list(lava1_det_n,lava1_det_np,
                                lava1_det_nm))
lava1_det_all<-rbindlist(list(lava1_det_0all,lava1_det_5all,
                               lava1_det_nall))
lava1_det_all<-unique(lava1_det_all$lava1)
lava1_det_all<-as.data.table(lava1_det_all,TRUE)
colnames(lava1_det_all)<-'Begin.File'

#make it look like wav file format
lava1_det_all$Begin.File <- sub("^", "S4A04765_", lava1_det_all$Begin.File)
lava1_det_all$Begin.File <- paste0(lava1_det_all$Begin.File, ".wav")
stri_sub(lava1_det_all$Begin.File, 18, 17) <- "_"
