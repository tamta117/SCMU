#find all processed wav files in lava1
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
#filter for ravmurrs
raven_cam<-all_cam%>%
  subset(all_cam$Species=="Raven")
murr_cam<-all_cam%>%
  subset(all_cam$Murrelet=="TRUE")
ravmurr_cam<-bind_rows(raven_cam,murr_cam)

#change time format
ravmurr_cam$Time<-gsub(":","",as.character(ravmurr_cam$Time))
ravmurr_cam$min1<-str_pad(ravmurr_cam$Time, width=6, side="left", pad="0")
ravmurr_cam<-ravmurr_cam%>%
  separate(`min1`,
         into=c("time1","min","time2"),
         sep=c(2,3), remove=FALSE)%>%
  select(-time2)
ravmurr_cam$min<-str_pad(ravmurr_cam$min, width=4, side="right", pad="0")
ravmurr_cam<-ravmurr_cam%>%
  unite("time_mp3",time1:min,remove=FALSE, sep="")

#change date format
ravmurr_cam$date_mp3<-parse_date_time(ravmurr_cam$Date, orders = c('ymd', 'mdy'),tz="")
ravmurr_cam$date_mp3<-gsub("-","",as.character(ravmurr_cam$date_mp3))

#filter for Lava1
lava1_det<-ravmurr_cam%>%
  subset(Site=="Lava1" & date_mp3<20210523)

#merge time and date + filter for 00 and 50 minutes
lava1_det_0<-lava1_det%>%
  subset(lava1_det$min=="0000")%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")
lava1_det_5<-lava1_det%>%
  subset(lava1_det$min=="5000")%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")
lava1_det_n<-lava1_det%>%
  subset(lava1_det$min!="5000" & lava1_det$min!="0000")%>%
  select(-time1,-min)%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")

#prepare wav files for lift off
lava1_det_0<-distinct(lava1_det_0,datetime_mp3, .keep_all = TRUE)
lava1_det_0<-lava1_det_0%>%select(time1,min,date_mp3,time_mp3)
lava1_det_0$time_mp3<-sub("^", "1", lava1_det_0$time_mp3)
lava1_det_0 <- sapply( lava1_det_0, as.numeric )
lava1_det_0<-as.data.table(lava1_det_0,TRUE)

lava1_det_5<-distinct(lava1_det_5,datetime_mp3, .keep_all = TRUE)
lava1_det_5<-lava1_det_5%>%select(time1,min,date_mp3,time_mp3)
lava1_det_5$time_mp3<-sub("^", "1", lava1_det_5$time_mp3)
lava1_det_5 <- sapply( lava1_det_5, as.numeric )
lava1_det_5<-as.data.table(lava1_det_5,TRUE)

#find +-10 minutes of each wav file
lava1_det_0$mdate = ifelse(lava1_det_0$time1 %in% "0", 
                       lava1_det_0$date_mp3-1,
                       lava1_det_0$date_mp3*1)
lava1_det_0$ptime<-lava1_det_0[,4]+1000
lava1_det_00<-lava1_det_0%>%
  subset(time1==0)
lava1_det_0<-lava1_det_0%>%
  subset(time1!=0)
lava1_det_0$mtime<-lava1_det_0[,4]-5000
lava1_det_00$mtime<-lava1_det_00[,4]+235000
lava1_det_0<-bind_rows(lava1_det_0,lava1_det_00)
lava1_det_0$time_mp3<-format(lava1_det_0$time_mp3, 
                              scientific = FALSE, 
                              trim = TRUE)
lava1_det_0 <- sapply( lava1_det_0, as.character )
lava1_det_0<-as.data.table(lava1_det_0,TRUE)
lava1_det_0$mtime<-substring(lava1_det_0$mtime, 2)
lava1_det_0$ptime<-substring(lava1_det_0$ptime, 2)
lava1_det_0$time_mp3<-substring(lava1_det_0$time_mp3, 2)
lava1_det_0m<-lava1_det_0%>%
  select(-ptime)%>%
  unite("datetime_mp3",mdate:mtime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
lava1_det_0p<-lava1_det_0%>%
  select(-time_mp3,-mdate)%>%
  unite("datetime_mp3",date_mp3:ptime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
lava1_det_0<-lava1_det_0%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")%>%
  select(datetime_mp3)

lava1_det_5$pdate = ifelse(lava1_det_5$time1 %in% "23", 
                           lava1_det_5$date_mp3+1,
                           lava1_det_5$date_mp3*1)
lava1_det_5$ptime<-lava1_det_5[,4]+5000
lava1_det_5$mtime<-lava1_det_5[,4]-1000
lava1_det_5 <- sapply( lava1_det_5, as.character )
lava1_det_5<-as.data.table(lava1_det_5,TRUE)
lava1_det_5$mtime<-substring(lava1_det_5$mtime, 2)
lava1_det_5$ptime<-substring(lava1_det_5$ptime, 2)
lava1_det_5$time_mp3<-substring(lava1_det_5$time_mp3, 2)
lava1_det_5p<-lava1_det_5%>%
  unite("datetime_mp3",pdate:ptime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
lava1_det_5m<-lava1_det_5%>%
  select(-time_mp3,-pdate,-ptime)%>%
  unite("datetime_mp3",date_mp3:mtime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
lava1_det_5<-lava1_det_5%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")%>%
  select(datetime_mp3)
  

#prepare wav files for lift off
lava1_det_n<-distinct(lava1_det_n,datetime_mp3)
lava1_det_num<-as.numeric(lava1_det_n$datetime_mp3)
lava1_det_n<-as.data.table(lava1_det_num,TRUE)

#find +-10 minutes of each wav file
lava1_det_n$pdatetime<-lava1_det_n[,1]+1000
lava1_det_n$mdatetime<-lava1_det_n[,1]-1000
lava1_det_n$mdatetime<-format(lava1_det_n$mdatetime, 
                                  scientific = FALSE, 
                                  trim = TRUE)
lava1_det_n <- sapply( lava1_det_n, as.character )
lava1_det_n<-as.data.table(lava1_det_n,TRUE)
lava1_det_np<-lava1_det_n%>%
  select(pdatetime)
colnames(lava1_det_np)<-"datetime_mp3"
lava1_det_nm<-lava1_det_n%>%
  select(mdatetime)
colnames(lava1_det_nm)<-"datetime_mp3"
lava1_det_n<-lava1_det_n%>%
  select(lava1_det_num)
colnames(lava1_det_n)<-"datetime_mp3"

#combine all data tables
lava1_det_all<-bind_rows(lava1_det_0m,lava1_det_5m,lava1_det_nm,
                         lava1_det_0p,lava1_det_5p,lava1_det_np,
                         lava1_det_0,lava1_det_5,lava1_det_n)
lava1_det_all<-distinct(lava1_det_all,datetime_mp3)
colnames(lava1_det_all)<-'Begin File'

#make it look like wav file format
lava1_det_all$`Begin File` <- sub("^", "S4A04765_", lava1_det_all$`Begin File`)
lava1_det_all$`Begin File` <- paste0(lava1_det_all$`Begin File`, ".wav")
stri_sub(lava1_det_all$`Begin File`, 18, 17) <- "_"
lava1_det_all$`Begin File`<-gsub("240000", "000000",
                    as.character(lava1_det_all$`Begin File`))
