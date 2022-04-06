#find all processed wav files in lava2
libribrary(tidyverse)
library(lubridate)
library(stringi)

#filter for lava2
lava2_det<-ravmurr_cam%>%
  subset(Site=="Lava2" & date_mp3<20210522)%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")
lava2_det<-distinct(lava2_det,datetime_mp3,.keep_all = TRUE)

#merge time and date + filter for 00 and 50 minutes
lava2_det_0<-lava2_det%>%
  subset(lava2_det$min=="0000")
lava2_det_5<-lava2_det%>%
  subset(lava2_det$min=="5000")
lava2_det_n<-lava2_det%>%
  subset(lava2_det$min!="5000" & lava2_det$min!="0000")

#prepare wav files for lift off
lava2_det_0<-lava2_det_0%>%select(time1,min,date_mp3,time_mp3)
lava2_det_0$time_mp3<-sub("^", "1", lava2_det_0$time_mp3)
lava2_det_0 <- sapply( lava2_det_0, as.numeric )
lava2_det_0<-as.data.table(lava2_det_0,TRUE)

lava2_det_5<-lava2_det_5%>%select(time1,min,date_mp3,time_mp3)
lava2_det_5$time_mp3<-sub("^", "1", lava2_det_5$time_mp3)
lava2_det_5 <- sapply( lava2_det_5, as.numeric )
lava2_det_5<-as.data.table(lava2_det_5,TRUE)

lava2_det_n<-lava2_det_n%>%select(time1,min,date_mp3,time_mp3)
lava2_det_n$time_mp3<-sub("^", "1", lava2_det_n$time_mp3)
lava2_det_n <- sapply( lava2_det_n, as.numeric )
lava2_det_n<-as.data.table(lava2_det_n,TRUE)

#find +-10 minutes of each wav file
lava2_det_0$mdate = ifelse(lava2_det_0$time1 %in% "0", 
                           lava2_det_0$date_mp3-1,
                           lava2_det_0$date_mp3*1)
lava2_det_0$ptime<-lava2_det_0[,4]+1000
lava2_det_0$mtime = ifelse(lava2_det_0$time1 %in% "0", 
                           lava2_det_0$time_mp3+235000,
                           lava2_det_0$time_mp3-5000)
lava2_det_0$time_mp3<-format(lava2_det_0$time_mp3, 
                             scientific = FALSE, 
                             trim = TRUE)
lava2_det_0 <- sapply( lava2_det_0, as.character )
lava2_det_0<-as.data.table(lava2_det_0,TRUE)
lava2_det_0$mtime<-substring(lava2_det_0$mtime, 2)
lava2_det_0$ptime<-substring(lava2_det_0$ptime, 2)
lava2_det_0$time_mp3<-substring(lava2_det_0$time_mp3, 2)
lava2_det_0m<-lava2_det_0%>%
  select(-ptime)%>%
  unite("datetime_mp3",mdate:mtime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
lava2_det_0p<-lava2_det_0%>%
  select(-time_mp3,-mdate)%>%
  unite("datetime_mp3",date_mp3:ptime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
lava2_det_0<-lava2_det_0%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")%>%
  select(datetime_mp3)

lava2_det_5$ptime<-lava2_det_5[,4]+5000
lava2_det_5$mtime<-lava2_det_5[,4]-1000
lava2_det_5 <- sapply( lava2_det_5, as.character )
lava2_det_5<-as.data.table(lava2_det_5,TRUE)
lava2_det_5$mtime<-substring(lava2_det_5$mtime, 2)
lava2_det_5$ptime<-substring(lava2_det_5$ptime, 2)
lava2_det_5$time_mp3<-substring(lava2_det_5$time_mp3, 2)
lava2_det_5p<-lava2_det_5%>%
  select(-time_mp3)%>%
  unite("datetime_mp3",date_mp3:ptime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
lava2_det_5m<-lava2_det_5%>%
  select(-time_mp3,-ptime)%>%
  unite("datetime_mp3",date_mp3:mtime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
lava2_det_5<-lava2_det_5%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")%>%
  select(datetime_mp3)

lava2_det_n$ptime<-lava2_det_n[,4]+1000
lava2_det_n$mtime<-lava2_det_n[,4]-1000
lava2_det_n <- sapply( lava2_det_n, as.character )
lava2_det_n<-as.data.table(lava2_det_n,TRUE)
lava2_det_n$mtime<-substring(lava2_det_n$mtime, 2)
lava2_det_n$ptime<-substring(lava2_det_n$ptime, 2)
lava2_det_n$time_mp3<-substring(lava2_det_n$time_mp3, 2)
lava2_det_np<-lava2_det_n%>%
  select(-time_mp3)%>%
  unite("datetime_mp3",date_mp3:ptime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
lava2_det_nm<-lava2_det_n%>%
  select(-time_mp3,-ptime)%>%
  unite("datetime_mp3",date_mp3:mtime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
lava2_det_n<-lava2_det_n%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")%>%
  select(datetime_mp3)

#combine all data tables
lava2_det_all<-bind_rows(lava2_det_0m,lava2_det_5m,lava2_det_nm,
                         lava2_det_0p,lava2_det_5p,lava2_det_np,
                         lava2_det_0,lava2_det_5,lava2_det_n)
lava2_det_all<-distinct(lava2_det_all,datetime_mp3)
colnames(lava2_det_all)<-'Begin File'

#make it look like wav file format
lava2_det_all$`Begin File` <- sub("^", "PWR01_", lava2_det_all$`Begin File`)
lava2_det_all$`Begin File` <- paste0(lava2_det_all$`Begin File`, ".wav")
stri_sub(lava2_det_all$`Begin File`, 15, 14) <- "_"
