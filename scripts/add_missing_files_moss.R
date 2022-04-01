#find all processed wav files in moss
libribrary(tidyverse)
library(lubridate)
library(stringi)

#filter for moss
moss_det<-ravmurr_cam%>%
  subset(Site=="Moss")%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")
moss_det<-distinct(moss_det,datetime_mp3,.keep_all = TRUE)

#merge time and date + filter for 00 and 50 minutes
moss_det_0<-moss_det%>%
  subset(moss_det$min=="0000")
moss_det_5<-moss_det%>%
  subset(moss_det$min=="5000")
moss_det_n<-moss_det%>%
  subset(moss_det$min!="5000" & moss_det$min!="0000")

#prepare wav files for lift off
moss_det_0<-moss_det_0%>%select(time1,min,date_mp3,time_mp3)
moss_det_0$time_mp3<-sub("^", "1", moss_det_0$time_mp3)
moss_det_0 <- sapply( moss_det_0, as.numeric )
moss_det_0<-as.data.table(moss_det_0,TRUE)

moss_det_5<-moss_det_5%>%select(time1,min,date_mp3,time_mp3)
moss_det_5$time_mp3<-sub("^", "1", moss_det_5$time_mp3)
moss_det_5 <- sapply( moss_det_5, as.numeric )
moss_det_5<-as.data.table(moss_det_5,TRUE)

moss_det_n<-moss_det_n%>%select(time1,min,date_mp3,time_mp3)
moss_det_n$time_mp3<-sub("^", "1", moss_det_n$time_mp3)
moss_det_n <- sapply( moss_det_n, as.numeric )
moss_det_n<-as.data.table(moss_det_n,TRUE)

#find +-10 minutes of each wav file
moss_det_0$mdate = ifelse(moss_det_0$time1 %in% "0", 
                           moss_det_0$date_mp3-1,
                           moss_det_0$date_mp3*1)
moss_det_0$ptime<-moss_det_0[,4]+1000
moss_det_0$mtime = ifelse(moss_det_0$time1 %in% "0", 
                           moss_det_0$time_mp3+235000,
                           moss_det_0$time_mp3-5000)
moss_det_0$time_mp3<-format(moss_det_0$time_mp3, 
                             scientific = FALSE, 
                             trim = TRUE)
moss_det_0 <- sapply( moss_det_0, as.character )
moss_det_0<-as.data.table(moss_det_0,TRUE)
moss_det_0$mtime<-substring(moss_det_0$mtime, 2)
moss_det_0$ptime<-substring(moss_det_0$ptime, 2)
moss_det_0$time_mp3<-substring(moss_det_0$time_mp3, 2)
moss_det_0m<-moss_det_0%>%
  select(-ptime)%>%
  unite("datetime_mp3",mdate:mtime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
moss_det_0p<-moss_det_0%>%
  select(-time_mp3,-mdate)%>%
  unite("datetime_mp3",date_mp3:ptime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
moss_det_0<-moss_det_0%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")%>%
  select(datetime_mp3)

moss_det_5$pdate = ifelse(moss_det_5$time1 %in% "23", 
                           moss_det_5$date_mp3+1,
                           moss_det_5$date_mp3*1)
moss_det_5$ptime<-moss_det_5[,4]+5000
moss_det_5$mtime<-moss_det_5[,4]-1000
moss_det_5 <- sapply( moss_det_5, as.character )
moss_det_5<-as.data.table(moss_det_5,TRUE)
moss_det_5$mtime<-substring(moss_det_5$mtime, 2)
moss_det_5$ptime<-substring(moss_det_5$ptime, 2)
moss_det_5$time_mp3<-substring(moss_det_5$time_mp3, 2)
moss_det_5p<-moss_det_5%>%
  select(-time_mp3)%>%
  unite("datetime_mp3",date_mp3:ptime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
moss_det_5m<-moss_det_5%>%
  select(-time_mp3,-ptime)%>%
  unite("datetime_mp3",date_mp3:mtime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
moss_det_5<-moss_det_5%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")%>%
  select(datetime_mp3)

moss_det_n$ptime<-moss_det_n[,4]+1000
moss_det_n$mtime<-moss_det_n[,4]-1000
moss_det_n$mtime<-format(moss_det_n$mtime, 
                              scientific = FALSE, 
                              trim = TRUE)
moss_det_n <- sapply( moss_det_n, as.character )
moss_det_n<-as.data.table(moss_det_n,TRUE)
moss_det_n$mtime<-substring(moss_det_n$mtime, 2)
moss_det_n$ptime<-substring(moss_det_n$ptime, 2)
moss_det_n$time_mp3<-substring(moss_det_n$time_mp3, 2)
moss_det_np<-moss_det_n%>%
  select(-time_mp3)%>%
  unite("datetime_mp3",date_mp3:ptime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
moss_det_nm<-moss_det_n%>%
  select(-time_mp3,-ptime)%>%
  unite("datetime_mp3",date_mp3:mtime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
moss_det_n<-moss_det_n%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")%>%
  select(datetime_mp3)

#combine all data tables
moss_det_all<-bind_rows(moss_det_0m,moss_det_5m,moss_det_nm,
                         moss_det_0p,moss_det_5p,moss_det_np,
                         moss_det_0,moss_det_5,moss_det_n)
moss_det_all<-distinct(moss_det_all,datetime_mp3)
colnames(moss_det_all)<-'Begin File'

#make it look like wav file format
moss_det_all$`Begin File` <- sub("^", "PWR04_", moss_det_all$`Begin File`)
moss_det_all$`Begin File` <- paste0(moss_det_all$`Begin File`, ".wav")
stri_sub(moss_det_all$`Begin File`, 15, 14) <- "_"
