#find all processed wav files in refuge
library(tidyverse)
library(lubridate)
library(stringi)

#filter for refuge
ref_det<-ravmurr_cam%>%
  subset(Site=="Refuge")%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")
ref_det<-distinct(ref_det,datetime_mp3,.keep_all = TRUE)

#merge time and date + filter for 00 and 50 minutes
ref_det_0<-ref_det%>%
  subset(ref_det$min=="0000")
ref_det_5<-ref_det%>%
  subset(ref_det$min=="5000")
ref_det_n<-ref_det%>%
  subset(ref_det$min!="5000" & ref_det$min!="0000")

#prepare wav files for lift off
ref_det_0<-ref_det_0%>%select(time1,min,date_mp3,time_mp3)
ref_det_0$time_mp3<-sub("^", "1", ref_det_0$time_mp3)
ref_det_0 <- sapply( ref_det_0, as.numeric )
ref_det_0<-as.data.table(ref_det_0,TRUE)

ref_det_5<-ref_det_5%>%select(time1,min,date_mp3,time_mp3)
ref_det_5$time_mp3<-sub("^", "1", ref_det_5$time_mp3)
ref_det_5 <- sapply( ref_det_5, as.numeric )
ref_det_5<-as.data.table(ref_det_5,TRUE)

ref_det_n<-ref_det_n%>%select(time1,min,date_mp3,time_mp3)
ref_det_n$time_mp3<-sub("^", "1", ref_det_n$time_mp3)
ref_det_n <- sapply( ref_det_n, as.numeric )
ref_det_n<-as.data.table(ref_det_n,TRUE)

#find +-10 minutes of each wav file
ref_det_0$ptime<-ref_det_0[,4]+1000
ref_det_0$mtime<-ref_det_0[,4]-5000
ref_det_0 <- sapply( ref_det_0, as.character )
ref_det_0<-as.data.table(ref_det_0,TRUE)
ref_det_0$mtime<-substring(ref_det_0$mtime, 2)
ref_det_0$ptime<-substring(ref_det_0$ptime, 2)
ref_det_0$time_mp3<-substring(ref_det_0$time_mp3, 2)
ref_det_0m<-ref_det_0%>%
  select(-ptime,-time_mp3)%>%
  unite("datetime_mp3",date_mp3:mtime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
ref_det_0p<-ref_det_0%>%
  select(-time_mp3)%>%
  unite("datetime_mp3",date_mp3:ptime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
ref_det_0<-ref_det_0%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")%>%
  select(datetime_mp3)

ref_det_5$ptime<-ref_det_5[,4]+5000
ref_det_5$mtime<-ref_det_5[,4]-1000
ref_det_5 <- sapply( ref_det_5, as.character )
ref_det_5<-as.data.table(ref_det_5,TRUE)
ref_det_5$mtime<-substring(ref_det_5$mtime, 2)
ref_det_5$ptime<-substring(ref_det_5$ptime, 2)
ref_det_5$time_mp3<-substring(ref_det_5$time_mp3, 2)
ref_det_5p<-ref_det_5%>%
  select(-time_mp3)%>%
  unite("datetime_mp3",date_mp3:ptime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
ref_det_5m<-ref_det_5%>%
  select(-time_mp3,-ptime)%>%
  unite("datetime_mp3",date_mp3:mtime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
ref_det_5<-ref_det_5%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")%>%
  select(datetime_mp3)

ref_det_n$ptime<-ref_det_n[,4]+1000
ref_det_n$mtime<-ref_det_n[,4]-1000
ref_det_n$mtime<-format(ref_det_n$mtime, 
                         scientific = FALSE, 
                         trim = TRUE)
ref_det_n <- sapply( ref_det_n, as.character )
ref_det_n<-as.data.table(ref_det_n,TRUE)
ref_det_n$mtime<-substring(ref_det_n$mtime, 2)
ref_det_n$ptime<-substring(ref_det_n$ptime, 2)
ref_det_n$time_mp3<-substring(ref_det_n$time_mp3, 2)
ref_det_np<-ref_det_n%>%
  select(-time_mp3)%>%
  unite("datetime_mp3",date_mp3:ptime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
ref_det_nm<-ref_det_n%>%
  select(-time_mp3,-ptime)%>%
  unite("datetime_mp3",date_mp3:mtime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
ref_det_n<-ref_det_n%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")%>%
  select(datetime_mp3)

#combine all data tables
ref_det_all<-bind_rows(ref_det_0m,ref_det_5m,ref_det_nm,
                        ref_det_0p,ref_det_5p,ref_det_np,
                        ref_det_0,ref_det_5,ref_det_n)
ref_det_all<-distinct(ref_det_all,datetime_mp3)
colnames(ref_det_all)<-'Begin File'

#make it look like wav file format
ref_det_all$`Begin File` <- sub("^", "S4A07766_", ref_det_all$`Begin File`)
ref_det_all$`Begin File` <- paste0(ref_det_all$`Begin File`, ".wav")
stri_sub(ref_det_all$`Begin File`, 18, 17) <- "_"
