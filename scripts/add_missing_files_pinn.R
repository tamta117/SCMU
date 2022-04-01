#find all processed wav files in pinnacle
libribrary(tidyverse)
library(lubridate)
library(stringi)

#filter for pinnacle
pinn_det<-ravmurr_cam%>%
  subset(Site=="Pinnacle")%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")
pinn_det<-distinct(pinn_det,datetime_mp3,.keep_all = TRUE)

#merge time and date + filter for 00 and 50 minutes
pinn_det_0<-pinn_det%>%
  subset(pinn_det$min=="0000")
pinn_det_5<-pinn_det%>%
  subset(pinn_det$min=="5000")
pinn_det_n<-pinn_det%>%
  subset(pinn_det$min!="5000" & pinn_det$min!="0000")

#prepare wav files for lift off
pinn_det_0<-pinn_det_0%>%select(time1,min,date_mp3,time_mp3)
pinn_det_0$time_mp3<-sub("^", "1", pinn_det_0$time_mp3)
pinn_det_0 <- sapply( pinn_det_0, as.numeric )
pinn_det_0<-as.data.table(pinn_det_0,TRUE)

pinn_det_5<-pinn_det_5%>%select(time1,min,date_mp3,time_mp3)
pinn_det_5$time_mp3<-sub("^", "1", pinn_det_5$time_mp3)
pinn_det_5 <- sapply( pinn_det_5, as.numeric )
pinn_det_5<-as.data.table(pinn_det_5,TRUE)

pinn_det_n<-pinn_det_n%>%select(time1,min,date_mp3,time_mp3)
pinn_det_n$time_mp3<-sub("^", "1", pinn_det_n$time_mp3)
pinn_det_n <- sapply( pinn_det_n, as.numeric )
pinn_det_n<-as.data.table(pinn_det_n,TRUE)

#find +-10 minutes of each wav file
pinn_det_0$mdate = ifelse(pinn_det_0$time1 %in% "0", 
                          pinn_det_0$date_mp3-1,
                          pinn_det_0$date_mp3*1)
pinn_det_0$ptime<-pinn_det_0[,4]+1000
pinn_det_0$mtime = ifelse(pinn_det_0$time1 %in% "0", 
                          pinn_det_0$time_mp3+235000,
                          pinn_det_0$time_mp3-5000)
pinn_det_0$time_mp3<-format(pinn_det_0$time_mp3, 
                            scientific = FALSE, 
                            trim = TRUE)
pinn_det_0 <- sapply( pinn_det_0, as.character )
pinn_det_0<-as.data.table(pinn_det_0,TRUE)
pinn_det_0$mtime<-substring(pinn_det_0$mtime, 2)
pinn_det_0$ptime<-substring(pinn_det_0$ptime, 2)
pinn_det_0$time_mp3<-substring(pinn_det_0$time_mp3, 2)
pinn_det_0m<-pinn_det_0%>%
  select(-ptime)%>%
  unite("datetime_mp3",mdate:mtime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
pinn_det_0p<-pinn_det_0%>%
  select(-time_mp3,-mdate)%>%
  unite("datetime_mp3",date_mp3:ptime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
pinn_det_0<-pinn_det_0%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")%>%
  select(datetime_mp3)

pinn_det_5$pdate = ifelse(pinn_det_5$time1 %in% "23", 
                          pinn_det_5$date_mp3+1,
                          pinn_det_5$date_mp3*1)
pinn_det_5$ptime<-pinn_det_5[,4]+5000
pinn_det_5$mtime<-pinn_det_5[,4]-1000
pinn_det_5 <- sapply( pinn_det_5, as.character )
pinn_det_5<-as.data.table(pinn_det_5,TRUE)
pinn_det_5$mtime<-substring(pinn_det_5$mtime, 2)
pinn_det_5$ptime<-substring(pinn_det_5$ptime, 2)
pinn_det_5$time_mp3<-substring(pinn_det_5$time_mp3, 2)
pinn_det_5p<-pinn_det_5%>%
  unite("datetime_mp3",pdate:ptime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
pinn_det_5m<-pinn_det_5%>%
  select(-time_mp3,-ptime,-pdate)%>%
  unite("datetime_mp3",date_mp3:mtime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
pinn_det_5<-pinn_det_5%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")%>%
  select(datetime_mp3)

pinn_det_n$ptime<-pinn_det_n[,4]+1000
pinn_det_n$mtime<-pinn_det_n[,4]-1000
pinn_det_n$mtime<-format(pinn_det_n$mtime, 
                         scientific = FALSE, 
                         trim = TRUE)
pinn_det_n <- sapply( pinn_det_n, as.character )
pinn_det_n<-as.data.table(pinn_det_n,TRUE)
pinn_det_n$mtime<-substring(pinn_det_n$mtime, 2)
pinn_det_n$ptime<-substring(pinn_det_n$ptime, 2)
pinn_det_n$time_mp3<-substring(pinn_det_n$time_mp3, 2)
pinn_det_np<-pinn_det_n%>%
  select(-time_mp3)%>%
  unite("datetime_mp3",date_mp3:ptime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
pinn_det_nm<-pinn_det_n%>%
  select(-time_mp3,-ptime)%>%
  unite("datetime_mp3",date_mp3:mtime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
pinn_det_n<-pinn_det_n%>%
  unite("datetime_mp3",date_mp3:time_mp3,remove=FALSE,sep="")%>%
  select(datetime_mp3)

#combine all data tables
pinn_det_all<-bind_rows(pinn_det_0m,pinn_det_5m,pinn_det_nm,
                        pinn_det_0p,pinn_det_5p,pinn_det_np,
                        pinn_det_0,pinn_det_5,pinn_det_n)
pinn_det_all<-distinct(pinn_det_all,datetime_mp3)
colnames(pinn_det_all)<-'Begin File'

#make it look like wav file format
pinn_det_all$`Begin File` <- sub("^", "PWR05_", pinn_det_all$`Begin File`)
pinn_det_all$`Begin File` <- paste0(pinn_det_all$`Begin File`, ".wav")
stri_sub(pinn_det_all$`Begin File`, 15, 14) <- "_"
