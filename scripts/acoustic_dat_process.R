## Script to process acoustic data for model
## 5 April 2022

## load libraries
library(tidyverse)
library(here)
library(lubridate)
library(janitor)
library(Rraven)
library(data.table)
library(stringi)

## read in acoustic txt files
#a1 <- read.csv(here("data", "acoustics", "Lava1_SM4_rav_210320.csv"), header = TRUE) %>%
#   mutate(site = "Lava1")
# #a2 <- read.csv(here("data", "acoustics", "Lava1_SM4_rav_320414.csv"), header = TRUE) %>%
#   mutate(site = "Lava1")
#a3 <- read.csv(here("data", "acoustics", "Lava1_SM4_rav_414523.csv"), header = TRUE) %>%
#   mutate(site = "Lava1")
# a4 <- read.csv(here("data", "acoustics", "Lava2_SM4_rav_210319.csv"), header = TRUE) %>%
#   mutate(site = "Lava2")
# a5 <- read.csv(here("data", "acoustics", "Moss_SM4_rav_210320.csv"), header = TRUE) %>%
#   mutate(site = "Moss")
# a6 <- read.csv(here("data", "acoustics", "Moss_SM4_rav_320414.csv"), header = TRUE) %>%
#   mutate(site = "Moss")
# a7 <- read.csv(here("data", "acoustics", "Moss_SM4_rav_414522.csv"), header = TRUE) %>%
#   mutate(site = "Moss") 
# a8 <- read.csv(here("data", "acoustics", "Moss_SM4_rav_522623.csv"), header = TRUE) %>%
#   mutate(site = "Moss") 
# a9 <- read.csv(here("data", "acoustics", "Moss_SM4_rav_623707.csv"), header = TRUE) %>%
#   mutate(site = "Moss") 
# a10 <- read.csv(here("data", "acoustics", "Pinnacle_SM4_rav_210319.csv"), header = TRUE) %>%
#   mutate(site = "Pinnacle")  
# a11 <- read.csv(here("data", "acoustics", "Pinnacle_SM4_rav_319414.csv"), header = TRUE) %>%
#   mutate(site = "Pinnacle")    
# a12 <- read.csv(here("data", "acoustics", "Pinnacle_SM4_rav_522623.csv"), header = TRUE) %>%
#   mutate(site = "Pinnacle")    

## combine dataframes into one
## need to fix time and date
# adf <- bind_rows(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) %>%
#   clean_names() %>%
#   mutate(year = 2021,
#          month = c(NA),
#          day = c(NA)) %>%
#   dplyr::select(year, month, day, site, begin_time_s, end_time_s, delta_time_s, low_freq_hz, 
#                 high_freq_hz, species, comments, begin_path, file_offset_s, begin_file)

#df$newDate <- as.POSIXct(as.Date(df$begin_time_s,origin="1899-12-30"))
#convertToDateTime(helpData$ExcelNum, origin = "1900-01-01")
#mutate(time = excel_numeric_to_date(as.numeric(as.character("begin_time_s")), date_system = "modern")) 

## export
#write.csv(adf, here("data", "acoustic_dat.csv"))

#### Make acoustic csv ####
### NEED TO TWEAK CODE
camera_org<-read.csv("data/camera/camera_dat.csv")
# camera_acoustic = subset(camera_org, select=c(-ImageQuality, -DeleteFlag, -CameraLocation,
#                                               -StartDate, -TechnicianName, -Service, -Empty, 
#                                               -Human, -HumanActivity, -Tags, -GoodPicture,
#                                               -Folder))
camera_acoustic <- camera_org
camera_acoustic <- subset(camera_acoustic, Animal=="true")
camera_lava1=subset(camera_acoustic, Site=="Lava1")
camera_lava2=subset(camera_acoustic, Site=="Lava2")
camera_moss=subset(camera_acoustic, Site=="Moss")
camera_pinnacle=subset(camera_acoustic, Site=="Pinnacle")
camera_refuge=subset(camera_acoustic, Site=="Refuge")

write.csv(camera_lava1,"odata\\lava1_acoustic.csv", row.names=FALSE)
write.csv(camera_lava2,"odata\\lava2_acoustic.csv", row.names=FALSE)
write.csv(camera_moss,"odata\\moss_acoustic.csv", row.names=FALSE)
write.csv(camera_pinnacle,"odata\\pinnacle_acoustic.csv", row.names=FALSE)
write.csv(camera_refuge,"odata\\refuge_acoustic.csv", row.names=FALSE)

## Read in cam csv with all detections from camera data by site 
lava1_cam<-read.csv(here("data/camera/archive/lava1_cam.csv"))
lava2_cam<-read.csv(here("data/camera/archive/lava2_cam.csv"))
moss_cam<-read.csv(here("data/camera/archive/moss_cam.csv"))
pinnacle_cam<-read.csv(here("data/camera/archive/pinnacle_cam.csv"))%>%
  mutate(Murrelet=ifelse(Murrelet=="true",TRUE,FALSE))
refuge_cam<-read.csv(here("data/camera/archive/refuge_cam.csv"))%>%
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

#### Add missing files Lava 2 ####
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

#### Add missing files Moss ####
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
  unite("datetime_mp3",pdate:ptime,remove=FALSE,sep="")%>%
  select(datetime_mp3)
moss_det_5m<-moss_det_5%>%
  select(-time_mp3,-ptime,-pdate)%>%
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

#### Add missing files Pinnacle ####
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

#### Add missing files Refuge ####
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

####anti_join and combine all acoustic data####
## read in data
lava1_dir<-imp_raven("data/acoustic/Lava1",all.data=TRUE)
lava1_dir$site<-"Lava1"
lava2_dir<-imp_raven("data/acoustic/Lava2",all.data=TRUE)
lava2_dir$`Begin File`<-gsub("PWR04", "PWR01",
                             as.character(lava2_dir$`Begin File`))
lava2_dir$site<-"Lava2"
moss_dir<-imp_raven("data/acoustic/Moss",all.data=TRUE)
moss_dir$site<-"Moss"
pinn_dir<-imp_raven("data/acoustic/Pinnacle",all.data=TRUE)
pinn_dir$site<-"Pinnacle"
ref_dir<-imp_raven("data/acoustic/Refuge",all.data=TRUE)
ref_dir$site<-"Refuge"

## find unique values
lava1_acoustic<-distinct(lava1_dir,`Begin File`)
lava2_acoustic<-distinct(lava2_dir,`Begin File`)
moss_acoustic<-distinct(moss_dir,`Begin File`)
pinn_acoustic<-distinct(pinn_dir,`Begin File`)
ref_acoustic<-distinct(ref_dir,`Begin File`)

## anti_join commences
lava1_non_det <- anti_join(lava1_det_all ,lava1_acoustic, by = "Begin File")
lava2_non_det <- anti_join(lava2_det_all ,lava2_acoustic, by = "Begin File")
moss_non_det <- anti_join(moss_det_all ,moss_acoustic, by = "Begin File")
pinn_non_det <- anti_join(pinn_det_all ,pinn_acoustic, by = "Begin File")
ref_non_det <- anti_join(ref_det_all ,ref_acoustic, by = "Begin File")

lava1_non_det$site<-"Lava1"
lava2_non_det$site<-"Lava2"
moss_non_det$site<-"Moss"
pinn_non_det$site<-"Pinnacle"
ref_non_det$site<-"Refuge"

lava1_non_det$Species<-"N"
lava2_non_det$Species<-"N"
moss_non_det$Species<-"N"
pinn_non_det$Species<-"N"
ref_non_det$Species<-"N"

#combine all processed acoustic tables
acoustic_dir<-rbindlist(list(lava1_dir,lava2_dir,moss_dir,pinn_dir,
                             ref_dir,lava1_non_det,lava2_non_det,
                             moss_non_det,pinn_non_det,ref_non_det),
                        fill=TRUE)

#emulate camera_dat table
acoustic_cam<-acoustic_dir%>%
  select(`Begin File`, Species, site)%>%
  distinct(`Begin File`,.keep_all=TRUE)%>%
  separate(`Begin File`,
           into=c("ex1","yr","mnth","d","ex2","hr","min","sec","ex3"),
           sep=c(-19,-15,-13,-11,-10,-8,-6,-4),remove=FALSE)%>%
  select(-ex1,-ex2,-ex3)%>%
  unite(c(yr,mnth,d),col=date,sep="-",remove=FALSE)%>%
  unite(c(hr,min,sec),col=time,sep=":",remove=FALSE)%>%
  unite(date,time,col=date_time,sep=" ",remove=FALSE)%>%
  mutate(date_time = as_datetime(date_time, format = "%Y-%m-%d %H:%M:%S"))%>%
  mutate(jday = yday(date_time),
         SCMU=case_when(Species=="M"~1,TRUE~0),
         CORA=case_when(Species=="R"~1,TRUE~0))%>%
  select(`Begin File`,site,date,time,date_time,yr,mnth,d,jday,
         hr,min,sec,SCMU,CORA)

write_csv(acoustic_cam,here("data/acoustic/acoustic_cam.csv"))