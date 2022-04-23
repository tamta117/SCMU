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

#### Find all processed acoustic files ####
#Read in cam csv with all detections from camera data by site 
lava1_cam<-read.csv(here("data/camera/archive/lava1_cam.csv"))
lava2_cam<-read.csv(here("data/camera/archive/lava2_cam.csv"))
moss_cam<-read.csv(here("data/camera/archive/moss_cam.csv"))
pinnacle_cam<-read.csv(here("data/camera/archive/pinnacle_cam.csv"))%>%
  mutate(Murrelet=ifelse(Murrelet=="true",TRUE,FALSE))
refuge_cam<-read.csv(here("data/camera/archive/refuge_cam.csv"))%>%
  mutate(Murrelet=ifelse(Murrelet=="true",TRUE,FALSE))

#join all cam files
all_cam<-rbindlist(list(lava1_cam, lava2_cam, moss_cam,
                        pinnacle_cam, refuge_cam))%>%
  unite("date_time",Date:Time,remove=FALSE, sep=" ")

#change time into desirable format
all_cam$date_time<-parse_date_time(all_cam$date_time, 
                                   c( "ymd HMS","mdy HMS"), tz="")
ravmurr_cam<-all_cam%>%
  subset(Species=="Raven" | Murrelet=="TRUE")%>%
  mutate(yr=year(date_time),
         mnth=month(date_time),
         d=day(date_time),
         hr=hour(date_time),
         min=minute(date_time),
         sec=second(date_time))
ravmurr_cam$min<-sapply(ravmurr_cam$min,as.character)
ravmurr_cam$min<-str_pad(ravmurr_cam$min, width=2, side="left", pad="0")
time_cam<-ravmurr_cam%>%
  separate(min,
           into=c("min1","min2"),
           sep=1, remove=FALSE)%>%
  select(-min2)
time_cam$min1<-str_pad(time_cam$min1, width=2, side="right", pad="0")
time_cam$min1<-sapply(time_cam$min1,as.integer)
time_cam$sec1<-"00"
time_cam$sec1<-sapply(time_cam$sec1,as.integer)

# +- 10 minutes
time.p<-time_cam%>%
  mutate(datetime=ymd_hms(paste(yr,mnth,d,hr,min1,sec1)),
         datetime.r=datetime+600)
time.m<-time_cam%>%
  mutate(datetime=ymd_hms(paste(yr,mnth,d,hr,min1,sec1)),
         datetime.r=datetime-600)
time<-time_cam%>%
  mutate(datetime.r=ymd_hms(paste(yr,mnth,d,hr,min1,sec1)))

# combine all time
time.all<-bind_rows(time.p,time.m,time)
time.all<-distinct(time.all,datetime.r,.keep_all = TRUE)

# make it look like wav file format
time.all$datetime.r<-gsub(" ", "_",
                    as.character(time.all$datetime.r))
time.all$datetime.r<-gsub(":", "",
                          as.character(time.all$datetime.r))
time.all$datetime.r<-gsub("-", "",
                          as.character(time.all$datetime.r))
time.all$datetime.r <- paste0(time.all$datetime.r, ".wav")

lava1.time<-time.all%>%
  subset(Site=="Lava1" & date_time<"2021-05-23")%>%
  select(datetime.r,Site)
lava1.time$datetime.r <- sub("^", "S4A04765_", lava1.time$datetime.r)
colnames(lava1.time)<-c("Begin File","site")

lava2.time<-time.all%>%
  subset(Site=="Lava2" & date_time<"2021-05-22")%>%
  select(datetime.r,Site)
lava2.time$datetime.r <- sub("^", "PWR01_", lava2.time$datetime.r)
colnames(lava2.time)<-c("Begin File","site")

moss.time<-time.all%>%
  subset(Site=="Moss")%>%
  select(datetime.r,Site)
moss.time$datetime.r <- sub("^", "PWR04_", moss.time$datetime.r)
colnames(moss.time)<-c("Begin File","site")

pinn.time<-time.all%>%
  subset(Site=="Pinnacle")%>%
  select(datetime.r,Site)
pinn.time$datetime.r <- sub("^", "PWR05_", pinn.time$datetime.r)
colnames(pinn.time)<-c("Begin File","site")

ref.time<-time.all%>%
  subset(Site=="Refuge")%>%
  select(datetime.r,Site)
ref.time$datetime.r <- sub("^", "S4A07766_", ref.time$datetime.r)
colnames(ref.time)<-c("Begin File","site")

# combine all tables
time.fin<-bind_rows(lava1.time,lava2.time,moss.time,pinn.time,
                    ref.time)

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

# bind tables
all.acoustic<-bind_rows(lava1_dir,lava2_dir,moss_dir,pinn_dir,
                        ref_dir)
all.acoustic<-distinct(all.acoustic,`Begin File`,.keep_all=TRUE)

#anti join
all.non.det<-anti_join(time.fin,all.acoustic,by="Begin File")

#combine all processed acoustic tables
acoustic_dir<-rbindlist(list(all.acoustic,all.non.det),fill=TRUE)

#emulate camera_dat table
acoustic_cam<-acoustic_dir%>%
  select(`Begin File`, Species, site)%>%
  distinct(`Begin File`,.keep_all=TRUE)%>%
  separate(`Begin File`,
           into=c("ex1","yr","mnth","d","ex2","hr","min","sec","ex3"),
           sep=c(-19,-15,-13,-11,-10,-8,-6,-4),remove=FALSE)%>%
  select(-ex1,-ex2,-ex3)%>%
  unite(c(hr,min,sec),col=time,sep=":",remove=FALSE)%>%
  mutate(date=ymd(paste(yr,mnth,d)),
         date_time=ymd_hms(paste(date,time)),
         jday = yday(date_time),
         SCMU=case_when(Species=="M"~1,TRUE~0),
         CORA=case_when(Species=="R"~1,TRUE~0))%>%
  select(`Begin File`,site,date,time,date_time,yr,mnth,d,jday,
         hr,min,sec,SCMU,CORA)

write_csv(acoustic_cam,here("data/acoustic/acoustic_cam.csv"))

#### bin time into dusk and day ####
acoustic_cam<-read.csv(here("data/acoustic/acoustic_cam.csv"))
acoustic_cam$hr<-as.numeric(acoustic_cam$hr)
acoustic_cam1<-acoustic_cam%>%
  subset(acoustic_cam$date<="2021-03-15")%>%
  mutate(tod=case_when(
    hr<=6~"dawn",
    hr>=7 & hr<=16 ~ "day",
    hr>=17~"dusk"))
acoustic_cam2<-acoustic_cam%>%
  subset(acoustic_cam$date>="2021-03-16" & 
           acoustic_cam$date<="2021-05-01")%>%
  mutate(tod=case_when(
    hr<=5~"dawn",
    hr>=6 & hr<=17 ~ "day",
    hr>=18~"dusk"))
acoustic_cam3<-acoustic_cam%>%
  subset(acoustic_cam$date>="2021-05-02" & 
           acoustic_cam$date<="2021-05-30")%>%
  mutate(tod=case_when(
    hr<=4~"dawn",
    hr>=5 & hr<=17 ~ "day",
    hr>=18~"dusk"))
acoustic_cam4<-acoustic_cam%>%
  subset(acoustic_cam$date>="2021-05-31")%>%
  mutate(tod=case_when(
    hr<=4~"dawn",
    hr>=5 & hr<=18 ~ "day",
    hr>=19~"dusk"))
acoustic_cam5<-bind_rows(acoustic_cam1,acoustic_cam2,
                         acoustic_cam3,acoustic_cam4)
write_csv(acoustic_cam5,here("data/acoustic/acoustic_cam_tod.csv"))
