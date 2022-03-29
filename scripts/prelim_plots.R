library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyr)

#table for murrelet per hr
#fiddled with the Date because if I don't, the output table is weird
#would say something like "06" hr instead of "6" hr, not sure why
#plot also look slightly different in between the two methods and I don't know why
#please send help
#read in csv
camera_org<-read.csv("C:\\Users\\Jelly Oon\\Documents\\Seabird\\camera_dat.csv")

camera<-read.csv("C:\\Users\\Jelly Oon\\Documents\\Seabird\\camera_dat.csv")%>%
  separate(`Time`,
           into=c("hr","misc1","min","misc2","sec"),
           sep=c(2,3,5,6))%>%
  separate(`Date`,
           into=c("yr","misc3","mon","misc4","d"),
           sep=c(4,5,7,8))%>%
  select(-misc1,-misc2,-misc3,-misc4)%>%
  mutate(datetime=ymd_hms(paste(yr,mon,d,hr,min,sec)),
         yr=year(datetime),
         mon=month(datetime),
         d=day(datetime),
         hr=hour(datetime),
         dayofyear=ymd(paste(yr,mon,d)))%>%
  group_by(dayofyear,hr)%>%
  summarize(murrelet=sum(Adult))

#table for murrelet per hr without changing Date so you can see what I'm talking about
camera1<-read.csv("C:\\Users\\Jelly Oon\\Documents\\Seabird\\camera_dat.csv")%>%
  separate(`Time`,
           into=c("hr","misc1","min","misc2","sec"),
           sep=c(2,3,5,6))%>%
  select(-misc1,-misc2)%>%
  group_by(hr)%>%
  summarize(murrelet=sum(Adult))

#table for raven per hr
camera_rav<-read.csv("C:\\Users\\Jelly Oon\\Documents\\Seabird\\camera_dat.csv")%>%
  filter(Species=="Raven")%>%
  separate(`Time`,
           into=c("hr","misc1","min","misc2","sec"),
           sep=c(2,3,5,6))%>%
  separate(`Date`,
           into=c("yr","misc3","mon","misc4","d"),
           sep=c(4,5,7,8))%>%
  select(-misc1,-misc2,-misc3,-misc4)%>%
  mutate(datetime=ymd_hms(paste(yr,mon,d,hr,min,sec)),
         yr=year(datetime),
         mon=month(datetime),
         d=day(datetime),
         hr=hour(datetime),
         dayofyear=ymd(paste(yr,mon,d)),
         pred=ifelse(PredationEvent=="true",1,0))%>%
  group_by(dayofyear,hr,pred)%>%
  summarize(raven=sum(Count))

#table for joined graph of raven and murrelet
camera_join<-left_join(camera, camera_rav)
camera_join1<-pivot_longer(camera_join,cols=c('raven','murrelet'),names_to='Species',values_to="count")

#plot murrelet per hr
(murr_plot<-ggplot(camera))+
  geom_bar(aes(x=hr,y=murrelet),
           stat="identity",position=position_dodge(0.9))+
  xlab("Time of day")+
  ylab("Murrelet")+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank())

ggsave("figures/murr_plot.jpeg", dpi=300)

#plot raven per hour
(raven_plot<-ggplot(camera_rav))+
  geom_bar(aes(x=hr,y=raven,fill=as.factor(pred)),
           stat="identity",position=position_dodge(0.9))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 24))+
  xlab("Time of day")+
  ylab("Raven")+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank())+
  scale_fill_manual(values = c("#39638b", "#26a185"),name="Predation Event")

ggsave("figures/raven_pred_plot.jpeg", dpi=300)

#plot for joined raven and murrrelet per hr
(join_plot<-ggplot(camera_join1))+
  geom_bar(aes(x=hr,y=count, fill=Species),
           stat="identity",position="dodge")+
  xlab("Time of day")+
  ylab("Count")+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank())

ggsave("figures/rav_murr_plot.jpeg", dpi=300)

#table and plot for mice
camera_mic<-read.csv("C:\\Users\\Jelly Oon\\Documents\\Seabird\\camera_dat.csv")%>%
  filter(Species=="Mouse")%>%
  separate(`Time`,
           into=c("hr","misc1","min","misc2","sec"),
           sep=c(2,3,5,6))%>%
  separate(`Date`,
           into=c("yr","misc3","mon","misc4","d"),
           sep=c(4,5,7,8))%>%
  select(-misc1,-misc2,-misc3,-misc4)%>%
  mutate(datetime=ymd_hms(paste(yr,mon,d,hr,min,sec)),
         yr=year(datetime),
         mon=month(datetime),
         d=day(datetime),
         hr=hour(datetime),
         dayofyear=ymd(paste(yr,mon,d)))%>%
  group_by(dayofyear,hr)%>%
  summarize(mice=sum(Count))

(mice_plot<-ggplot(camera_mic))+
  geom_bar(aes(x=hr,y=mice),
           stat="identity",position=position_dodge(0.9), fill = "#39638b")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 24))+
  xlab("Time of day")+
  ylab("Mice")+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank())

ggsave("figures/mice_plot.jpeg", dpi=300)

#table and plot for barn owl
camera_owl<-read.csv("C:\\Users\\Jelly Oon\\Documents\\Seabird\\camera_dat.csv")%>%
  filter(Species=="Barn owl")%>%
  separate(`Time`,
           into=c("hr","misc1","min","misc2","sec"),
           sep=c(2,3,5,6))%>%
  separate(`Date`,
           into=c("yr","misc3","mon","misc4","d"),
           sep=c(4,5,7,8))%>%
  select(-misc1,-misc2,-misc3,-misc4)%>%
  mutate(datetime=ymd_hms(paste(yr,mon,d,hr,min,sec)),
         yr=year(datetime),
         mon=month(datetime),
         d=day(datetime),
         hr=hour(datetime),
         dayofyear=ymd(paste(yr,mon,d)))%>%
  group_by(dayofyear,hr)%>%
  summarize(owl=sum(Count))

(owl_plot<-ggplot(camera_owl))+
  geom_bar(aes(x=hr,y=owl),
           stat="identity",position=position_dodge(0.9), fill = "#39638b")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 24))+
  xlab("Time of day")+
  ylab("Barn owl")+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank())

ggsave("figures/owl_plot.jpeg", dpi=300)

#join all tables
camera_join_0.5<-left_join(camera_mic, camera_owl)
camera_join_all<-left_join(camera_join,camera_join_0.5)
camera_join_all<-pivot_longer(camera_join_all,cols=c('raven','murrelet','mice','owl'),names_to='Species',values_to="count")

(join_all_plot<-ggplot(camera_join_all))+
  geom_bar(aes(x=hr,y=count, fill=Species),
           stat="identity",position="dodge")+
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 20))+
  xlab("Time of day")+
  ylab("Count")+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank())
