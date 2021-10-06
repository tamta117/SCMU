library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyr)

#table for murrelet per hr
#fiddled with the Date because if I don't, the output table is weird
#would say something like "06" hr instead of "6" hr, not sure why
#plot also look slightly different in between the two methods and I don't know why
#please send help
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
         dayofyear=ymd(paste(yr,mon,d)))%>%
  group_by(dayofyear,hr)%>%
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
  geom_bar(aes(x=hr,y=raven),
           stat="identity",position=position_dodge(0.9), fill = "#39638b")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 24))+
  xlab("Time of day")+
  ylab("Raven")+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank())

ggsave("figures/raven_plot.jpeg", dpi=300)

#plot for joined raven and murrrelet per hr
(join_plot<-ggplot(camera_join1))+
  geom_bar(aes(x=hr,y=count, fill=Species),
           stat="identity",position="dodge")+
  xlab("Time of day")+
  ylab("Count")+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank())

ggsave("figures/rav_murr_plot.jpeg", dpi=300)
