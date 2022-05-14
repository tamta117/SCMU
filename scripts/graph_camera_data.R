#script for graphs and visuals for cameras

library(here)
library(tidyverse)
library(ggplot2)

#### load data ####
camera_hourly <- read.csv(here("data", "camera_hourly_mod.csv"))
camera.m <- camera_hourly%>%
  select(hour,jday,SCMU_hourly_det,tod,image_type)%>%
  group_by(hour)%>%
  #group_by(jday)%>%
  #group_by(tod)%>%
  #group_by(image_type)%>%
  summarize(m=sum(SCMU_hourly_det))
camera.r <- camera_hourly%>%
  select(hour,jday,CORA_hourly_det,tod,image_type)%>%
  group_by(hour)%>%
  #group_by(jday)%>%
  #group_by(tod)%>%
  #group_by(image_type)%>%
  summarize(r=sum(CORA_hourly_det))
camera.plot<-left_join(camera.m,camera.r)
camera.plot1<-pivot_longer(camera.plot,cols=c("m","r"),names_to = "species",
                             values_to="nobs")

#### graph 1 ####
ggplot(camera.plot1)+
  geom_rect(data = band, aes(xmin = xstart, xmax = xend, 
                             ymin = 0, ymax = Inf), 
            alpha = 0.4)+
  geom_bar(aes(x=hour,y=nobs,fill=as.factor(species)),
           stat = "identity", 
           position = position_dodge(0.9))+
  xlab("Hour of the day")+
  ylab("Number of detection")+
  scale_fill_manual(values = c("#4869b1","#ffb172"), name = "Species",
                    labels=c("Murrelet","Raven"))
ggsave(here("figures/camera.hr.png"),dpi=300)

ggplot(camera.plot1)+
  geom_bar(aes(x=jday,y=nobs,fill=as.factor(species)),
           stat = "identity", 
           position = position_dodge(0.9))+
  xlab("Day of the year")+
  ylab("Number of detection")+
  xlim(40,125)+
  scale_fill_manual(values = c("#4869b1","#ffb172"), name = "Species",
                    labels=c("Murrelet","Raven"))
ggsave(here("figures/camera.jday.png"),dpi=300)

ggplot(camera.plot1)+
  geom_bar(aes(x=tod,y=nobs,fill=as.factor(species)),
           stat = "identity", 
           position = position_dodge(0.9))+
  xlab("Time of day")+
  ylab("Number of detection")+
  scale_fill_manual(values = c("#4869b1","#ffb172"), name = "Species",
                    labels=c("Murrelet","Raven"))
ggsave(here("figures/camera.tod.png"),dpi=300)

ggplot(camera.plot1)+
  geom_bar(aes(x=image_type,y=nobs,fill=as.factor(species)),
           stat = "identity", 
           position = position_dodge(0.9))+
  xlab("Image type")+
  ylab("Number of detection")+
  scale_fill_manual(values = c("#4869b1","#ffb172"), name = "Species",
                    labels=c("Murrelet","Raven"))
ggsave(here("figures/camera.image_type.png"),dpi=300)
