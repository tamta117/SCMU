#script for graphs and visuals for acoustic

library(here)
library(tidyverse)
library(ggplot2)

#### load data ####
acoustic.m <- acoustic%>%
  select(tod,time_int,hr,jday,SCMU)%>%
  group_by(hr)%>%
  #group_by(jday)%>%
  #group_by(time_int)%>%
  #group_by(tod)%>%
  summarize(m=sum(SCMU))
acoustic.r <- acoustic%>%
  select(tod,time_int,hr,jday,SCMU,CORA)%>%
  group_by(hr)%>%
  #group_by(jday)%>%
  #group_by(time_int)%>%
  #group_by(tod)%>%
  summarize(r=sum(CORA))
acoustic.plot<-left_join(acoustic.m,acoustic.r)
acoustic.plot1<-pivot_longer(acoustic.plot,cols=c("m","r"),names_to = "species",
                             values_to="nobs")

#### graph 1 ####
band<-data.frame(xstart=c(4,17), xend=c(6,19),col=letters[1:2])
ggplot()+
  geom_rect(data = band, aes(xmin = xstart, xmax = xend, 
                              ymin = 0, ymax = Inf), 
            alpha = 0.4)+
  geom_bar(data=acoustic.plot1,aes(x=hr,y=nobs,fill=as.factor(species)),
           stat = "identity", 
           position = position_dodge(0.9))+
  xlab("Hour of the day")+
  ylab("Number of detection")+
  scale_fill_manual(values = c("#4869b1","#ffb172"), name = "Species",
                    labels=c("Murrelet","Raven"))
ggsave(here("figures/acoustic.hr.png"),dpi=300)

ggplot(acoustic.plot1)+
  geom_bar(aes(x=jday,y=nobs,fill=as.factor(species)),
           stat = "identity", 
           position = position_dodge(0.9))+
  xlab("Day of the year")+
  ylab("Number of detection")+
  xlim(40,125)+
  scale_fill_manual(values = c("#4869b1","#ffb172"), name = "Species",
                    labels=c("Murrelet","Raven"))
ggsave(here("figures/acoustic.jday.png"),dpi=300)

ggplot(acoustic.plot1)+
  geom_bar(aes(x=factor(time_int, level=c("after","aduring","before")),y=nobs,fill=as.factor(species)),
           stat = "identity", 
           position = position_dodge(0.9))+
  xlab("Acoustic vs camera \n time difference")+
  ylab("Number of detection")+
  scale_x_discrete(labels=c("before","during","after"))+
  scale_fill_manual(values = c("#4869b1","#ffb172"), name = "Species",
                    labels=c("Murrelet","Raven"))
ggsave(here("figures/acoustic.time_int.png"),dpi=300)

ggplot(acoustic.plot1)+
  geom_bar(aes(x=tod,y=nobs,fill=as.factor(species)),
           stat = "identity", 
           position = position_dodge(0.9))+
  xlab("Time of day")+
  ylab("Number of detection")+
  coord_cartesian(ylim=c(0, 20))+
  scale_fill_manual(values = c("#4869b1","#ffb172"), name = "Species",
                    labels=c("Murrelet","Raven"))
ggsave(here("figures/acoustic.tod.cut.png"),dpi=300)
