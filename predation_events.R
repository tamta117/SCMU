camera_pred<-camera_org%>%
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
  group_by(dayofyear,hr,Species)%>%
  summarize(predation=sum(pred))

(pred_plot<-ggplot(camera_pred))+
  geom_bar(aes(x=hr,y=predation, fill=Species),
           stat="identity",position="dodge")+
  xlab("Time of day")+
  ylab("Number of predation per hour")+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank())

ggsave("figures/predation_plot.jpeg", dpi=300)
