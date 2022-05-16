#create prediction plots for models

library(effects)
library(sjPlot)
library(ggeffects)

#camera murrelet
plot(allEffects(m1),selection=1,xlab="Image type",
     ylab="Probability of \n murrelet detection",rescale.axis=F)
plot(allEffects(m1),selection=2,xlab="Time of day",
     ylab="Probability of \n murrelet detection",rescale.axis=F)
plot(allEffects(m1),selection=3,xlab="Day of the year",
     ylab="Probability of \n murrelet detection",rescale.axis=F)

#camera raven
plot(allEffects(m2.5),selection=1,xlab="Image type",
     ylab="Probability of \n raven detection",rescale.axis=F)
plot(allEffects(m2.5),selection=2,xlab="Time of day",
     ylab="Probability of \n raven detection",rescale.axis=F)
plot(allEffects(m2.5),selection=3,xlab="Day of the year",
     ylab="Probability of \n raven detection",rescale.axis=F)
plot(allEffects(m5),selection=3,xlab="Day of the year",
     ylab="Probability of murrelet detection", rescale.axis=F)

#acoustic
plot(allEffects(m5),selection=1,xlab="Time difference",
     ylab="Probability of \n murrelet detection",rescale.axis=F)
plot(allEffects(m5),selection=2,xlab="Time of day",
     ylab="Probability of \n murrelet detection",rescale.axis=F)
plot(allEffects(m5),selection=3,xlab="Day of the year",
     ylab="Probability of \n murrelet detection",rescale.axis=F)

#let's try something else
#camera murrelet
plot_model(m1, type="pred", terms="jday.stand [all]", title="",
           axis.title = c("Day of the year",
                          "Probability of \n murrelet detection"),
           color="#2f4a8a")
ggsave("figures/pred.cam.jday.png")
plot_model(m1, type="pred",terms="tod")
ggsave("figures/pred.cam.tod.png")

#let's try something else pt. 2
#camera murrelet
jday <- ggpredict(m1, terms = "jday.stand [all]")
jday.mur<-ggplot(jday, aes(x, predicted)) + 
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.15, fill="#2f4a8a")+
  geom_smooth(color="#2f4a8a")+
  labs(x="Day of the year", y="Probability of \n murrelet detection")
ggsave("figures/pred.cam.jday.png",width=1175,height=749,units="px",dpi=300)

tod <- ggpredict(m1, terms = "tod")
ggplot(tod, aes(x, predicted)) + 
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high),color="#2f4a8a")+
  labs(x="Time of day", y="Probability of \n murrelet detection")+
  scale_x_discrete(labels=c("Dawn","Day","Dusk","Night"))
ggsave("figures/pred.cam.tod.png")

image_type <- ggpredict(m1, terms = "image_type")
ggplot(image_type, aes(x, predicted)) + 
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high),color="#2f4a8a")+
  labs(x="Image type", y="Probability of \n murrelet detection")+
  scale_x_discrete(labels=c("Motion","Timelapse"))
ggsave("figures/pred.cam.image_type.png")

#camera raven
image_type1 <- ggpredict(m2.5, terms = "image_type")
image_type1$species<-"Raven"
image_type$species<-"Murrelet"
image_type.bind<-bind_rows(image_type,image_type1)
ggplot(image_type.bind, aes(x, predicted, col=species)) + 
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high),position = position_dodge(width = 0.3))+
  labs(x="Image type", y="Probability of detection")+
  scale_x_discrete(labels=c("Motion","Timelapse"))+
  scale_color_manual(values = c("#4869b1","#ffb172"), name = "Species",
                     labels=c("Murrelet","Raven"))+
  theme(legend.key = element_rect(fill="white"))+
  guides(color=guide_legend(override.aes=list(fill=NA)))
ggsave("figures/pred.cam.bind.image_type.png",width=1175,height=749,units="px",dpi=300)

jday1 <- ggpredict(m2.5, terms = "jday.stand [all]")
jday1$species<-"Raven"
jday$species<-"Murrelet"
jday.bind<-bind_rows(jday1,jday)
jday.bind$jday<-jday.bind$x*sd(camera_hourly$jday)+mean(camera_hourly$jday)
ggplot(jday.bind, aes(x, predicted)) + 
  geom_ribbon(data=jday,aes(ymin=conf.low, ymax=conf.high), alpha=0.15, 
              fill="#2f4a8a")+
  geom_ribbon(data=jday1,aes(ymin=conf.low, ymax=conf.high), alpha=0.15, 
              fill="#ffb172")+
  geom_smooth(aes(col=species))+
  scale_color_manual(values = c("#4869b1","#ffb172"), name = "Species",
                     labels=c("Murrelet","Raven"))+
  labs(x="Day of the year", y="Probability of detection")+
  theme(legend.background = element_rect(fill="white"))+
  guides(color=guide_legend(override.aes=list(fill=NA)))
ggsave("figures/pred.cam.bind.jday.png",width=1175,height=749,units="px",dpi=300)

##START NEW PLOTTING CODE## 
#get label vector for real-scale x-axis 
min.x <- round(min(camera_hourly$jday),dig=-1)
max.x <- round(max(camera_hourly$jday),dig=-1)
int.length <- (max.x-min.x)/5
x.axis.real <- seq(from=min.x,to=max.x,by=int.length)
x.axis <- (x.axis.real - mean(camera_hourly$jday))/sd(camera_hourly$jday) 
#plot
jday1 <- ggpredict(m2.5, terms = "jday.stand [all]")
jday1$species<-"Raven"
jday$species<-"Murrelet"
jday.bind<-bind_rows(jday1,jday)
jday.bind$jday<-jday.bind$x*sd(camera_hourly$jday)+mean(camera_hourly$jday)
ggplot(jday.bind, aes(x, predicted)) + 
        geom_ribbon(data=jday,aes(ymin=conf.low, ymax=conf.high), alpha=0.15, 
                    fill="#2f4a8a")+
        geom_ribbon(data=jday1,aes(ymin=conf.low, ymax=conf.high), alpha=0.15, 
                    fill="#ffb172")+
        geom_smooth(aes(col=species))+
        scale_color_manual(values = c("#4869b1","#ffb172"), name = "Species",
                           labels=c("Murrelet","Raven"))+
        labs(x="Day of the year", y="Probability of detection")+
        theme(legend.background = element_rect(fill="white"))+
        guides(color=guide_legend(override.aes=list(fill=NA)))+
        scale_x_continuous(breaks = x.axis, labels = as.character(x.axis.real)) 
ggsave("figures/pred.cam.bind.jday.png",width=1175,height=749,units="px",dpi=300)
##END NEW PLOTTING CODE## 

tod1 <- ggpredict(m2.5, terms = "tod")
tod$species<-"Murrelet"
tod1$species<-"Raven"
tod.bind<-bind_rows(tod,tod1)
ggplot(tod.bind, aes(x, predicted, col=species)) + 
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high),position = position_dodge(width = 0.3))+
  labs(x="Time of day", y="Probability of detection")+
  scale_x_discrete(labels=c("Dawn","Day","Dusk","Night"))+
  scale_color_manual(values = c("#4869b1","#ffb172"), name = "Species",
                     labels=c("Murrelet","Raven"))+
  theme(legend.key = element_rect(fill="white"))
ggsave("figures/pred.cam.bind.tod.png",width=1175,height=749,units="px",dpi=300)

#acoustic
time_dif <- ggpredict(m5, terms = "time_int")
time_dif$time = with(time_dif, reorder(x,predicted))
time_dif%>%
  arrange(predicted)%>%
  mutate(x = factor(x, levels=c("after","aduring","before")))

ggplot(time_dif, aes(x, predicted)) + 
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high),color="#2f4a8a")+
  labs(x="Image type", y="Probability of \n murrelet detection")+
  scale_x_discrete(limits=c("after","aduring","before"),labels=c("Before","During","After"))
ggsave("figures/pred.cam.timediff.png",width=1175,height=749,units="px",dpi=300)

jday2 <- ggpredict(m5, terms = "jday.stand [all]")
ggplot(jday2, aes(x, predicted)) + 
  geom_smooth(color="#2f4a8a")+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.15, fill="#2f4a8a")+
  labs(x="Day of the year", y="Probability of \n murrelet detection")
ggsave("figures/pred.acu.jday.png",width=1175,height=749,units="px",dpi=300)


##START NEW PLOTTING CODE## 
#get label vector for real-scale x-axis 
min.x <- round(min(acoustic$jday),dig=-1)
max.x <- round(max(acoustic$jday),dig=-1)
int.length <- (max.x-min.x)/4
x.axis.real <- seq(from=min.x,to=max.x,by=int.length)
x.axis <- (x.axis.real - mean(camera_hourly$jday))/sd(camera_hourly$jday) 
#plot
jday2 <- ggpredict(m5, terms = "jday.stand [all]")
ggplot(jday2, aes(x, predicted)) + 
        geom_smooth(color="#2f4a8a")+
        geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.15, fill="#2f4a8a")+
        labs(x="Day of the year", y="Probability of \n murrelet detection")+
        scale_x_continuous(breaks = x.axis, labels = as.character(x.axis.real)) 
ggsave("figures/pred.acu.jday.png",width=1175,height=749,units="px",dpi=300)
##END NEW PLOTTING CODE## 

tod1 <- ggpredict(m5, terms = "tod")
ggplot(tod1, aes(x, predicted)) + 
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high),color="#2f4a8a")+
  labs(x="Time of day", y="Probability of \n murrelet detection")+
  scale_x_discrete(labels=c("Dawn","Day","Dusk","Night"))
ggsave("figures/pred.acu.tod.png",width=1175,height=749,units="px",dpi=300)
