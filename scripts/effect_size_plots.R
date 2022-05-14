#predicted plots for glm models

library(jtools)

#plot camera models
apatheme=theme_minimal()+
  theme(panel.border=element_blank(),
        axis.line=element_line(),
        legend.title=element_blank(), 
        axis.text=element_text(size=10),
        axis.title=element_text(size=10))
plot_summs(m1,m2.5, colors=c("#2f4a8a","#ffb172"),
                 coefs=c("Day of \n the year"="jday.stand",
                         "Day"="todday",
                         "Dusk"="toddusk",
                         "Night"="todnight",
                         "Timelapse"="image_typetimelapse"),
                 point.size=2, model.names = c("Murrelet","Raven"))+
  apatheme+labs(x="Effect size",y=NULL)
ggsave("figures/camera.plot.png",width=7.5,height=3.5,units="in",dpi=300)

#plot m1
plot_summs(m1, colors="#2f4a8a",
                        coefs=c("Day of \n the year"="jday.stand",
                                "Day"="todday",
                                "Dusk"="toddusk",
                                "Night"="todnight",
                                "Timelapse"="image_typetimelapse"),
                        point.size=2)+
  apatheme+labs(x="Effect size",y=NULL)
ggsave("figures/cam.murr.plot.png",width=7.5,height=3.5,units="in",dpi=300)

#plot m2.5
plot_summs(m2.5, colors="#ffb172",
           coefs=c("Day of \n the year"="jday.stand",
                   "Day"="todday",
                   "Dusk"="toddusk",
                   "Night"="todnight",
                   "Timelapse"="image_typetimelapse"),
           point.size=2)+
  apatheme+labs(x="Effect size",y=NULL)
ggsave("figures/cam.rav.plot.png",width=7.5,height=3.5,units="in",dpi=300)

#plot acoustic models
plot_summs(m5, colors="#2f4a8a",
                        coefs=c("Day of \n the year"="jday.stand",
                                "Day"="todday",
                                #"Dusk"="toddusk",
                                "Night"="todnight",
                                "Before"="time_intbefore",
                                "After"="time_intafter"))+
  apatheme+labs(x="Effect size",y=NULL)
ggsave("figures/acoustic.dusk.plot.png",width=7.5,height=3.5,units="in",dpi=300)
