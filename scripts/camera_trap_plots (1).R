## AI SCMU Camera Trap Plots
## Tam Ta
## Updated by Amelia DuVall 7 Oct 2021

#### load libraries and clean data ####
library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyr)
library(here)
library(janitor)

## we could also look at the amount of time ravens spend at each site

# read in csv
# camera_org<-read.csv("C:\\Users\\Jelly Oon\\Documents\\Seabird\\camera_dat.csv")

## another way to read in data using here() function
camera_raw <- read.csv(here("odata/camera_dat.csv")) 
  
camera_org <- camera_raw %>%
  clean_names() %>% # turn column headers into different format
  unite(date, time, col = "date_time", sep=" ", remove = FALSE) %>% # combine date and time into timestamp
  mutate(date_time = as_datetime(date_time, format = "%Y-%m-%d %H:%M:%S")) %>% # format timestamp as timestamp
  mutate(ID = 1:nrow(camera_raw),
         year = year(date_time), # pull year from timestamp
         month = month(date_time), # pull month
         day = day(date_time), # pull day
         jday = yday(date_time), # pull julian day
         hour = hour(date_time), # pull hour
         min = minute(date_time), # pull minute
         sec = second(date_time), # pull second
         image_type = ifelse(sec == 00, "timelapse", "motion"), # categorize image as timelapse or motion-activated
         int_diff = date_time - lag(date_time), # find interval between images taken
         int_diff_secs = as.numeric(int_diff, units = 'mins')) %>% # compute to seconds
  arrange(site, date_time) %>%
  mutate(image_no = paste(year,site, ID, sep = "_"),) %>%
  #mutate(burst = ifelse(int_diff_secs < 5, TRUE, FALSE)) %>% # filter out images if interval between them is <5 seconds (double-check this)
  dplyr::select(folder, relative_path, file, image_no, site, date_time, date, time,
                           year, month, day, jday, hour, min, sec, image_type, everything())

#### summary tables ####
table(camera_org$image_type)
table(camera_org$image_quality)
table(camera_org$month)
table(camera_org$hour)
table(camera_org$animal)
table(camera_org$murrelet)
table(camera_org$adult)
table(camera_org$species)
table(camera_org$count)
table(camera_org$predation_event)

## export reformatted dataset to load in later
# write.csv(camera_org, here("Data", "camera", "camera_dat_clean.csv"), row.names = FALSE)

pred <- camera_org %>%
  filter(!is.na(species)) # take out rows where Species is blank

murr <- camera_org %>%
  filter(murrelet == "true") # only include rows where murrelet is present

## extract independent detections (set at 10 minutes, code from Sarah B. Bassing)
dat <- arrange(pred, site, date_time)
caps <- c()
caps[1] <- 1
for (i in 2:nrow(dat)){
  if (dat$site[i-1] != dat$site[i]) caps[i] = i
  else (if (dat$species[i-1] != dat$species[i]) caps[i] = i
        else (if (difftime(dat$date_time[i], dat$date_time[i-1], units = c("mins")) > 10) caps[i] = i
              else caps[i] = caps[i-1]))
}

caps <- as.factor(caps)

#'  Add new column to larger data set
capdata <- cbind(as.data.frame(dat), caps)

#'  Retain only the first image from each unique detection event
pred_detect <- capdata %>%
  group_by(caps) %>%
  slice(1L) %>%
  ungroup()

## re-run for murr
dat <- arrange(murr, site, date_time)
caps <- c()
caps[1] <- 1
for (i in 2:nrow(dat)){
  if (dat$site[i-1] != dat$site[i]) caps[i] = i
  #else (if (dat$species[i-1] != dat$species[i]) caps[i] = i
        else (if (difftime(dat$date_time[i], dat$date_time[i-1], units = c("mins")) > 10) caps[i] = i
              else caps[i] = caps[i-1])
}

caps <- as.factor(caps)

#'  Add new column to larger data set
capdata <- cbind(as.data.frame(dat), caps)

#'  Retain only the first image from each unique detection event
murr_detect <- capdata %>%
  group_by(caps) %>%
  slice(1L) %>%
  ungroup()

## export out
#write.csv(pred_detect, here("Data", "camera", "pred_detect.csv"), row.names = FALSE)
#write.csv(murr_detect, here("Data", "camera", "murr_detect.csv"), row.names = FALSE)


#### raven and murrelet visitation ####
# df for murrelets only by hour
camera_murr <- murr_detect %>%
  mutate(carcass=case_when(
    comments=="dead"~"true",
    predation_event=="true"~"true",
    predation_event=="false"~"false"))%>%
  group_by(hour,carcass) %>%
  summarize(murrelet = sum(adult))

# df for ravens only by hour
camera_rav <- pred_detect %>%
  filter(species == "Raven") %>%
  group_by(hour)%>%
  summarize(raven = sum(count))

# table for joined graph of raven and murrelet
camera_join <- left_join(camera_murr, camera_rav, by = "hour")
camera_join$murrelet[camera_join$carcass == "true"] <- 0
camera_join1 <- pivot_longer(camera_join, cols=c('raven','murrelet'), names_to='species', values_to="count")

# plot murrelet per hr
(murr_plot <- ggplot(camera_murr))+
  geom_bar(aes(x=hour,y=murrelet,fill=carcass),
           stat="identity",position=position_dodge(0.9))+
  xlab("Time of day")+
  ylab("Number of Murrelets Observed")+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank())+
  scale_fill_manual(values = c("#39638b", "#26a185"),name="Carcass")

ggsave("figures/murr_pred_plot.png", dpi=300)

# plot raven per hour
(raven_plot<-ggplot(camera_rav))+
  geom_bar(aes(x=hour,y=raven),
           stat="identity",position=position_dodge(0.9))+
  xlab("Time of day")+
  ylab("Number of Ravens Observed")+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank())

ggsave("figures/raven_plot.jpeg", dpi=300)
#ggsave("figures/raven_pred_plot.jpeg", dpi=300)

# plot for joined raven and murrelet per hr
(join_plot<-ggplot(camera_join1))+
  geom_bar(aes(x=hour,y=count, fill=species),
           stat="identity",position="dodge")+
  xlab("Time of day")+
  ylab("Count")+
  ggtitle("Murrelet and Raven Visitation - 2021") +
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank())

ggsave("figures/rav_murr_car_plot.png",dpi=300)
#ggsave("figures/rav_murr_plot.jpeg", dpi=300)

## plot for murrelet and raven visitation by site
# df for murrelets by hour and site
camera_murr2 <- murr_detect %>%
  group_by(site, hour) %>%
  summarize(murrelet = sum(adult))

# df for ravens by hour and site
camera_rav2 <- pred_detect %>%
  filter(species == "Raven") %>%
  group_by(site, hour)%>%
  summarize(raven = sum(count))

# table for joined graph of raven and murrelet
camera_join2 <- left_join(camera_murr2, camera_rav2, by = c("hour", "site"))
camera_join3 <- pivot_longer(camera_join2, cols=c('raven','murrelet'), names_to='species', values_to="count")

ggplot(camera_join3)+
  geom_bar(aes(x=hour,y=count, fill=species),
           stat="identity",position="dodge")+
  xlab("Time of day")+
  ylab("Count")+
  facet_wrap(~site, nrow = 1) +
  ggtitle("Murrelet and Raven Visitation - 2021") +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"),
        panel.grid = element_blank(),
        legend.position = "bottom")

ggsave(width = 10, height = 4, here("figures", "murr_rav_by_site.png"))

#### mice visitation ####
#table and plot for mice
camera_mic <- pred_detect %>%
  filter(species == "Mouse")%>%
  group_by(hour)%>%
  summarize(mice = sum(count))

(mice_plot<-ggplot(camera_mic))+
  geom_bar(aes(x=hour,y=mice),
           stat="identity",position=position_dodge(0.9), fill = "#39638b")+
  #scale_x_continuous(expand = c(0, 0), limits = c(0, 24))+
  xlab("Time of day")+
  ylab("Number of Mice Observed")+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank())

 ggsave(here("figures", "mice_plot.png"))
#ggsave("figures/mice_plot.jpeg", dpi=300)

#### barn owl visitation ####
camera_owl <- pred_detect %>%
  filter(species=="Barn owl")%>%
  group_by(hour)%>%
  summarize(owl=sum(count))

(owl_plot<-ggplot(camera_owl))+
  geom_bar(aes(x=hour,y=owl),
           stat="identity",position=position_dodge(0.9), fill = "#39638b")+
  #scale_x_continuous(expand = c(0, 0), limits = c(0, 24))+
  xlab("Time of day")+
  ylab("Number of Barn owls Observed")+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank())

ggsave(here("figures", "owl_plot.png"))
# ggsave("figures/owl_plot.jpeg", dpi=300)

#### all species visitation ####
join1 <- left_join(camera_join, camera_mic, by = "hour")
join2 <- left_join(join1, camera_owl, by = "hour")
all_species <- pivot_longer(join2, cols=c('raven','murrelet','mice', 'owl'), names_to='species', values_to="count")

ggplot(all_species) +
  geom_bar(aes(x=hour, y=count, fill=species),
           stat="identity", position="dodge")+
  facet_wrap(~species) +
  xlab("Time of day")+
  ylab("Count")+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank())

ggsave(here("figures", "all_species.png"))

#### detection by time lapse versus motion ####
murr_img_type <- murr_detect %>%
  group_by(image_type) %>%
  summarize(murrelet = sum(adult))

murr_img_type_car <- murr_detect %>%
  mutate(carcass=case_when(
    comments=="dead"~"true",
    predation_event=="true"~"true",
    predation_event=="false"~"false"))%>%
  group_by(image_type,carcass) %>%
  summarize(murrelet = sum(adult))

rav_img_type <- pred_detect %>%
  filter(species == "Raven") %>%
  group_by(image_type) %>%
  summarize(raven = sum(count))

mice_img_type <- pred_detect %>%
  filter(species == "Mouse") %>%
  group_by(image_type) %>%
  summarize(mice = sum(count))

owl_img_type <- pred_detect %>%
  filter(species == "Barn owl") %>%
  group_by(image_type) %>%
  summarize(owl = sum(count))

all_detect <- list(murr_img_type, rav_img_type, mice_img_type, owl_img_type) %>%
  reduce(full_join, by = "image_type") %>%
  pivot_longer(cols=c('raven','murrelet','mice', 'owl'), names_to='species', values_to="count")

ggplot(data = all_detect) +
  geom_bar(aes(x=species, y=count, fill=image_type),
           stat="identity", position="dodge")+
  #facet_wrap(~image_type) +
  ggtitle("Detection Type by Species") +
  ylab("Count")+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank())

ggplot(data = murr_img_type_car) +
  geom_bar(aes(x=carcass, y=murrelet, fill=image_type),
           stat="identity", position="dodge")+
  #facet_wrap(~image_type) +
  ggtitle("Murrelet Detection Type by Carcass") +
  ylab("Count")+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank())

ggsave("figures/murr_img_car.png",dpi=300)

#### predation events ####
pred <- pred_detect %>%
  filter(predation_event == "true") %>%
  group_by(species) %>%
  summarize(count = n())

ggplot(data = pred) +
  geom_bar(aes(x=species, y=count, fill = species),
           stat="identity", position="dodge")+
  #facet_wrap(~image_type) +
  ggtitle("Predation Events by Species") +
  ylab("Count")+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank(),
        legend.position = "none")
           
ggsave(here("Figures", "predation_by_species.png"))

pred_img_type <- pred_detect %>%
  filter(predation_event == "true") %>%
  group_by(image_type) %>%
  summarize(count = n())

#plot for all visitation events
all_species <- pivot_longer(join2, cols=c('raven','murrelet','mice', 'owl'), names_to='species', values_to="count")

pred_detect2 <- pred_detect%>%
  filter(predation_event=="false")%>%
  #filter(species!="Unknown")%>%
  group_by(species)%>%
  summarize (count = n())

ggplot(data = pred_detect2) +
  geom_bar(aes(x=species, y=count),
           stat="identity", position="dodge")+
  #facet_wrap(~image_type) +
  ggtitle("Visitation events by species") +
  ylab("Count")+
  xlab("Species")+
  scale_x_discrete(limits = c("Mouse","Other","Raven","Unknown","Barn owl"))+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank(),
        legend.position = "none")

ggsave("figures/all_visitation_nopred.png",dpi=300)

#plot for all predation events
ggplot(data = pred) +
  geom_bar(aes(x=species, y=count, fill = species),
           stat="identity", position="dodge")+
  #facet_wrap(~image_type) +
  ggtitle("Predation Events by Species") +
  ylab("Count")+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_blank(),
        legend.position = "none")

ggsave(here("Figures", "predation_by_species.png"))

## look at unknown and other species images
unk <- pred_detect %>%
  filter(species == "Unknown")
#write.csv(unk, here("Data", "camera", "unknown_species.csv"), row.names = FALSE)

other <- pred_detect %>%
  filter(species == "Other")
#write.csv(other, here("Data", "camera", "other_species.csv"), row.names = FALSE)

## look at predation events by time of day
pred_hr <- pred_detect %>%
  filter(predation_event == "true") %>%
  group_by(site, hour, species) %>%
  summarize(count = n())

ggplot(data = pred_hr) +
  geom_bar(aes(x=hour, y=count, fill = species),
           stat="identity", position="dodge")+
  facet_wrap(~site, nrow = 1) +
  ggtitle("Predation Events by Species") +
  ylab("Count")+ xlab("Time of Day") +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"),
        panel.grid = element_blank(),
        legend.position = "bottom")

ggsave(width = 10, height = 4, here("Figures", "pred_by_hr_site_species.png"))
