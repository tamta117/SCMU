## Script to process camera data for model
## 5 April 2022

## TODO
# fix image set df to detections within 10 min window?

## load libraries
library(tidyverse)
library(here)
library(lubridate)
library(janitor)

## load raw data files from Timelapse
lava1 <-
  list.files(path = here("data", "camera", "Lava1"),
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  dplyr::select(-c("...27")) %>%
  mutate(Site = "Lava1",
         cam_id = c("cam_01")) %>%
  dplyr::select(Site, everything())

lava2 <-
  list.files(path = here("Data", "camera", "Lava2"),
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  dplyr::select(-c("...27")) %>%
  mutate(Site = "Lava2",
         cam_id = c("cam_02")) %>%
  dplyr::select(Site, everything())

moss <-
  list.files(path = here("Data", "camera", "Moss"),
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  dplyr::select(-c("...27")) %>%
  mutate(Site = "Moss",
         cam_id = ifelse(Folder == "Moss Camera 1", "cam_03", "cam_04")) %>%
  dplyr::select(Site, everything())

pinnacle <-
  list.files(path = here("Data", "camera", "Pinnacle"),
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  dplyr::select(-c("...27")) %>%
  mutate(Site = "Pinnacle",
         cam_id = ifelse(Folder == "Pinnacle Camera 1", "cam_05", "cam_06")) %>%
  dplyr::select(Site, everything())

refuge <-
  list.files(path = here("Data", "camera", "Refuge"),
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  dplyr::select(-c("...27")) %>%
  mutate(Site = "Refuge",
         cam_id = c("cam_07")) %>%
  dplyr::select(Site, everything())

## combine into one dataframe, one row per camera observation
camera_raw_dat <- rbind(lava1, lava2, moss, pinnacle, refuge) %>%
  mutate(Date = as_date(Date, format = "%d-%b-%Y"),
         Adult = as.integer(Adult),
         Offspring = as.integer(Offspring),
         Count = as.integer(Count))

## export csv with all camera observations (not cleaned)
# write.csv(camera_raw_dat, here("Data", "camera", "camera_raw_dat.csv"), row.names = FALSE)
nrow(camera_raw_dat) # total observations/attempts

## clean dataframe and reformat
camera_dat_full <- camera_raw_dat %>%
  clean_names() %>% 
  unite(date, time, col = "date_time", sep=" ", remove = FALSE) %>% # combine date and time into timestamp
  mutate(date_time = as_datetime(date_time, format = "%Y-%m-%d %H:%M:%S")) %>% # format timestamp as timestamp
  mutate(ID = 1:nrow(camera_raw_dat),
         year = year(date_time), # pull year from timestamp
         month = month(date_time), # pull month
         day = day(date_time), # pull day
         jday = yday(date_time), # pull julian day
         hour = hour(date_time), # pull hour
         min = minute(date_time), # pull minute
         sec = second(date_time), # pull second
         min2 = substring(min, 2),  
         image_type = ifelse(min == 0 & sec == 00 | min2 == 0 & sec == 00, "timelapse", "motion"), # categorize image as timelapse or motion-activated based on second hand
         int_diff = date_time - lag(date_time), # find interval between images taken
         int_diff_secs = as.numeric(int_diff, units = 'mins'), # compute to seconds
         murrelet_status = ifelse(murrelet == "true" & is.na(comments), "alive", # fix dead murrelets
                                  ifelse(murrelet == "true" & str_detect(comments, "dead", negate = TRUE), "alive",
                                         ifelse(murrelet == "true" & str_detect(comments,"dead"), "dead", NA)))) %>%
  arrange(site, date_time) %>%
  mutate(image_no = paste(year, site, ID, sep = "_"), # create unique image ID
         #SCMU = ifelse(murrelet == "true" & murrelet_status == "alive", 1, 0), # give 1 if SCMU alive detected, otherwise 0
         SCMU = case_when(murrelet == "true" & murrelet_status == "alive" ~ 1, TRUE ~ 0),
         CORA = case_when(species == "Raven" ~ 1, TRUE ~ 0),
         BNOW = case_when(species == "Barn owl" ~ 1, TRUE ~ 0),
         PEEM = case_when(species == "Mouse" ~ 1, TRUE ~ 0),
         predation = case_when(predation_event == "true" ~ 1, TRUE ~ 0),
         detection = case_when(SCMU == 1 ~ 1, # any detection events get a 1 
                               CORA == 1 ~ 1,
                               BNOW == 1 ~ 1, 
                               PEEM == 1 ~ 1,
                               predation == 1 ~ 1, TRUE ~ 0)) %>% 
  #mutate(burst = ifelse(int_diff_secs < 5, TRUE, FALSE)) %>% # filter out images if interval between them is <5 seconds (double-check this)
  dplyr::select(folder, relative_path, file, image_no, site, cam_id, date_time, date, time,
                year, month, day, jday, hour, min, sec, image_type, everything())

camera_dat_full$hour<-as.numeric(camera_dat_full$hour)
dat1<-camera_dat_full%>%
  subset(camera_dat_full$date<="2021-03-15")%>%
  mutate(tod=case_when(
    hour<=6~"dawn",
    hour>=7 & hour<=16 ~ "day",
    hour>=17~"dusk"))
dat2<-camera_dat_full%>%
  subset(camera_dat_full$date>="2021-03-16" & 
           camera_dat_full$date<="2021-05-01")%>%
  mutate(tod=case_when(
    hour<=5~"dawn",
    hour>=6 & hour<=17 ~ "day",
    hour>=18~"dusk"))
dat3<-camera_dat_full%>%
  subset(camera_dat_full$date>="2021-05-02" & 
           camera_dat_full$date<="2021-05-30")%>%
  mutate(tod=case_when(
    hour<=4~"dawn",
    hour>=5 & hour<=17 ~ "day",
    hour>=18~"dusk"))
dat4<-camera_dat_full%>%
  subset(camera_dat_full$date>="2021-05-31")%>%
  mutate(tod=case_when(
    hour<=4~"dawn",
    hour>=5 & hour<=18 ~ "day",
    hour>=19~"dusk"))
dat5<-bind_rows(dat1, dat2, dat3, dat4) %>%
  dplyr::select(folder, relative_path, file, image_no, site, cam_id, date_time, date, time,
                year, month, day, jday, hour, min, sec, tod, everything())

# write.csv(dat5, here("data", "camera", "camera_dat_full.csv"), row.names = FALSE)

## create dataframe for SCMU and CORA with all images 
camera_dat_all_images <- dat5 %>%
  dplyr::select(image_no, site, cam_id, date, time, date_time, year, month, day, jday, hour, min, sec, tod, image_type, detection,
                SCMU, CORA)

# write.csv(camera_dat_all_images, here("data", "camera_dat_all_images.csv"), row.names = FALSE)

## extract independent detections (set at 10 minutes, code from Sarah B. Bassing) for image set
dat <- camera_dat_all_images %>%
  dplyr::select(image_no, site, date_time, image_type, SCMU, CORA) %>%
  pivot_longer(cols = c(SCMU, CORA), names_to = "species", values_to = "detection") %>%
  filter(detection == 1) %>%
  arrange(site, date_time)
caps <- c()
caps[1] <- 1
for (i in 2:nrow(dat)){
  if (dat$site[i-1] != dat$site[i]) caps[i] = i
  if (dat$image_type[i-1] != dat$image_type[i]) caps[i] = i
  else (if (dat$species[i-1] != dat$species[i]) caps[i] = i
        else (if (difftime(dat$date_time[i], dat$date_time[i-1], units = c("mins")) >= 10) caps[i] = i
              else caps[i] = caps[i-1]))
}

caps <- as.factor(caps)

#'  Add new column to larger data set
capdata <- cbind(as.data.frame(dat), caps)

#'  Retain only the first image from each unique detection event
detect <- capdata %>%
  group_by(caps) %>%
  slice(1L) %>%
  ungroup() %>%
  dplyr::select(-c(caps)) %>%
  group_by(image_no, site, date_time) %>%
  pivot_wider(names_from = species, values_from = detection)

## create new dataframe with detection info
camera_dat_image_set <- camera_dat_all_images %>%
  dplyr::select(-c(detection, SCMU, CORA)) %>%
  full_join(detect, by = c("image_no", "site", "date_time", "image_type")) %>%
  replace(is.na(.), 0) # replace NA values with 0

## export csv
# write.csv(camera_dat_image_set, here("data", "camera_dat_image_set.csv"), row.names = FALSE)

## create hourly detection data
hourly_det <- camera_dat_image_set %>%
  dplyr::select(site, cam_id, date, jday, hour, tod, image_type, SCMU, CORA) %>%
  group_by(site, cam_id, date, hour, image_type) %>%
  mutate(SCMU_hourly_det = ifelse(any(SCMU == 1), 1, 0),
         CORA_hourly_det = ifelse(any(CORA == 1), 1, 0)) %>%
  group_by(site, cam_id, date, hour, tod, image_type) %>%
  slice(1L) %>%
  dplyr::select(site, cam_id, date, jday, hour, tod, image_type, SCMU_hourly_det, CORA_hourly_det)

## export csv
# write.csv(hourly_det, here("data", "camera_hourly_det.csv"), row.names = FALSE)
