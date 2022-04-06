## AI detection model
## A DuVall ajduvall@uw.edu 
## 15 March 2022

## load libraries
library(here)
library(tidyverse)
library(ggplot2)
library(lme4)

## load data
#audio <- read.csv(here("data", "acoustic_dat.csv"))
cam <- read.csv(here("data", "camera", "camera_dat.csv"))

## clean the audio data
# head(audio)
# colnames(audio)
# audio2 <- audio %>%
#   rename(time = begin_time_s) %>% # fix
#   mutate(det_type = c("acoustic"),
#          species = ifelse(species == "M", "murrelet",
#                           ifelse(species == "O", "other",
#                                  ifelse(species == "R", "raven", "unknown"))),
#          jday = c(NA),
#          hour = c(NA)) %>% # julian day
#   dplyr::select(year, jday, site, det_type, hour, species) 
# head(audio2)

## clean the camera data
#head(cam)
#colnames(cam)
# cam2 <- cam %>%
#   mutate(det_type = ifelse(image_type == "motion", "mot-cam", "tl-cam")) %>%
#   dplyr::select(year, jday, site, det_type, hour, species) 

## reformat murrelet camera detection data
murr2 <- murr_cam %>%
  mutate(det_type = ifelse(image_type == "motion", "mot-cam", "tl-cam"),
         species = c("murrelet")) %>%
  dplyr::select(year, jday, site, det_type, hour, species)

## reformat raven camera detection data
pred2 <- pred_cam %>%
  mutate(det_type = ifelse(image_type == "motion", "mot-cam", "tl-cam"),
         species = ifelse(species == "Barn owl", "owl",
                           ifelse(species == "Mouse", "mouse",
                                  ifelse(species == "Other", "other",
                                         ifelse(species == "Raven", "raven", "unknown"))))) %>%
  dplyr::select(year, jday, site, det_type, hour, species)

## combine murrelet and predator camera detection data
cam <- bind_rows(murr2, pred2)
head(cam)
colnames(cam)

## combine into one dataframe of all encounters
df <- bind_rows(audio2, cam)

## examine data
colnames(df)
table(df$year)
table(df$det_type, useNA = "always")
table(df$species, useNA = "always")

## create raven dataframe
rav <- df %>% 
  filter(species == "raven")

## raven detections as a funtion of det_type
m1 <- lm(detection ~ det_type + jday + hour + site, data = df, family = binomial)
m2 <- lm(detection ~ det_type)
m3 <- lm(detection ~ site + jday)

library(cars)
