## Create camera detection and non-detection data
## 29 March 2022

## load libraries
library(here)
library(tidyverse)
library(lubridate)

## load file with all camera images
all_cam <- read.csv(here("data", "camera", "camera_dat.csv")) %>%
  mutate(Date = as_date(Date, format = "%Y-%m-%d")) %>%
  unite(col = key, c(Site, File, Date), sep = "_", remove = FALSE)

## load detection data
c1 <- read.csv(here("data", "camera", "lava1_cam.csv")) %?%
c2 <- read.csv(here("data", "camera", "lava2_cam.csv"))
c3 <- read.csv(here("data", "camera", "moss_cam.csv"))
c4 <- read.csv(here("data", "camera", "pinnacle_cam.csv")) %>%
  mutate(Animal = ifelse(Animal == "true", TRUE, FALSE),
         Murrelet = ifelse(Murrelet == "true", TRUE, FALSE),
         Egg = ifelse(Egg == "true", TRUE, FALSE),
         PredationEvent = ifelse(PredationEvent == "true", TRUE, FALSE),
         SecondOpinion = ifelse(SecondOpinion == "true", TRUE, FALSE))
c5 <- read.csv(here("data", "camera", "refuge_cam.csv"))  %>%
  mutate(Animal = ifelse(Animal == "true", TRUE, FALSE),
         Murrelet = ifelse(Murrelet == "true", TRUE, FALSE),
         Egg = ifelse(Egg == "true", TRUE, FALSE),
         PredationEvent = ifelse(PredationEvent == "true", TRUE, FALSE),
         SecondOpinion = ifelse(SecondOpinion == "true", TRUE, FALSE))

## combine camera detections into one file
cam_det <- bind_rows(c1, c2, c3, c4, c5) %>%
  mutate(Date = as_date(Date, format = "%m/%d/%Y")) %>%
  unite(col = key, c(Site, File, Date), sep = "_", remove = FALSE) %>%
  distinct(key, .keep_all = TRUE)
colnames(cam_det)

## look for non-detection data
cam_non_det <- anti_join(dir, cam_det, by = "key")
