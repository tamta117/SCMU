## AI SCMU Predation - Camera Analysis
## Amelia DuVall ajduvall@uw.edu  
## 29 Sept 2021

# TODO
# data has duplicates?

## load libraries
library(here)
library(tidyverse)
library(lubridate)

## load data
lava1 <-
  list.files(path = here("Data", "camera", "Lava1"),
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  dplyr::select(-c("...27")) %>%
  mutate(Site = "Lava1") %>%
  dplyr::select(Site, everything())

lava2 <-
  list.files(path = here("Data", "camera", "Lava2"),
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  dplyr::select(-c("...27")) %>%
  mutate(Site = "Lava2") %>%
  dplyr::select(Site, everything())

moss <-
  list.files(path = here("Data", "camera", "Moss"),
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  dplyr::select(-c("...27")) %>%
  mutate(Site = "Moss") %>%
  dplyr::select(Site, everything())

pinnacle <-
  list.files(path = here("Data", "camera", "Pinnacle"),
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  dplyr::select(-c("...27")) %>%
  mutate(Site = "Pinnacle") %>%
  dplyr::select(Site, everything())

refuge <-
  list.files(path = here("Data", "camera", "Refuge"),
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  dplyr::select(-c("...27")) %>%
  mutate(Site = "Refuge") %>%
  dplyr::select(Site, everything())

## combine all
camera_dat <- rbind(lava1, lava2, moss, pinnacle, refuge) %>%
  mutate(Date = as_date(Date, format = "%d-%b-%Y"),
         # Time = as_datetime(Time, format = "%H:%M:%S"),
         # Time = as_hms(Time),
         Adult = as.integer(Adult),
         Offspring = as.integer(Offspring),
         Count = as.integer(Count))
write.csv(camera_dat, here("Data", "camera", "camera_dat.csv"), row.names = FALSE)

## create tables
SCMUtbl <- camera_dat %>%
  group_by(Site) %>%
  filter(Murrelet == "true") %>%
  summarize(NoAdults = sum(Adult, na.rm = TRUE),
            NoOffspring = sum(Offspring, na.rm = TRUE),
            TotCount = sum(Count, na.rm = TRUE))
write.csv(SCMUtbl, here("Data", "camera", "SCMU_tbl.csv"), row.names = FALSE)

predtbl <- camera_dat %>%
  filter(PredationEvent == "true") %>%
  group_by(Site) %>%
  summarize(NoPredationEvent = n())
write.csv(predtbl, here("Data", "camera", "pred_tbl.csv"), row.names = FALSE)

spptbl <- table(camera_dat$Species, useNA = "always")
write.csv(spptbl, here("Data", "camera", "spp_tbl.csv"), row.names = FALSE)


