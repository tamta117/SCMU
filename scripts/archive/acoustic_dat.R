## Process acoustic data
## A DuVall ajduvall@uw.edu
## 15 March 2022

## load libraries
library(here)
library(tidyverse)
library(lubridate)
library(janitor)
library(openxlsx)

## read in acoustic txt files
a1 <- read.csv(here("data", "acoustics", "Lava1_SM4_rav_210320.csv"), header = TRUE) %>%
  mutate(site = "Lava1")
a2 <- read.csv(here("data", "acoustics", "Lava1_SM4_rav_320414.csv"), header = TRUE) %>%
  mutate(site = "Lava1")
a3 <- read.csv(here("data", "acoustics", "Lava1_SM4_rav_414523.csv"), header = TRUE) %>%
  mutate(site = "Lava1")
a4 <- read.csv(here("data", "acoustics", "Lava2_SM4_rav_210319.csv"), header = TRUE) %>%
  mutate(site = "Lava2")
a5 <- read.csv(here("data", "acoustics", "Moss_SM4_rav_210320.csv"), header = TRUE) %>%
  mutate(site = "Moss")
a6 <- read.csv(here("data", "acoustics", "Moss_SM4_rav_320414.csv"), header = TRUE) %>%
  mutate(site = "Moss")
a7 <- read.csv(here("data", "acoustics", "Moss_SM4_rav_414522.csv"), header = TRUE) %>%
  mutate(site = "Moss") 
a8 <- read.csv(here("data", "acoustics", "Moss_SM4_rav_522623.csv"), header = TRUE) %>%
  mutate(site = "Moss") 
a9 <- read.csv(here("data", "acoustics", "Moss_SM4_rav_623707.csv"), header = TRUE) %>%
  mutate(site = "Moss") 
a10 <- read.csv(here("data", "acoustics", "Pinnacle_SM4_rav_210319.csv"), header = TRUE) %>%
  mutate(site = "Pinnacle")  
a11 <- read.csv(here("data", "acoustics", "Pinnacle_SM4_rav_319414.csv"), header = TRUE) %>%
  mutate(site = "Pinnacle")    
a12 <- read.csv(here("data", "acoustics", "Pinnacle_SM4_rav_522623.csv"), header = TRUE) %>%
  mutate(site = "Pinnacle")    

## combine dataframes into one
## need to fix time and date
adf <- bind_rows(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) %>%
  clean_names() %>%
  mutate(year = 2021,
         month = c(NA),
         day = c(NA)) %>%
  dplyr::select(year, month, day, site, begin_time_s, end_time_s, delta_time_s, low_freq_hz, 
                high_freq_hz, species, comments, begin_path, file_offset_s, begin_file)
  
#df$newDate <- as.POSIXct(as.Date(df$begin_time_s,origin="1899-12-30"))
#convertToDateTime(helpData$ExcelNum, origin = "1900-01-01")
#mutate(time = excel_numeric_to_date(as.numeric(as.character("begin_time_s")), date_system = "modern")) 

## export
write.csv(adf, here("data", "acoustic_dat.csv"))

## find non-detection data

