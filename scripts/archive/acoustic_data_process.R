## Create acoustic detection and non-detection acoustic data
## 29 March 2022

## load libraries
library(here)
library(tidyverse)

#### Lava 1 ####
## load data
d1 <- read.csv(here("odata", "directory", "Lava1_210320.txt"), header = FALSE)
d2 <- read.csv(here("odata", "directory", "Lava1_320414.txt"), header = FALSE)
d3 <- read.csv(here("odata", "directory", "Lava1_414523.txt"), header = FALSE)

## combine into one dataframe
lava1 <- bind_rows(d1, d2, d3)
colnames(lava1)

nrow(d1) + nrow(d2) + nrow(d3)


## load detections
#det <- read.csv(here("odata", "lava1_acoustic.csv"))
#colnames(det)
a1 <- read.delim(here("odata", "Lava1_SM4_mur_210320.txt"))
a2 <- read.delim(here("odata", "Lava1_SM4_mur_320414.txt"))
a3 <- read.delim(here("odata", "Lava1_SM4_mur_414523.txt"))
a4 <- read.delim(here("odata", "Lava1_SM4_rav_210320.txt"))
a5 <- read.delim(here("odata", "Lava1_SM4_rav_320414.txt"))
a6 <- read.delim(here("odata", "Lava1_SM4_rav_414523.txt"))

## combine into into one dataframe
lava1_det <- bind_rows(a1, a2, a3, a4, a5, a6) %>%
  mutate(V1 = Begin.File)
nrow(a1) + nrow(a2) + nrow(a3) + nrow(a4) + nrow(a5) + nrow(a6)
nrow(lava1_det)

lava1_det2 <- unique(lava1_det$V1) %>%
  as.data.frame()
colnames(lava1_det2) <- "V1"

## filter out non-detection data
non_det <- anti_join(lava1, lava1_det2, by = "V1")
nrow(lava1) - nrow(lava1_det2)

## create 