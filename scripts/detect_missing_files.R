#determine missing files from processed acoustic txt
library(here)
library(tidyverse)
library(data.table)

#read in data
d1 <- read.delim(here("data", "acoustic", "Lava1_SM4_rav_210320.txt"))
d2 <- read.delim(here("data", "acoustic", "Lava1_SM4_rav_320414.txt"))
d3 <- read.delim(here("data", "acoustic", "Lava1_SM4_rav_414523.txt"))
d4 <- read.delim(here("data", "acoustic", "Lava1_SM4_mur_210320.txt"))
d5 <- read.delim(here("data", "acoustic", "Lava1_SM4_mur_320414.txt"))
d6 <- read.delim(here("data", "acoustic", "Lava1_SM4_mur_414523.txt"))

#combine data
lava1_acoustic <- bind_rows(d1, d2, d3, d4, d5, d6)

#find unique values
lava1_acoustic<-unique(lava1_acoustic$Begin.File)
lava1_acoustic<-as.data.table(lava1_acoustic, TRUE)
colnames(lava1_acoustic)<-'Begin.File'

#anti_join commences
lava1_non_det <- anti_join(lava1_det_all, lava1_acoustic, by = "Begin.File")