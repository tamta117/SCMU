#determine missing files from processed acoustic txt
library(here)
library(tidyverse)
library(Rraven)

#read in data
lava1_acoustic<-imp_raven("data/acoustic/Lava1",all.data=TRUE)

#find unique values
lava1_acoustic<-distinct(lava1_acoustic,`Begin File`)

#anti_join commences
lava1_non_det <- anti_join(lava1_det_all ,lava1_acoustic, by = "Begin File")