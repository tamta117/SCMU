#determine missing files from processed acoustic txt
library(here)
library(tidyverse)
library(Rraven)

#read in data
lava1_acoustic<-imp_raven("data/acoustic/Lava1",all.data=TRUE)
lava2_acoustic<-imp_raven("data/acoustic/Lava2",all.data=TRUE)
lava2_acoustic$`Begin File`<-gsub("PWR04", "PWR01",
       as.character(lava2_acoustic$`Begin File`))
moss_acoustic<-imp_raven("data/acoustic/Moss",all.data=TRUE)
pinn_acoustic<-imp_raven("data/acoustic/Pinnacle",all.data=TRUE)
ref_acoustic<-imp_raven("data/acoustic/Refuge",all.data=TRUE)

#find unique values
lava1_acoustic<-distinct(lava1_acoustic,`Begin File`)
lava2_acoustic<-distinct(lava2_acoustic,`Begin File`)
moss_acoustic<-distinct(moss_acoustic,`Begin File`)
pinn_acoustic<-distinct(pinn_acoustic,`Begin File`)
ref_acoustic<-distinct(ref_acoustic,`Begin File`)

#anti_join commences
lava1_non_det <- anti_join(lava1_det_all ,lava1_acoustic, by = "Begin File")
lava2_non_det <- anti_join(lava2_det_all ,lava2_acoustic, by = "Begin File")
moss_non_det <- anti_join(moss_det_all ,moss_acoustic, by = "Begin File")
pinn_non_det <- anti_join(pinn_det_all ,pinn_acoustic, by = "Begin File")
ref_non_det <- anti_join(ref_det_all ,ref_acoustic, by = "Begin File")
