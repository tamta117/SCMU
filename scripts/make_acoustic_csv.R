camera_org<-read.csv("odata\\camera_dat.csv")
camera_acoustic = subset(camera_org, select=c(-ImageQuality, -DeleteFlag, -CameraLocation,
                      -StartDate, -TechnicianName, -Service, -Empty, 
                      -Human, -HumanActivity, -Tags, -GoodPicture,
                      -Folder))
camera_acoustic=subset(camera_acoustic, Animal=="true")
camera_lava1=subset(camera_acoustic, Site=="Lava1")
camera_lava2=subset(camera_acoustic, Site=="Lava2")
camera_moss=subset(camera_acoustic, Site=="Moss")
camera_pinnacle=subset(camera_acoustic, Site=="Pinnacle")
camera_refuge=subset(camera_acoustic, Site=="Refuge")

write.csv(camera_lava1,"odata\\lava1_acoustic.csv", row.names=FALSE)
write.csv(camera_lava2,"odata\\lava2_acoustic.csv", row.names=FALSE)
write.csv(camera_moss,"odata\\moss_acoustic.csv", row.names=FALSE)
write.csv(camera_pinnacle,"odata\\pinnacle_acoustic.csv", row.names=FALSE)
write.csv(camera_refuge,"odata\\refuge_acoustic.csv", row.names=FALSE)
