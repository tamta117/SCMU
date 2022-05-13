# Efficacy of remote monitoring tools in the detection of predation events on a rare seabird

### TO DO
clean-up acoustic and camera data files; remove unnecessary data  
explain acoustic data process (e.g., 10 minutes on either side)

===================================

This repository contains pre-processed camera trap and acoustic monitoring data at 5 breeding locations for Scripps's murrelets *Synthliboramphus scrippsi* at Anacapa Island in Channel Islands National Park. Units were deployed during the breeding season (~February to June) in 2021 and 2022. 

This repository contains the following files:  

1. Data   
    + the `acoustic_raw_files` folder contains the pre-processed acoustic data in text files by each breeding location. Data is grouped by site and files are split into murrelet and raven positive detections. Acoustic data was processed using Raven Pro software. 
    + the `camera_raw_files` folder contains the pre-processed camera data in csv files by each breeding location. Data files contain one record per image, including detection and non-detection data. Camera data was processed using Timelapse software.  
    + `camera_raw_dat.csv` is the raw version of the camera data; it contains all image records. 
    + `camera_dat_full.csv`is the cleaned version of the camera data; it contains all image records. 
    + `camera_dat_all_images.csv` is a slimmed down version with CORA and SCMU detections only.  
    + `camera_hourly_mod.csv` is the dataset used in the analysis.  
    + `ai_scmu_sites.csv` contains GPS latitude/longitude coordinates for the 5 breeding locations.  

2. Scripts  
    + `acoustic_dat_process.R` is the script to compile, reformat, and clean acoustic data. 
    + `camera_dat_process.R` is the script to compile, reformat, and clean camera trap data. 
    + `models.Rmd` is the model file.  
    + `map_site.R` creates a study site map.  
    + `graph_acoustic_data.R` is used to generate plots using model results for acoustic data.  
    + `graph_camera_data.R` is used to generate plots using model results for camera data.  
    
### Sites
1. Pinnacle Cave, West Anacapa Island (2 cameras, 1 acoustic device)  

2. Moss Cave, West Anacapa Island (2 cameras, 1 acoustic device)   

3. Lava Bench #1 Cave, Middle Anacapa Island (1 camera, 1 acoustic device)  

4. Lava Bench #2 Cave, Middle Anacapa Island (1 camera, 1 acoustic device)    

5. Refuge Cave, Middle Anacapa Island (1 camera, 1 acoustic device)    


### Technical Specifications
Acoustic data was recorded on Wildlife Acoustic Song Meter SM4 units. Units were programmed to record one minute of audio every ten minutes for 24 hours/day.  

Camera data was recorded by Reconyx HyperFire 2 IR units. Units were programmed for motion-activated and timelapse images. Motion-activated settings were programmed to "agressive"" (5 pictures per trigger, RapidFire, 0 sec quiet period between triggers). Timelapse settings were programmed to take one image every 10 minutes. Cameras were programmed to run 24 hours/day. 

