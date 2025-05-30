# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.
#
#####################################################################################
# 01_load.R
# script to load FLEX output files
# written by Joanna Burgar (Joanna.Burgar@gov.bc.ca) - 06-Nov-2024
#####################################################################################
R_version <- paste0("R-",version$major,".",version$minor)
.libPaths(paste0("C:/Program Files/R/",R_version,"/library")) # to ensure reading/writing libraries from C drive

# Load Packages
list.of.packages <- c("tidyverse","sf", "raster","fasterize","Cairo")

# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
#####################################################################################

Boreal_names <- c("Boreal_Cassiar","Boreal_DC", "Boreal_FN","Boreal_FSJ","Boreal_Mackenzie")
Columbian_names <- c("Cariboo","Chilcotin","Omineca")

FLEX_name <- "Boreal5"
FLEX_dir <- "./Output_files/"
list.files(paste0(FLEX_dir, FLEX_name))
list.files(getwd())

FLEX_files <- list.files(paste0(FLEX_dir, FLEX_name))
FLEX_final_fisher_territories <- FLEX_files[grep("*final_fisher_territories", FLEX_files, ignore.case = FALSE)]
# FLEX_final_fisher_territories <- FLEX_files[grep("_(1|2|3|4|5|6|7|8|9|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25)_.*final_fisher_territories", FLEX_files)]

FLEX_list=list()
hr_list = list()

for(i in 1:length(FLEX_final_fisher_territories)){
  # i=1
  rFLEX <- raster(paste0(FLEX_dir, FLEX_name, "/", FLEX_final_fisher_territories[i]))

  plot(rFLEX)
  
  # create hr table to summarise number and size of hr per output
  hr <- as_tibble(freq(rFLEX))
  hr <- hr %>% dplyr::filter(value>0)
  hr$HRNum <- rownames(hr)
  hr$Areakm2 <- hr$count * 0.01 #convert from ha to km2
  hr$Name <- paste0(FLEX_name,i)
  hr %>% summarise(min(Areakm2), mean (Areakm2), max(Areakm2))
  
  hr_list[[i]] <- hr
  
  rFLEX[rFLEX > 0] <- 1
  FLEX_list[[i]] <- rFLEX 
  
  # return(list(hr_list, FLEX_list))

}

FLEX_hr <- data.table::rbindlist(hr_list)
FLEX_hr_sum <- FLEX_hr %>% group_by(Name) %>% summarise(mean(Areakm2), sd(Areakm2), min(Areakm2), max(Areakm2))
colnames(FLEX_hr_sum) <- c("FLEX_run","mean_Areakm2", "sd_Areakm2","min_Areakm2","max_Areakm2")
numHR <- FLEX_hr %>% group_by(Name) %>% count(Name) %>% ungroup()  %>% dplyr::select(n)
FLEX_hr_sum$countHR <- numHR$n

FLEX_hr_sum %>% summarise(mean(countHR), min(countHR), max(countHR))
FLEX_hr_sum %>% summarise(mean(mean_Areakm2), min(min_Areakm2), max(max_Areakm2))

FLEX_stack = stack(FLEX_list)
FLEX_sumstack <- stackApply(FLEX_stack, indices=1, fun=sum)
plot(FLEX_sumstack)
writeRaster(FLEX_sumstack, file=paste0(FLEX_dir, FLEX_name, "/", FLEX_name, "_stack.tif"), bylayer=TRUE, overwrite=TRUE)



################################################################################
# Reclassify values using a custom function
r_new <- calc(FLEX_sumstack, function(x) {
  ifelse(x > 0 & x <= 50, 1,
         ifelse(x > 50, 2, 0)
  )
})
plot(r_new)
values(r_new)
writeRaster(r_new, file=paste0(FLEX_dir, FLEX_name, "/", FLEX_name, "reclass.tif"), bylayer=TRUE)



# sum(values(FLEX_sumstack)) # 460323 ha predicted fisher habitat in ref scenario; 551820 in nharv_thlb; and 555728 in nharv scenario

# to make binary habitat / no habitat raster layers
FLEX_sumstack[values(FLEX_sumstack)>0] <- 1

writeRaster(FLEX_sumstack, file=paste0(FLEX_dir, FLEX_name, "/", FLEX_name, "_VRI2021_binary.tif"), bylayer=TRUE)

Cairo(file=paste0(FLEX_dir, FLEX_name, "/", FLEX_name, "_VRI2021_binary.png"), type="png", width=1800, height=2200,pointsize=15,bg="white",dpi=300)
plot(FLEX_sumstack)
dev.off()
