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
# 03_FP_outputs.R
# script to load FLEX output files
# written by Joanna Burgar (Joanna.Burgar@gov.bc.ca) - 30-May-2025
#####################################################################################
# Load Packages
list.of.packages <- c("terra","tidyverse","sf", "Cairo", "data.table")

# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
#####################################################################################
# For Quesnel
# Quesnel <- sf::st_read(dsn=here::here(), layer="Quesnel_BEC")
# Quesnel  %>% group_by(ZONE) %>% summarise(sum(Area_sqkm))
# ggplot()+
#   geom_sf(data=Quesnel)
# Quesnel %>% glimpse() %>% st_drop_geometry()
# Quesnel_merged <- Quesnel %>%
#   summarise(geometry = sf::st_union(geometry), .groups = "drop")
# ggplot()+geom_sf(data=Quesnel_merged)
# st_area(Quesnel_merged)*1e-6
# aoi_filtered <- Quesnel_merged

### Area of Interest
DSN <- "//sfp.idir.bcgov/S140/S40203/Ecosystems/Conservation Science/Species/Mesocarnivores/Projects/Enterprise Fisher Telemetry/2. Data/Scat_Surveys"
aoi_layer <- "MF_Diet_StudyArea"

aoi <- sf::st_read(dsn=DSN, layer=aoi_layer)
aoi <- st_zm(aoi)
aoi_merged <- st_union(aoi)

ggplot()+
  geom_sf(data=aoi_merged)

# raster
FLEX_name <- "chilcotin"
FLEX_dir <- "./Output_files/"
list.files(paste0(FLEX_dir, FLEX_name))

FLEX_files <- list.files(paste0(FLEX_dir, FLEX_name))
FLEX_final_fisher_territories <- FLEX_files[grep("*final_fisher_territories", FLEX_files, ignore.case = FALSE)]
# FLEX_final_fisher_territories <- FLEX_files[grep("_(1|2|3|4|5|6|7|8|9|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25)_.*final_fisher_territories", FLEX_files)]

hr_list <- list()
FLEX_list <- list()

for (i in seq_along(FLEX_final_fisher_territories)) {
  # Read raster using terra
  rFLEX <- rast(paste0(FLEX_dir, FLEX_name, "/", FLEX_final_fisher_territories[i]))
  
  # Align AOI CRS to raster CRS
  aoi_merged_aligned <- st_transform(aoi_merged, crs = crs(rFLEX))
  
  # Convert AOI to SpatVector
  aoi_vect <- vect(aoi_merged_aligned)
  
  # Crop and mask
  clipped_rFLEX <- crop(rFLEX, aoi_vect)
  clipped_rFLEX <- mask(clipped_rFLEX, aoi_vect)
  
  # Plot for quick check
  plot(clipped_rFLEX)
  
  # Frequency table
  hr <- as_tibble(freq(clipped_rFLEX))
  hr <- hr %>% filter(value > 0)
  
  # Calculate area per pixel in km²
  pixel_area_km2 <- prod(res(clipped_rFLEX)) / 1e6
  
  # Add HR number, area, and name
  hr$HRNum <- seq_len(nrow(hr))
  hr$Areakm2 <- hr$count * pixel_area_km2
  hr$Name <- paste0(FLEX_name, i)
  
  # Print summary stats
  print(hr %>% summarise(min_Areakm2 = min(Areakm2),
                         mean_Areakm2 = mean(Areakm2),
                         max_Areakm2 = max(Areakm2)))
  
  # Save to lists
  hr_list[[i]] <- hr
  
  # Binarize original rFLEX and save
  rFLEX_bin <- rFLEX
  rFLEX_bin[rFLEX_bin > 0] <- 1
  FLEX_list[[i]] <- rFLEX_bin
}


FLEX_hr <- data.table::rbindlist(hr_list)
FLEX_hr_sum <- FLEX_hr %>% group_by(Name) %>% summarise(mean(Areakm2), sd(Areakm2), min(Areakm2), max(Areakm2))
colnames(FLEX_hr_sum) <- c("FLEX_run","mean_Areakm2", "sd_Areakm2","min_Areakm2","max_Areakm2")
numHR <- FLEX_hr %>% group_by(Name) %>% count(Name) %>% ungroup()  %>% dplyr::select(n)
FLEX_hr_sum$countHR <- numHR$n

FLEX_hr_sum %>% summarise(mean(countHR), min(countHR), max(countHR))
FLEX_hr_sum %>% summarise(mean(mean_Areakm2), min(min_Areakm2), max(max_Areakm2))


# To just get the clipped output (without # of predicted territories)
# Filter to fisher region of interest
FLEXraster <- rast(paste0(getwd(),"/Chilcotin5_stack.tif"))

# Clip and mask raster to AOI
clipped_raster <- crop(FLEXraster, aoi_vect)  # Crop to bounding box
clipped_raster <- mask(clipped_raster, aoi_vect)  # Mask to exact shape

plot(clipped_raster)

writeRaster(clipped_raster, "MF_Cariboo.tif", datatype = "INT4S",overwrite=TRUE) # for large rasters, need to provide data type
writeRaster(clipped_raster, "MF_Chilcotin.tif", datatype = "INT4S",overwrite=TRUE) # for large rasters, need to provide data type
# writeRaster(clipped_raster, "Nazko_Omineca.tif", datatype = "INT4S",overwrite=TRUE) # for large rasters, need to provide data type

#####################################################################################
#####################################################################################

# To refine priority areas within FLP
# Either run through the above code or bring in layers already created

# If needing to bring in raster files
Q_cariboo <- rast(paste0(getwd(),"/Quesnel_Cariboo.tif"))
Q_chilcotin <- rast(paste0(getwd(),"/Quesnel_Chicotin.tif"))

# Check the rasters
print(Q_cariboo)
plot(Q_cariboo)

print(Q_chilcotin)
plot(Q_chilcotin)

# Check values
hist(values(Q_cariboo))
hist(values(Q_chilcotin))


### Now change the values of the rasters to only include the good habitat 
# Adjust values to classes
# Define classification matrix
reclass_matrix_hab <- matrix(c(
  0,  0,     0,  # not good habitat
  1,  5,     1,  # low quality habitat
  5,  10,     2,
  10,  15,     3,
  15,  20,     4,
  20,  25,     5  # high quality habitat
), ncol = 3, byrow = TRUE)


### Reclassify the rasters
### Cariboo portion
Q_cariboo_reclass <- classify(Q_cariboo, rcl = reclass_matrix_hab)
hist(Q_cariboo_reclass)
# exclude non fisher habitat
Q_cariboo_reclass[Q_cariboo_reclass == 0] <- NA
Q_cariboo_reclass[Q_cariboo_reclass == 1] <- NA
Q_cariboo_reclass[Q_cariboo_reclass == 2] <- NA

# Plot to check
plot(Q_cariboo_reclass)


### Chilcotin portion
Q_chilcotin_reclass <- classify(Q_chilcotin, rcl = reclass_matrix_hab)
hist(Q_chilcotin_reclass)
# exclude non fisher habitat
Q_chilcotin_reclass[Q_chilcotin_reclass == 0] <- NA
Q_chilcotin_reclass[Q_chilcotin_reclass == 1] <- NA
Q_chilcotin_reclass[Q_chilcotin_reclass == 2] <- NA

# Plot to check
plot(Q_chilcotin_reclass)

#########################################################
## clip to priority areas
# Bring in vector files
GIS_Dir <- "//sfp.idir.bcgov/S140/S40203/Ecosystems/Conservation Science/Species/Mesocarnivores/Projects/FLM/5. Presentations/Quesnel_FLP/"
# list.files(GIS_Dir) # check it works
Q_NoHarvest <- sf::st_read(dsn = GIS_Dir, layer="Potential_NoHarvest")

ggplot()+
  geom_sf(data = Q_NoHarvest(aes(fill=VOITpot)))

# Create the plot
ggplot(data = Q_NoHarvest) +
  geom_sf(aes(fill = VOITpot), color = "black", size = 0.3) +  # color = border color
  scale_fill_viridis_d(option = "C", begin = 0.2, end = 0.8) 

#########################################################
# Ensure both have the same CRS
aoi <- st_transform(Q_NoHarvest, crs = st_crs(Q_chilcotin_reclass))

# Original buffers (may overlap)
buffers <- st_buffer(Q_NoHarvest, dist = 2000, nQuadSegs = 60)

# Remove overlaps: process one-by-one
buffers_no_overlap <- list()

for (i in seq_len(nrow(buffers))) {
  this_poly <- buffers[i, ]
  
  if (i == 1) {
    # First polygon is added as-is
    buffers_no_overlap[[i]] <- this_poly
  } else {
    # Subtract all previous polygons from the current one
    previous <- do.call(rbind, buffers_no_overlap)
    this_poly$geometry <- st_difference(this_poly$geometry, st_union(previous))
    buffers_no_overlap[[i]] <- this_poly
  }
}

# Combine into a single sf object
buffers_no_overlap_sf <- do.call(rbind, buffers_no_overlap)

# Plot
ggplot() +
  geom_sf(data = buffers_no_overlap_sf, aes(fill = VOITpot), alpha = 0.6, color = "black") +
  geom_sf(data = Q_NoHarvest, fill = NA, color = "grey40", linetype = "dashed") +
  labs(title = "Non-overlapping Buffered Polygons",
       fill = "VOIT Potential")


###################################################
## Bring in rasters of interest
# To refine priority areas within FLP
# Either run through the above code or bring in layers already created

# If needing to bring in raster files
Q_cariboo <- rast(paste0(getwd(),"/Quesnel_Cariboo.tif"))
Q_chilcotin <- rast(paste0(getwd(),"/Quesnel_Chicotin.tif"))

# Check the rasters
print(Q_cariboo)
plot(Q_cariboo)

print(Q_chilcotin)
plot(Q_chilcotin)

# Check values
hist(values(Q_cariboo))
hist(values(Q_chilcotin))

# Filter to fisher region of interest

aoi_filtered <- st_transform(aoi_filtered, crs = st_crs(Q_cariboo_reclass))


# Clip and mask raster to AOI
clipped_raster <- crop(Q_cariboo_reclass, aoi_filtered)  # Crop to bounding box
clipped_raster <- mask(clipped_raster, aoi_filtered)  # Mask to exact shape

plot(clipped_raster)

###--- Export as both rasters and polygon shapefiles
# Define file paths for output
raster_output_path <- "Q_cariboo_reclass.tif"
polygon_output_path <- "Q_cariboo_reclass.shp"

# Export reclassified raster
writeRaster(clipped_raster, raster_output_path, overwrite=TRUE, datatype="INT2S")

# Convert raster to polygons (vectorize)
Q_cariboo_polygon <- as.polygons(clipped_raster, trunc=TRUE, dissolve=TRUE)

# Export as shapefile
writeVector(Q_cariboo_polygon, polygon_output_path, overwrite=TRUE)



