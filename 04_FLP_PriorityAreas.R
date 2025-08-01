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
# 04_FLP_PriorityAreas.R
# script to fine tune FLEX output files for FLP
# written by Joanna Burgar (Joanna.Burgar@gov.bc.ca) - 1-Aug-2025
#####################################################################################
# Load Packages
list.of.packages <- c("terra","tidyverse","sf", "Cairo", "data.table", "bcdata", "units")

# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
#####################################################################################

###--- function to retrieve geodata from BCGW

retrieve_geodata_aoi <- function(ID) {
 
  # Step 1: Get bounding box from AOI
  bbox <- st_bbox(aoi)
  
  # Step 2: Query full dataset and download locally
  message("Downloading and collecting data...")
  full_data <- bcdc_query_geodata(ID) %>%
    collect()
  
  # Step 3: Rough prefilter by bounding box
  message("Cropping to bounding box...")
  cropped_data <- st_crop(full_data, bbox)
  
  # Step 4: Precise intersection with AOI
  message("Clipping to AOI...")
  aoi_data <- st_intersection(cropped_data, aoi)
  
  if (nrow(aoi_data) == 0) {
    warning("No features intersect with the AOI.")
    return(NULL)
  }
  
  # Step 5: Add area column in km²
  aoi_data$Area_km2 <- drop_units(st_area(aoi_data)) * 1e-6
  
  return(aoi_data)
}

#####################################################################################
# For Quesnel Priority Areas (now need refining)

DSN <- "//sfp.idir.bcgov/S140/S40203/Ecosystems/Conservation Science/Species/Mesocarnivores/Projects/FLM/5. Presentations/Quesnel_FLP/"
Q_VOIT_v2 <- sf::st_read(dsn = DSN, layer="Q_VOIT_v2")

ggplot()+
  geom_sf(data=Q_VOIT_v2)

### Now bring in Quensel raster FLEX outputs
Q_cariboo <- rast(paste0(getwd(),"/Quesnel_Cariboo.tif"))
Q_chilcotin <- rast(paste0(getwd(),"/Quesnel_Chicotin.tif"))

# Check the rasters
print(Q_cariboo)
plot(Q_cariboo)

print(Q_chilcotin)
plot(Q_chilcotin)

# Only keep "good" habitat
# exclude non fisher habitat
Q_cariboo[Q_cariboo < 15] <- NA
Q_chilcotin[Q_chilcotin < 15] <- NA

#####################################################################################
### Now load layers from BC Data warehouse
# load covariates from bcdata
# using the bc data warehouse option to clip to aoi
# first find the Quesnel TSA
# bcdc_search("TSA", res_format = "wms")
# 1: FADM - Timber Supply Area (TSA) (multiple, wms, kml, oracle_sde)
# ID: 8daa29da-d7f4-401c-83ae-d962e3a28980
# Name: fadm-timber-supply-area-tsa
Q_tsa <- bcdc_query_geodata("8daa29da-d7f4-401c-83ae-d962e3a28980") %>%
  filter(TSA_NUMBER_DESCRIPTION=="Quesnel TSA") %>%
  collect()

Q_tsa_dissolved <- Q_tsa %>%
  st_union() %>%           # merge all geometries
  st_make_valid() %>%      # fix any topology issues just in case
  st_sf()  

ggplot()+
  geom_sf(data=Q_tsa_dissolved)
  
# now use the TSA boundary as the aoi for all other queries
aoi <- Q_tsa_dissolved %>% st_transform(3005)

### OGMAs
# 1: Old Growth Management Areas - Legal - Current (multiple, wms, kml, csv)
# ID: 1b30f3bd-0ad0-4128-916b-66c6dd91dea4
# Name: old-growth-management-areas-legal-current
aoi.OGMA <- retrieve_geodata_aoi(ID = "1b30f3bd-0ad0-4128-916b-66c6dd91dea4")

ggplot()+
  geom_sf(data = aoi)+
  geom_sf(data = aoi.OGMA)
