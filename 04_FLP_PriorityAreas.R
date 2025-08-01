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

rm(Q_tsa)
# now use the TSA boundary as the aoi for all other queries
aoi <- Q_tsa_dissolved %>% st_transform(3005)

#### Areas to co-locate (i.e., other constraints on landbase)
### OGMAs
# bcdc_search("OGMA", res_format = "wms")
# 1: Old Growth Management Areas - Legal - Current (multiple, wms, kml, csv)
# ID: 1b30f3bd-0ad0-4128-916b-66c6dd91dea4
# Name: old-growth-management-areas-legal-current
aoi.OGMA <- retrieve_geodata_aoi(ID = "1b30f3bd-0ad0-4128-916b-66c6dd91dea4")

ggplot()+
  geom_sf(data = aoi)+
  geom_sf(data = aoi.OGMA)


### WHAs
bcdc_search("WHA", res_format = "wms")
# 2: Wildlife Habitat Areas - Approved (multiple, wms, kml)
# ID: b19ff409-ef71-4476-924e-b3bcf26a0127
# Name: wildlife-habitat-areas-approved
aoi.WHA <- retrieve_geodata_aoi(ID = "b19ff409-ef71-4476-924e-b3bcf26a0127")

ggplot()+
  geom_sf(data = aoi)+
  geom_sf(data = aoi.WHA)

### UWR
bcdc_search("UWR", res_format = "wms")
# 2: Ungulate Winter Range - Approved (multiple, wms, kml)
# ID: 712bd887-7763-4ed3-be46-cdaca5640cc1
# Name: ungulate-winter-range-approved
aoi.UWR <- retrieve_geodata_aoi(ID = "712bd887-7763-4ed3-be46-cdaca5640cc1")

ggplot()+
  geom_sf(data = aoi)+
  geom_sf(data = aoi.UWR)

### Old growth deferral
bcdc_search("deferral", res_format = "wms")
# 2: Old Growth Technical Advisory Panel (TAP) - Priority Deferral Areas - Current
# View (multiple, other, wms, kml, arcgis_rest)
# ID: f257ca4a-0c33-4eb2-9da8-21dff4482f58
# Name:
aoi.OGD <- retrieve_geodata_aoi(ID = "f257ca4a-0c33-4eb2-9da8-21dff4482f58")

ggplot()+
  geom_sf(data = aoi)+
  geom_sf(data = aoi.OGD)

### Provincial Parks
bcdc_search("protected areas", res_format = "wms")
# 8: BC Parks, Ecological Reserves, and Protected Areas (wms, kml, multiple)
# ID: 1130248f-f1a3-4956-8b2e-38d29d3e4af7
# Name: bc-parks-ecological-reserves-and-protected-areas
aoi.PA <- retrieve_geodata_aoi(ID = "1130248f-f1a3-4956-8b2e-38d29d3e4af7")

ggplot()+
  geom_sf(data = aoi)+
  geom_sf(data = aoi.PA)

### save co-location sf objects in one place
all_colocate <- list(
  OGMA = aoi.OGMA,
  WHA = aoi.WHA,
  UWR = aoi.UWR,
  OGD = aoi.OGD,
  PA = aoi.PA
)

# Save as an .RDS file
saveRDS(all_colocate, "Q_colocate.rds")

#### Areas to remove (i.e., challenging to conserve)
### burn severity
bcdc_search("burn severity", res_format = "wms")
# 1: Fire Burn Severity - Historical (wms, kml, multiple)
# ID: c58a54e5-76b7-4921-94a7-b5998484e697
# Name: fire-burn-severity-historical
# 2: Fire Burn Severity - Same Year (multiple, wms, kml, fgdb)
# ID: 04c5ad28-d8eb-4c49-90c5-48b9b98fdfe9
# Name: fire-burn-severity-same-year
aoi.burn2024 <- retrieve_geodata_aoi(ID = "04c5ad28-d8eb-4c49-90c5-48b9b98fdfe9")

ggplot()+
  geom_sf(data = aoi)+
  geom_sf(data = aoi.burn2024)

aoi.burnhist <- retrieve_geodata_aoi(ID = "c58a54e5-76b7-4921-94a7-b5998484e697")

ggplot()+
  geom_sf(data = aoi)+
  geom_sf(data = aoi.burnhist)

### save avoid sf objects in one place
all_avoid <- list(
  burn2024 = aoi.burn2024,
  burnhist = aoi.burnhist
)

# Save as an .RDS file
saveRDS(all_avoid, "Q_avoid.rds")


## NEXT STEPS:
# 1. ADD IN THE PARCEL MAP PRIVATE LAND FOR THE PRIORITY AREAS
# 2. REFINE BASED ON AVOIDS (REMOVE ALL)
# 3. KEEP ALL "GOOD HABITAT", ESPECIALLY IN AREAS OR CLOSE TO AREAS WITH COLOCATION
# 4. REVIEW RORY'S NOTES FOR WHERE TO DELINEATE