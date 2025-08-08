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
# bring in private land within, as shapefile created elsewhere

DSN <- "//sfp.idir.bcgov/S140/S40203/Ecosystems/Conservation Science/Species/Mesocarnivores/Projects/FLM/5. Presentations/Quesnel_FLP/"
Q_VOIT_v2 <- sf::st_read(dsn = DSN, layer="Q_VOIT_v2")
Q_private <- sf::st_read(dsn = DSN, layer="Q_VOIT_privateland")

ggplot()+
  geom_sf(data=Q_VOIT_v2)

ggplot()+
  geom_sf(data=Q_private)


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

# #### Areas to co-locate (i.e., other constraints on landbase)
# ### OGMAs
# # bcdc_search("OGMA", res_format = "wms")
# # 1: Old Growth Management Areas - Legal - Current (multiple, wms, kml, csv)
# # ID: 1b30f3bd-0ad0-4128-916b-66c6dd91dea4
# # Name: old-growth-management-areas-legal-current
# aoi.OGMA <- retrieve_geodata_aoi(ID = "1b30f3bd-0ad0-4128-916b-66c6dd91dea4")
# 
# ggplot()+
#   geom_sf(data = aoi)+
#   geom_sf(data = aoi.OGMA)
# 
# ### WHAs
# bcdc_search("WHA", res_format = "wms")
# # 2: Wildlife Habitat Areas - Approved (multiple, wms, kml)
# # ID: b19ff409-ef71-4476-924e-b3bcf26a0127
# # Name: wildlife-habitat-areas-approved
# aoi.WHA <- retrieve_geodata_aoi(ID = "b19ff409-ef71-4476-924e-b3bcf26a0127")
# 
# ggplot()+
#   geom_sf(data = aoi)+
#   geom_sf(data = aoi.WHA)
# 
# ### UWR
# bcdc_search("UWR", res_format = "wms")
# # 2: Ungulate Winter Range - Approved (multiple, wms, kml)
# # ID: 712bd887-7763-4ed3-be46-cdaca5640cc1
# # Name: ungulate-winter-range-approved
# aoi.UWR <- retrieve_geodata_aoi(ID = "712bd887-7763-4ed3-be46-cdaca5640cc1")
# 
# ggplot()+
#   geom_sf(data = aoi)+
#   geom_sf(data = aoi.UWR)
# 
# ### Old growth deferral
# bcdc_search("deferral", res_format = "wms")
# # 2: Old Growth Technical Advisory Panel (TAP) - Priority Deferral Areas - Current
# # View (multiple, other, wms, kml, arcgis_rest)
# # ID: f257ca4a-0c33-4eb2-9da8-21dff4482f58
# # Name:
# aoi.OGD <- retrieve_geodata_aoi(ID = "f257ca4a-0c33-4eb2-9da8-21dff4482f58")
# 
# ggplot()+
#   geom_sf(data = aoi)+
#   geom_sf(data = aoi.OGD)
# 
# ### Provincial Parks
# bcdc_search("protected areas", res_format = "wms")
# # 8: BC Parks, Ecological Reserves, and Protected Areas (wms, kml, multiple)
# # ID: 1130248f-f1a3-4956-8b2e-38d29d3e4af7
# # Name: bc-parks-ecological-reserves-and-protected-areas
# aoi.PA <- retrieve_geodata_aoi(ID = "1130248f-f1a3-4956-8b2e-38d29d3e4af7")
# 
# ggplot()+
#   geom_sf(data = aoi)+
#   geom_sf(data = aoi.PA)
# 
# ### save co-location sf objects in one place
# all_colocate <- list(
#   OGMA = aoi.OGMA,
#   WHA = aoi.WHA,
#   UWR = aoi.UWR,
#   OGD = aoi.OGD,
#   PA = aoi.PA
# )
# 
# # Save as an .RDS file
# saveRDS(all_colocate, "Q_colocate.rds")
all_colocate <- readRDS("Q_colocate.rds")

# #### Areas to remove (i.e., challenging to conserve)
# ### burn severity
# bcdc_search("burn severity", res_format = "wms")
# # 1: Fire Burn Severity - Historical (wms, kml, multiple)
# # ID: c58a54e5-76b7-4921-94a7-b5998484e697
# # Name: fire-burn-severity-historical
# # 2: Fire Burn Severity - Same Year (multiple, wms, kml, fgdb)
# # ID: 04c5ad28-d8eb-4c49-90c5-48b9b98fdfe9
# # Name: fire-burn-severity-same-year
# aoi.burn2024 <- retrieve_geodata_aoi(ID = "04c5ad28-d8eb-4c49-90c5-48b9b98fdfe9")
# 
# ggplot()+
#   geom_sf(data = aoi)+
#   geom_sf(data = aoi.burn2024)
# 
# aoi.burnhist <- retrieve_geodata_aoi(ID = "c58a54e5-76b7-4921-94a7-b5998484e697")
# 
# ggplot()+
#   geom_sf(data = aoi)+
#   geom_sf(data = aoi.burnhist)
# 
# ### save avoid sf objects in one place
# all_avoid <- list(
#   burn2024 = aoi.burn2024,
#   burnhist = aoi.burnhist
# )
# 
# # Save as an .RDS file
# saveRDS(all_avoid, "Q_avoid.rds")
all_avoid <- readRDS("Q_avoid.rds")

# now remove all low or unburned polygons
# only want to exclude the medium and high burn severity areas
all_avoid$burn2024 <- all_avoid$burn2024 %>% filter(BURN_SEVERITY_RATING %in% c("High","Medium"))
all_avoid$burnhist <- all_avoid$burnhist %>% filter(BURN_SEVERITY_RATING %in% c("High","Medium"))

all_avoid$private <- Q_private

# ########################################################
# ## 2. REFINE BASED ON AVOIDS (REMOVE ALL)
# # Rasterize each to match rQ
# 
# # rQ <- Q_chilcotin
# rQ <- Q_cariboo
# 
# avoid_rasters <- lapply(names(all_avoid), function(name) {
#   sf_obj <- all_avoid[[name]]
#   
#   # Skip NULL entries
#   if (is.null(sf_obj)) {
#     warning(paste("Skipping", name, "- is NULL"))
#     return(NULL)
#   }
#   
#   # Proceed with cleaning and rasterizing
#   sf_obj <- st_make_valid(sf_obj)
#   sf_obj <- st_cast(sf_obj, "POLYGON", warn = FALSE)
#   sf_obj$field <- 1L
#   
#   v <- tryCatch(vect(sf_obj), error = function(e) {
#     warning(paste("Skipping", name, "- vect() failed:", e$message))
#     return(NULL)
#   })
#   
#   if (is.null(v)) return(NULL)
#   
#   r <- rasterize(v, rQ, field = "field", background = 0)
#   names(r) <- name
#   return(r)
# })
# 
# # Step 3: Combine and make an "avoid" mask
# avoid_rasters <- Filter(Negate(is.null), avoid_rasters)
# avoid_stack <- rast(avoid_rasters)
# 
# # Combine: any pixel with 1 in either layer is an "avoid"
# avoid_mask <- sum(avoid_stack, na.rm = TRUE) > 0
# 
# # Step 4: Invert mask — we want to KEEP only where no avoid layers overlap
# keep_mask <- !avoid_mask  # TRUE where no overlap, FALSE where burned
# 
# # Step 5: Apply to rQ
# Q_clean <- mask(rQ, keep_mask, maskvalues = 0)
# 
# # Plot
# plot(Q_clean, main = "rQ excluding burned areas")
# # writeRaster(Q_clean, "Q_keep_Chilcotin.tif", overwrite=T)
# writeRaster(Q_clean, "Q_keep_Cariboo.tif", overwrite=T)

Q_keep_Cariboo <- rast(paste0(getwd(),"/Q_keep_Cariboo.tif"))
Q__keep_Chilcotin <- rast(paste0(getwd(),"/Q_keep_Chilcotin.tif"))
plot(Q__keep_Chilcotin)

# #########################################################
# # 3. NOW DETERMINE WHERE VALUES CO-LOCATE
# 
# # Q_clean <- Q_keep_Cariboo
# Q_clean <- Q__keep_Chilcotin
# 
# all_colocate <- lapply(all_colocate, function(x) st_transform(x, crs(rQ)))
# 
# # Clean and rasterize each sf object safely
# raster_list <- lapply(names(all_colocate), function(name) {
#   sf_obj <- all_colocate[[name]]
#   
#   # Ensure valid geometry
#   sf_obj <- st_make_valid(sf_obj)
#   
#   # Force to polygons if needed (avoid MULTIPOLYGON or GEOMETRYCOLLECTION)
#   sf_obj <- st_cast(sf_obj, "POLYGON", warn = FALSE)
#   
#   # Add a dummy field (ensure same length as geometries)
#   sf_obj$field <- 1L
#   
#   # Convert to terra SpatVector
#   v <- tryCatch(vect(sf_obj), error = function(e) NULL)
#   
#   if (is.null(v)) {
#     warning(paste("Skipping", name, "- could not convert to SpatVector"))
#     return(NULL)
#   }
#   
#   # Rasterize
#   r <- rasterize(v, Q_clean, field = "field", background = 0)
#   names(r) <- name
#   return(r)
# })
# 
# # Remove any failed/NULL layers
# raster_list <- Filter(Negate(is.null), raster_list)
# 
# # Stack and sum
# overlap_stack <- rast(raster_list)
# overlap_count <- sum(overlap_stack, na.rm = TRUE)
# 
# # Plot
# plot(overlap_count, main = "Number of overlapping layers per pixel")
# 
# # Create a mask for high values
# high_value <- Q_clean > 15
# 
# # Create a mask for areas with >1 overlapping feature
# overlap_mask <- overlap_count > 0
# 
# # Combine the two masks (logical AND)
# priority_areas <- high_value & overlap_mask
# 
# # Plot the result
# plot(priority_areas, main = "High rQ & Overlapping Features")
# 
# # Optional: mask Q_clean itself to visualize the values in those areas
# Q_selected <- mask(Q_clean, priority_areas, maskvalues = 0)
# plot(Q_selected, main = "rQ Values in Overlapping Areas")
# 
# plot(Q_cariboo)
# plot(Q_chilcotin)
# 
# # Export the result
# # writeRaster(Q_selected, "Q_selected_Cariboo.tif", overwrite=T)
# writeRaster(Q_selected, "Q_selected_Chilcotin.tif", overwrite=T)

Q_selected_Cariboo <- rast(paste0(getwd(),"/Q_selected_Cariboo.tif"))
Q_selected_Chilcotin <- rast(paste0(getwd(),"/Q_selected_Chilcotin.tif"))

#### REFINE THE AREAS
Q_general_areas <- sf::st_read(dsn = DSN, layer="Q_general_areas")


# Assume:
# - r1 is your primary raster
r1 <- Q__keep_Chilcotin
plot(r1)

plot(Q_keep_Chilcotin)
plot(Q_keep_Cariboo)
# - r2 is your secondary raster
r2 <- Q_selected_Chilcotin

plot(Q_selected_Chilcotin)
plot(Q_selected_Cariboo)

# - poly_list is your list of individual polygons (sf)
ggplot()+
  geom_sf(data=Q_general_areas)
poly_list <- Q_general_areas

###
Q_general_areas$Id <- c(1,2,3,4,5,6)
poly_split <- split(Q_general_areas, Q_general_areas$Id) %>% 
  map(st_as_sf)

Q_general_areas <- st_transform(Q_general_areas, crs = crs(r1))


clip_both_rasters <- function(poly_row) {
  name <- poly_row$Label
  v <- terra::vect(poly_row)
  
  # Check overlap before cropping
  if (!terra::relate(v, r1, "intersects")) {
    message(paste("Skipping", name, "- no overlap with raster"))
    return(NULL)
  }
  
  r1_clipped <- mask(crop(r1, v), v)
  r2_clipped <- mask(crop(r2, v), v)
  
  list(name = name, r1 = r1_clipped, r2 = r2_clipped)
}


clipped <- map(1:nrow(Q_general_areas), function(i) {
  clip_both_rasters(Q_general_areas[i, ])
})

clipped <- map(1:nrow(Q_general_areas), ~clip_both_rasters(Q_general_areas[.x, ]))
clipped <- compact(clipped)
names(clipped) <- map_chr(clipped, "name")

plot(r1)
plot(st_geometry(Q_general_areas), add = TRUE, border = "red", lwd = 2)

###########################################

make_priority_mask <- function(r1, r2) {
  # Mask from r1:
  high_r1 <- r1 >= 20 & r1 <= 25
  med_r1  <- r1 >= 15 & r1 < 20
  
  # Mask from r2:
  high_r2 <- r2 > 0
  
  # Combine into one layer:
  # 2 = high priority
  # 1 = medium priority
  # NA = ignore
  mask <- classify(r1, matrix(c(-Inf, 15, NA,   # ignore < 15
                                15, 20, 1,       # medium
                                20, 25, 2,       # high
                                25, Inf, NA),    # ignore > 25
                              ncol = 3, byrow = TRUE))
  
  # Set high priority where r2 > 0
  mask[high_r2] <- 2
  
  return(mask)
}

priority_masks <- map(clipped, ~ make_priority_mask(.x$r1, .x$r2))
priority_polys <- map(priority_masks, ~ as.polygons(.x, na.rm = TRUE))
high_priority <- map(priority_polys, ~ .x[.x$lyr.1 == 2, ])
valid_priority <- map(priority_polys, ~ .x[.x$lyr.1 %in% c(1, 2), ])

# plot(priority_masks$Area12)

# Convert terra raster to dataframe for ggplot
raster_to_df <- function(r, name) {
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colnames(df) <- c("x", "y", "priority")
  df$area <- name
  return(df)
}

# Apply to all priority masks
priority_dfs <- map2(priority_masks, names(priority_masks), raster_to_df)

# Combine into one data frame
priority_all <- bind_rows(priority_dfs)

ggplot(priority_all, aes(x = x, y = y, fill = factor(priority))) +
  geom_raster() +
  facet_wrap(~ area) +
  scale_fill_manual(
    values = c("1" = "orange", "2" = "red"),
    name = "Priority",
    labels = c("1" = "Medium", "2" = "High")
  ) +
  coord_equal() +
  theme_minimal() +
  labs(
    title = "Priority Areas from Raster Masking",
    x = "Longitude", y = "Latitude"
  )

##########################################################
plot(priority_masks$Area91011)
writeRaster(priority_masks$Area91011, "rArea91011.tif", overwrite=T)
writeRaster(priority_masks$Area1, "rArea1.tif", overwrite=T)
writeRaster(priority_masks$Area2, "rArea2.tif", overwrite=T)
writeRaster(priority_masks$Area3, "rArea3.tif", overwrite=T)
writeRaster(priority_masks$Area6, "rArea6.tif", overwrite=T)
writeRaster(priority_masks$Area12, "rArea12.tif", overwrite=T)

# Find cells equal to 2
r2 <- priority_masks$Area91011 == 2

# Each cell’s area in m² (or match raster CRS units)
cell_areas <- cellSize(r2, unit = "m")

# Total area where value == 2
total_area <- global(cell_areas * r2, "sum", na.rm = TRUE)

total_area / 10000

# Area 12 = 8322 ha or 83.22 km2 for high priority
# Area 1 = 11714 ha or 117.14 km2 for high priority
# Area 2 = 24984 ha 249.84 km2 for high priority
# Area 3 = 12751 ha 127.51 km2 for high priority
# Area 6 = 27137 ha or 271.37 km2 for high priority
# Area 91011 = 65696 or 656.96 km2 ha for high priority
