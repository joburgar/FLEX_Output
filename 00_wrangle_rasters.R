#####################################################################################
R_version <- paste0("R-",version$major,".",version$minor)
.libPaths(paste0("C:/Program Files/R/",R_version,"/library")) # to ensure reading/writing libraries from C drive

# install.packages("terra")
library(terra)
library(tidyverse)
library(data.table)
library(sf)
library(bcdata)
# Save raster with layer names

# List all raster files in a folder
r_files <- list.files(paste0(here::here(), "/Input_files/"),pattern = "\\.tif$", full.names = TRUE)

# Load and stack them
raster_stack <- rast(r_files)

# Check the stack
print(raster_stack)
# plot(raster_stack)

# Load the polygon shapefile (SpatVector)
GIS_Boreal_Dir <- "//sfp.idir.bcgov/S140/S40203/Ecosystems/Conservation Science/Species/Mesocarnivores/Fisher_status/CDC_ESR/"

Boreal <- sf::st_read(dsn = GIS_Boreal_Dir, layer = "Boreal_RE_2025")
Columbian <- sf::st_read(dsn = GIS_Boreal_Dir, layer = "Columbian_RE_2025")


# GIS_Columbian_Dir <- "//sfp.idir.bcgov/S140/S40203/Ecosystems/Conservation Science/Species/Mesocarnivores/Projects/MMP/2.Data/Mesocarnivores DB/5. GIS"
# subpopulations <- sf::st_read(dsn = file.path(GIS_Dir, "BC_Fisher_populations_2024.gdb"), layer = "Subpopulations")
# 
# Cariboo <- subpopulations |> dplyr::filter(Subpop == "Cariboo")
# Chilcotin <- subpopulations |> dplyr::filter(Subpop == "Chilcotin")
# Omineca <- subpopulations |> dplyr::filter(Subpop == "Omineca")
# 
# rm(subpopulations)# housekeeping
# Find TSAs within Boreal

# bcdc_search("tsa", res_format = "wms")
# 1: FADM - Timber Supply Area (TSA) (multiple, wms, kml, oracle_sde)
# ID: 8daa29da-d7f4-401c-83ae-d962e3a28980
# Name: fadm-timber-supply-area-tsa
tsa <- bcdc_get_data("8daa29da-d7f4-401c-83ae-d962e3a28980")

# save.image("wrangle_rasters_inputs.RData")
# load("wrangle_rasters_inputs.RData")

Boreal_tsa <- tsa %>% sf::st_intersection(Boreal)
ggplot()+
  geom_sf(data = Boreal_tsa, aes(col=TSA_NUMBER))

rm(tsa)# housekeeping

Boreal_tsa %>% group_by(TSA_NUMBER) %>% summarise(sum(Area)) 

Boreal_tsa_merged <- Boreal_tsa %>%
  group_by(TSA_NUMBER) %>%
  summarise(geometry = sf::st_union(geometry), .groups = "drop")

Boreal_tsa_merged <- Boreal_tsa_merged %>% dplyr::mutate(area_meters = st_area(Boreal_tsa_merged))
Boreal_tsa_merged %>% st_drop_geometry()

ggplot()+
  geom_sf(data = Boreal_tsa_merged, aes(col=TSA_NUMBER))

Boreal_tsa_merged$TSA_NUMBER <- as.factor(Boreal_tsa_merged$TSA_NUMBER)
Boreal_tsa %>% group_by(TSA_NUMBER) %>% count(TSA_NUMBER_DESCRIPTION) %>% sf::st_drop_geometry()
# 04         Cassiar TSA                6
# 08         Fort Nelson TSA            6
# 16         MacKenzie TSA              5
# 40         Fort St. John TSA          7
# 41         Dawson Creek TSA           5
rm(Boreal_tsa)

# Ensure both have the same CRS

terra::crs(raster_stack) <- "EPSG:3005"
Boreal <- sf::st_transform(Boreal, crs = terra::crs(raster_stack))

FLEXraster <- function(aoi) {
  
  aoi_filtered <- terra::vect(aoi)  # convert sf to SpatVector for terra compatibility
  
  # Crop and mask
  clipped_raster <- terra::crop(raster_stack, aoi_filtered)
  clipped_raster <- terra::mask(clipped_raster, aoi_filtered)
  
   # Load an example raster (assuming clipped_raster is already loaded)
  pixel_id <- clipped_raster[[1]]  # Use first layer as a template
  
  # Create a sequence of unique IDs for all pixels, including NA ones
  pixel_id[] <- as.integer(seq_len(ncell(pixel_id)))  # Assign unique ID to every pixel
  
  # Convert to integer type
  pixel_id <- round(pixel_id)
  
  # Add pixel ID layer to the raster stack
  updated_stack <- c(clipped_raster, pixel_id)
  
  # Rename the new layer for clarity
  names(updated_stack)[nlyr(updated_stack)] <- "Pixel_ID"
  
  # Plot to check the result
  plot(updated_stack)
  
  # Check layer names
  print(names(updated_stack))
  
  # Reorder layers
  layer_names <- c("Pixel_ID", "fisher_pop", "denning2023", "rest_cavity2023", 
                   "movement2023", "rest_rust2023", "rest_cwd2023", "open2023")
  
  if (!all(layer_names %in% names(updated_stack))) {
    stop("One or more expected layers are missing in clipped_raster.")
  }
  
  new_order <- updated_stack[[layer_names]]
  
  # Rename layers for FLEX
  flex_names <- c("pixelid", "ras_fisher_pop", "ras_fisher_denning_init", "ras_fisher_cavity_init", 
                  "ras_fisher_movement_init", "ras_fisher_rust_init", "ras_fisher_cwd_init", 
                  "ras_fisher_open_init")
  
  names(new_order) <- flex_names
  
  # Plot final result
  plot(new_order)
  
  return(new_order)
}

Boreal_forFLEX <- FLEXraster(aoi = Boreal)

# Save the  raster
writeRaster(Boreal_forFLEX, "Boreal_forFLEX.tif", datatype = "INT4S",overwrite=TRUE) # for large rasters, need to provide data type



