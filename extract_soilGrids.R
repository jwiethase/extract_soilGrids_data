#' ---
#' title: "Tarangire-Manyara soil analysis"
#' author: "Joris Wiethase"
#' date: "Nov 27, 2019"
#' ---

# Start with clean environment
rm(list = ls(all=TRUE))  

# Load packages
suppressWarnings(suppressMessages(suppressPackageStartupMessages({
  library(GSIF)
  library(raster)
  library(ggplot2)
  library(ggthemes)
  library(dplyr)
})))

# Set global ggplot theme
theme_set(ggthemes::theme_few(base_size = 10))

# Set working directory
setwd("~/Google Drive/Work/PhD_York/site_data/soilGrids/extract_soilGrids_data/")

-----
#'## Data import and preparation
#' Import soil files, downloaded from https://soilgrids.org/ for 15cm depth
soil_sand <- raster("soil_files/SNDPPT_M_sl3_250m.tif")
soil_silt <- raster("soil_files/SLTPPT_M_sl3_250m.tif")
soil_clay <- raster("soil_files/CLYPPT_M_sl3_250m.tif")
soil_cec <- raster("soil_files/CECSOL_M_sl3_250m.tif")
soil_bld <- raster("soil_files/BLDFIE_M_sl3_250m.tif")
soil_orc <- raster("soil_files/ORCDRC_M_sl3_250m.tif")
soil_phi <- raster("soil_files/PHIHOX_M_sl3_250m.tif")


-----
#'## Data extraction
#' Create grid to extract data from raster
# Use metric CRS to space out grid equal to soil data resolution (250m). Use extent of pre-defined,
# chosen polygon
grd <- expand.grid(x = seq(from = 172052.5794289853656664, to = 192020.0272475373349153,
                             by = 250),
                     y = seq(from = 9574300.2855877578258514, 
                             to = 9594281.8629634641110897, 
                             by = 250))
sp::coordinates(grd) <- ~x + y
sp::gridded(grd) <- TRUE

# Convert CRS to WGS84 for value extraction
sp::proj4string(grd) <-  CRS("+init=epsg:32737")
grd_WGS84 <- spTransform(grd, CRS("+init=epsg:4326"))
grd_WGS84 <- as.data.frame(grd_WGS84)

#' Extract soil values from tifs using the grid points
extr_df <- grd_WGS84

extr_df$sand <- raster::extract(soil_sand, grd_WGS84)
extr_df$silt <- raster::extract(soil_silt, grd_WGS84)
extr_df$clay <- raster::extract(soil_clay, grd_WGS84)
extr_df$cec <- raster::extract(soil_cec, grd_WGS84)
extr_df$bld <- raster::extract(soil_bld, grd_WGS84)
extr_df$orc <- raster::extract(soil_orc, grd_WGS84)
extr_df$phi <- raster::extract(soil_phi, grd_WGS84)

#' Replace missing values:
extr_df$bldf <- ifelse(is.na(extr_df$bld),
                       mean(extr_df$bld, na.rm=TRUE), extr_df$bld)

#' Calculate water holding capacity
extr_df <- cbind(extr_df, GSIF::AWCPTF(extr_df$sand, extr_df$silt,
                                       extr_df$clay, extr_df$orc, BLD=extr_df$bldf*1000, extr_df$cec,
                                       extr_df$phi, h1=-10, h2=-20, h3=-31.6)) %>% 
  mutate(field_capacity = AWCh1 + WWP)

# Save the created data frame
# write.csv(extr_df, "extracted_soil.csv")

-----
  #'## Visually explore the data
  #' Plot density of soil properties for the chosen area
ggplot() +
  geom_density(data = extr_df, aes(field_capacity)) +
  geom_vline(xintercept = median(extr_df$field_capacity), 
             col = "red", alpha = .5, lty = 2) +
  xlab("Field capacity (%)") 

ggplot() +
  geom_density(data = extr_df, aes(sand)) +
  geom_vline(xintercept = median(extr_df$sand), 
             col = "red", alpha = .5, lty = 2) +
  xlab("Weight percentage of the sand particles at 15cm depth (%)") 

ggplot() +
  geom_density(data = extr_df, aes(silt)) +
  geom_vline(xintercept = median(extr_df$silt), 
             col = "red", alpha = .5, lty = 2) +
  xlab("Weight percentage of the silt particles at 15cm depth (%)") 

ggplot() +
  geom_density(data = extr_df, aes(clay)) +
  geom_vline(xintercept = median(extr_df$clay), 
             col = "red", alpha = .5, lty = 2) +
  xlab("Weight percentage of the clay particles at 15cm depth (%)") 

-----
#'## Summarize the soil values
soil_average <- extr_df %>% 
  dplyr::summarise_all(list(av = median)) %>% 
  mutate_if(is.numeric, ~round(., 3))
print(soil_average)

solomon_cult3 <- GSIF::AWCPTF(SNDPPT=60.6, SLTPPT=13.6, CLYPPT=25.7, 
             ORCDRC=8.3, BLD=1.33*1000, CEC=14.1, PHIHOX=7, h1=-10, h2=-20, h3=-31.6)
solomon_cult3$field_capacity = solomon_cult3$AWCh1 + solomon_cult3$WWP
