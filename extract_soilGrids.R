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
  library(tidyverse)
})))

# Set global ggplot theme
theme_set(ggthemes::theme_few(base_size = 10))

# Set working directory
setwd("~/Google Drive/Work/PhD_York/site_data/soilGrids/extract_soilGrids_data/")


#'## Data import and preparation
#' Import soil files, downloaded from https://soilgrids.org/ for 15cm depth
soil_sand <- raster("soil_files_TZ/SNDPPT_M_sl3_250m.tif")
soil_silt <- raster("soil_files_TZ/SLTPPT_M_sl3_250m.tif")
soil_clay <- raster("soil_files_TZ/CLYPPT_M_sl3_250m.tif")
soil_cec <- raster("soil_files_TZ/CECSOL_M_sl3_250m.tif")
soil_bld <- raster("soil_files_TZ/BLDFIE_M_sl3_250m.tif")
soil_orc <- raster("soil_files_TZ/ORCDRC_M_sl3_250m.tif")
soil_phi <- raster("soil_files_TZ/PHIHOX_M_sl3_250m.tif")



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
extr_df$bld <- raster::extract(soil_bld, grd_WGS84)   # in kg/m3
extr_df$orc <- raster::extract(soil_orc, grd_WGS84)
extr_df$phi <- raster::extract(soil_phi, grd_WGS84)*0.1

#' Replace missing values:
extr_df$bldf <- ifelse(is.na(extr_df$bld),
                       mean(extr_df$bld, na.rm=TRUE), extr_df$bld)

#' Calculate water holding capacity
extr_df <- cbind(extr_df, GSIF::AWCPTF(extr_df$sand, extr_df$silt,
                                       extr_df$clay, extr_df$orc, BLD=extr_df$bldf, extr_df$cec,
                                       extr_df$phi, h1=-10, h2=-20, h3=-31.6)) %>% 
  mutate(field_capacity = AWCh1 + WWP)

# Save the created data frame
# write.csv(extr_df, "extracted_soil.csv")


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


#'## Summarize the soil values
soil_average <- extr_df %>% 
  dplyr::summarise_all(list(av = median)) %>% 
  mutate_if(is.numeric, ~round(., 3))
print(soil_average)


#'## Compare the extracted soil values with values from spatially close (~85km) field study
solomon <- data.frame("Site" = c("wood_nat", "wood_degr", "cult3", "cult15"),
                      "SNDPPT" = c(50.4, 69.7, 60.6, 60.4),
                      "SLTPPT" = c(19.4, 11.6, 13.6, 13.5),
                      "CLYPPT" = c(30.2, 18.6, 25.7, 26.1),
                      "ORCDRC" = c(18.7, 13.8, 8.3, 8.2),
                      "BLD" = c(1.2, 1.33, 1.33, 1.33)*1000,  # Convert g/cm3 to kg/m3
                      "CEC" = c(16.8, 15.7, 14.1, 11.7),
                      "PHIHOX" = c(6.6, 6.9, 7, 6.7))

solomon_soil <- solomon %>% 
  group_by(Site) %>% 
  do(new = GSIF::AWCPTF(SNDPPT = .$SNDPPT, SLTPPT = .$SLTPPT, CLYPPT = .$CLYPPT, 
                        ORCDRC= .$ORCDRC, BLD= .$BLD, CEC =.$CEC, 
                        PHIHOX = .$PHIHOX,
                        h1=-10, h2=-20, h3=-31.6)) %>% 
  unnest() %>% 
  mutate(field_capacity = AWCh1 + WWP)
  
t.test(solomon_soil$field_capacity, extr_df$field_capacity) 


#'## Extract soil values or York (soil that will be used to prepare mix for study)
soil_sand2 <- raster("soil_files_York/SNDPPT_M_sl3_250m.tif")
soil_silt2 <- raster("soil_files_York/SLTPPT_M_sl3_250m.tif")
soil_clay2 <- raster("soil_files_York/CLYPPT_M_sl3_250m.tif")
soil_cec2 <- raster("soil_files_York/CECSOL_M_sl3_250m.tif")
soil_bld2 <- raster("soil_files_York/BLDFIE_M_sl3_250m.tif")
soil_orc2 <- raster("soil_files_York/ORCDRC_M_sl3_250m.tif")
soil_phi2 <- raster("soil_files_York/PHIHOX_M_sl3_250m.tif")
  
grd2 <- expand.grid(x = seq(from = 622618, to = 633896,
                           by = 250),
                   y = seq(from = 5965695, 
                           to = 5986171, 
                           by = 250))
sp::coordinates(grd2) <- ~x + y
sp::gridded(grd2) <- TRUE

# Convert CRS to WGS84 for value extraction
sp::proj4string(grd2) <-  CRS("+init=epsg:32630")
grd2_WGS84 <- spTransform(grd2, CRS("+init=epsg:4326"))
grd2_WGS84 <- as.data.frame(grd2_WGS84)

#' Extract soil values from tifs using the grid points
extr_df2 <- grd2_WGS84

extr_df2$sand <- raster::extract(soil_sand2, grd2_WGS84)
extr_df2$silt <- raster::extract(soil_silt2, grd2_WGS84)
extr_df2$clay <- raster::extract(soil_clay2, grd2_WGS84)
extr_df2$cec <- raster::extract(soil_cec2, grd2_WGS84)
extr_df2$bld <- raster::extract(soil_bld2, grd2_WGS84)
extr_df2$orc <- raster::extract(soil_orc2, grd2_WGS84)
extr_df2$phi <- raster::extract(soil_phi2, grd2_WGS84)*0.1

#' Replace missing values:
extr_df2$bldf <- ifelse(is.na(extr_df2$bld),
                       mean(extr_df2$bld, na.rm=TRUE), extr_df2$bld)

#' Calculate water holding capacity
extr_df2 <- cbind(extr_df2, GSIF::AWCPTF(extr_df2$sand, extr_df2$silt,
                                       extr_df2$clay, extr_df2$orc, BLD=extr_df2$bldf, extr_df2$cec,
                                       extr_df2$phi, h1=-10, h2=-20, h3=-31.6)) %>% 
  mutate(field_capacity = AWCh1 + WWP)
  
#'## Summarize the soil values
soil_average2 <- extr_df2 %>% 
  dplyr::summarise_all(list(av = median)) %>% 
  mutate_if(is.numeric, ~round(., 3))

print(soil_average2)
soil_comp <- rbind(soil_average, soil_average2)  
soil_comp$Site <- c("TZ", "York") 
soil_comp <- soil_comp %>% 
  dplyr::select(-c(x_av, y_av, tetaS_av, AWCh3_av, AWCh2_av)) %>% 
  dplyr::select(Site, everything())


soil_average2
GSIF::AWCPTF(SNDPPT = 38, SLTPPT = 21.7, CLYPPT = 40.3, 
             ORCDRC= 2.84, BLD= 1230, CEC =20.4, 
             PHIHOX = 5.61,
             h1=-10, h2=-20, h3=-31.6)["AWCh1"] + GSIF::AWCPTF(SNDPPT = 38, SLTPPT = 21.7, CLYPPT = 40.3, 
                                                               ORCDRC= 3.3, BLD= 1200, CEC =20.4, 
                                                               PHIHOX = 4.8,
                                                               h1=-10, h2=-20, h3=-31.6)["WWP"]

