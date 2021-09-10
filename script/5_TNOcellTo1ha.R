rm(list = ls())


# Libraries ---------------------------------------------------------------

library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(knitr)
library(MASS)
library(RColorBrewer)
library(knitr)
library(stringr)

# New directories ---------------------------------------------------------

for (y in 2010:2014) { dir.create(paste0('RData/LU.cropLambert', y), showWarnings = F)}
for (y in 2010:2014) { dir.create(paste0('RData/CO2Lambert_', y), showWarnings = F)}
for (y in 2010:2014) { dir.create(paste0('Out/CO2Sector_cell_', y), showWarnings = F)}

# Loading environments ----------------------------------------------------

load('../Typo1/RData/Maps.RData') # loads the SpPolyDF Europe and Boundaries


# Spatial data ------------------------------------------------------------

crs <- CRS('+proj=longlat +datum=WGS84')
crsLambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

EuropeWGS <- spTransform(Europe, crs)
BoundariesWGS <- spTransform(Boundaries, crs)

res <- c(0.125, 0.0625)

layer <- c('Residential', 'NonResidential', 'Road', 'NonRoad', 'Agriculture')

# Processing the csv files -------------------------------------------------

year <- 2010:2014
# 2010 partly done
# 2012 done

for (y in year) {
  print(paste0('Processing year ', y))
  
  # ES008L2, ES025L2, ES045L0, ES062L0, ES072L0, ES074L0
  for (i in levels(BoundariesWGS$FUA)[-c(188, 204, 217, 226, 229, 231)]) {
    print(i)
    load(paste0('RData/LU.crop_', y, '/LU.crop_', i, '.RData')) # loading LU.crop from i
    LU.cropLambert <- spTransform(LU.crop, crsLambert)
    LU.cropLambert$FUA <- i
    save(LU.cropLambert, file = paste0('RData/LU.cropLambert_', y, '/LU.cropLambert_', i, '.RData'))
    
    # Loading CO2 shapefiles grids
    load(paste0('RData/CO2_', y, '/CO2_', i, '.RData')) # loading CO2.poly.Residential, CO2.poly.NonResidential, CO2.poly.Road, CO2.poly.NonRoad and CO2.poly.Agriculture from i
    # Reprojecting
    CO2.poly.ResidentialLambert <- if(!is.null(CO2.poly.Residential)) {spTransform(CO2.poly.Residential, crsLambert)}
    CO2.poly.NonResidentialLambert <- if(!is.null(CO2.poly.NonResidential)) {spTransform(CO2.poly.NonResidential, crsLambert)}
    CO2.poly.RoadLambert <- if(!is.null(CO2.poly.Road)) {spTransform(CO2.poly.Road, crsLambert)}
    CO2.poly.NonRoadLambert <- if(!is.null(CO2.poly.NonRoad)) {spTransform(CO2.poly.NonRoad, crsLambert)}
    CO2.poly.AgricultureLambert <- if(!is.null(CO2.poly.Agriculture)) {spTransform(CO2.poly.Agriculture, crsLambert)}
    # CO2 = emissions in kg per ha (100 * 100 m2)
    CO2.poly.ResidentialLambert$CO2 <- (CO2.poly.ResidentialLambert$Residential / mean(gArea(LU.cropLambert, byid = T))) / 10000
    CO2.poly.NonResidentialLambert$CO2 <- (CO2.poly.NonResidentialLambert$NonResidential / mean(gArea(LU.cropLambert, byid = T))) / 10000
    CO2.poly.RoadLambert$CO2 <- (CO2.poly.RoadLambert$Road / mean(gArea(LU.cropLambert, byid = T))) / 10000
    CO2.poly.NonRoadLambert$CO2 <- if(!is.null(CO2.poly.NonRoad)) {(CO2.poly.NonRoadLambert$NonRoad / mean(gArea(LU.cropLambert, byid = T))) / 10000}
    CO2.poly.AgricultureLambert$CO2 <- (CO2.poly.AgricultureLambert$Agriculture / mean(gArea(LU.cropLambert, byid = T))) / 10000
    # Adding the FUA
    CO2.poly.ResidentialLambert$FUA <- i
    CO2.poly.NonResidentialLambert$FUA <- i
    CO2.poly.RoadLambert$FUA <- i
    CO2.poly.NonRoadLambert$FUA <- if(!is.null(CO2.poly.NonRoad)) {i}
    CO2.poly.AgricultureLambert$FUA <- i
    # Exportin the data
    save(CO2.poly.ResidentialLambert, CO2.poly.NonResidentialLambert, CO2.poly.RoadLambert, 
         CO2.poly.NonRoadLambert, CO2.poly.AgricultureLambert, 
         file = paste0('RData/CO2Lambert_', y, '/CO2Lambert_', i, '.RData'))
    
    rm(CO2.poly.ResidentialLambert, CO2.poly.NonResidentialLambert, CO2.poly.RoadLambert, 
       CO2.poly.NonRoadLambert, CO2.poly.AgricultureLambert)
  }
  
  # Ratserise at 100 * 100 m resolution
  # 
  # Export the rasters in ../Out/CO2/ as stack
  # 
  # Import them all in the list object: Emissions (1 stack of raster per FUA)
  
  
  for (i in levels(BoundariesWGS$FUA)[-c(188, 204, 217, 226, 229, 231)]) {
    load(paste0('RData/LU.cropLambert/LU.cropLambert_', i, '.RData'))
    load(paste0('RData/CO2Lambert_', y, '/CO2Lambert_', i, '.RData'))
    
    r <- raster(ext = extent(LU.cropLambert), res = 100, crs = crsLambert)
    r0 <- r; r0[] <- NA
    
    CO2.Residential <- if(!is.null(CO2.poly.ResidentialLambert)) {rasterize(CO2.poly.ResidentialLambert, r, field = 'CO2')} else {r0}
    CO2.NonResidential <- if(!is.null(CO2.poly.NonResidentialLambert)) {rasterize(CO2.poly.NonResidentialLambert, r, field = 'CO2')} else {r0}
    CO2.Road <- if(!is.null(CO2.poly.RoadLambert)) {rasterize(CO2.poly.RoadLambert, r, field = 'CO2')} else {r0}
    CO2.NonRoad <- if(!is.null(CO2.poly.NonRoadLambert)) {rasterize(CO2.poly.NonRoadLambert, r, field = 'CO2')} else {r0}
    CO2.Agriculture <- if(!is.null(CO2.poly.AgricultureLambert)) {rasterize(CO2.poly.AgricultureLambert, r, field = 'CO2')} else {r0}
    
    writeRaster(setNames(stack(CO2.Residential, CO2.NonResidential, CO2.Road, 
                               CO2.NonRoad, CO2.Agriculture), layer),
                paste0('Out/CO2Sector_cell_', y, '/Emissions_', i, '.tif'), overwrite = T)
    
    rm(CO2.Residential, CO2.NonResidential, CO2.Road, CO2.NonRoad, CO2.Agriculture)
  }
}