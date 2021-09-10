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

for (y in 2010:2014) { dir.create(paste0('RData/LU.crop_', y), showWarnings = F)}
for (y in 2010:2014) { dir.create(paste0('RData/CO2_', y), showWarnings = F)}

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
  
  LU.stack <- list()
  for (l in layer) {
    LU.stack[[l]] <- raster(paste0('Out/LU_', y, '/TNO.stack_', l, '.tif'))
  }
  LU.stack <- stack(LU.stack)
  res(LU.stack) <- res
  print(LU.stack)
  
  ### Extract raster values and attribute it to vectors from corresponding LU
  # - LU.stack.Residential[[i]]: cropped raster data for FUA i and LU Residential
  # - Residential.UA[[i]]$CO2: CO2 emissions for FUA i in Residential sector
  
  # VERY LONG, DO NOT RE-RUN, UNLESS NECESSARY
  # ES008L2, ES025L2, ES045L0, ES062L0, ES072L0, ES074L0
  for (i in levels(BoundariesWGS$FUA)[-c(188, 204, 217, 226, 229, 231)]) {
    print(i)
    load(paste0('RData/LU/LU_', i, '.RData'))
    Residential.UA <- if (dim(Residential.UA)[1] != 0) { spTransform(Residential.UA, crs) }
    NonResidential.UA <- if (dim(NonResidential.UA)[1] != 0) { spTransform(NonResidential.UA, crs) }
    Road.UA <- if (dim(Road.UA)[1] != 0) { spTransform(Road.UA, crs) }
    NonRoad.UA <- if (dim(NonRoad.UA)[1] != 0) { spTransform(NonRoad.UA, crs) }
    Agriculture.UA <- if (dim(Agriculture.UA)[1] != 0) { spTransform(Agriculture.UA, crs) }
    
    # LU.crop <- raster::crop(LU.stackLambert, landUse, snap = 'out')
    LU.crop <- raster::crop(LU.stack, extent(subset(BoundariesWGS, FUA == i)), snap = 'out')
    # LU.crop <- raster::mask(LU.crop, subset(BoundariesWGS, FUA == i)) # REMOVES TOO MANY CELLS WITH THIS COMMAND
    LU.crop <- rasterToPolygons(LU.crop)
    
    # Residential
    CO2.poly.Residential <- if (!is.null(Residential.UA)) { intersect(LU.crop[, 'Residential'], gBuffer(Residential.UA, width = 0)) }
    # NonResidential
    CO2.poly.NonResidential <- if (!is.null(NonResidential.UA)) { intersect(LU.crop[, 'NonResidential'], gBuffer(NonResidential.UA, width = 0)) }
    # Road
    CO2.poly.Road <- if (!is.null(Road.UA)) { intersect(LU.crop[, 'Road'], gBuffer(Road.UA, width = 0)) }
    # NonRoad
    CO2.poly.NonRoad <- if (!is.null(NonRoad.UA)) { intersect(LU.crop[, 'NonRoad'], gBuffer(NonRoad.UA, width = 0)) }
    # Agriculture
    CO2.poly.Agriculture <- if (!is.null(Agriculture.UA)) { intersect(LU.crop[, 'Agriculture'], gBuffer(Agriculture.UA, width = 0)) }
    
    save(LU.crop, file = paste0('RData/LU.crop_', y, '/LU.crop_', i, '.RData'))
    save(CO2.poly.Residential, CO2.poly.NonResidential, CO2.poly.Road, CO2.poly.NonRoad, CO2.poly.Agriculture, 
         file = paste0('RData/CO2_', y, '/CO2_', i, '.RData'))
    
    rm(CO2.poly.Residential, CO2.poly.NonResidential, CO2.poly.Road, CO2.poly.NonRoad, CO2.poly.Agriculture)
  }
}