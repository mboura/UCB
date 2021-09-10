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

for (y in 2010:2014) { dir.create(paste0('Out/CO2Sector_', y), showWarnings = F)}
for (y in 2010:2014) { dir.create(paste0('Out/CO2All_', y), showWarnings = F)}

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
    
    Emissions_cell <- stack(paste0('Out/CO2Sector_cell_', y, '/Emissions_', i, '.tif'))
    Emissions_pt <- stack(paste0('Out/CO2Sector_point_', y, '/Emissions_', i, '.tif'))
    names(Emissions_cell) <- layer
    names(Emissions_pt) <- layer[c(2,4)]
    
    # Total E per sector per FUA
    Emissions <- stack(Emissions_cell, Emissions_pt)
    Emissions <- stackApply(Emissions, indices = c(1:5,2,4), fun = 'sum')
    names(Emissions) <- layer
    writeRaster(Emissions, filename = paste0('Out/CO2Sector_', y, '/Emissions_', i, '.tif'), overwrite = T)
    
    # Total E per FUA
    EmissionsAll <- stackApply(Emissions, indices = rep(1, 5), fun = 'sum')
    EmissionsAll[EmissionsAll == 0] <- NA
    names(EmissionsAll) <- 'CO2'
    writeRaster(EmissionsAll, filename = paste0('Out/CO2All_', y, '/Emissions_', i, '.tif'), overwrite = T)
  }
}