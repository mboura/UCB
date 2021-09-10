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

dir.create('RData', showWarnings = F)
dir.create('RData/LU', showWarnings = F)

# Loading environments ----------------------------------------------------

load('../Typo1/RData/Maps.RData') # loads the SpPolyDF Europe and Boundaries


# Spatial data ------------------------------------------------------------

crs <- CRS('+proj=longlat +datum=WGS84')
crsLambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

EuropeWGS <- spTransform(Europe, crs)
BoundariesWGS <- spTransform(Boundaries, crs)

res <- c(0.125, 0.0625)

layer <- c('Residential', 'NonResidential', 'Road', 'NonRoad', 'Agriculture')

# LU categories

# Split the data into the LU categories of interest
# - CO2 emissions coming from Residential, NonResidential, Road, NonRoad and Agriculture
# - CO2 sequestration coming from Forest, Wetlands, Herbaceous and other UGI.
# - Water: just for mapping

for (i in levels(Boundaries$FUA)) {
  file <- list.files('../Typo1/RData/LU', recursive = T, pattern = paste0(i, '.RData$'), full.names = T)
  load(file) # loading landUse of FUA i
  if (is.na(proj4string(landUse))) { proj4string(landUse) <- CRS(crsLambert) }
  
  Residential.UA <- landUse[landUse@data$CODE2012 %in% c(11100, 11210, 11220, 11230, 11240, 11300), ]
  NonResidential.UA <- landUse[landUse@data$CODE2012 %in% c(12100), ]
  Road.UA <- landUse[landUse@data$CODE2012 %in% c(12210, 12220), ]
  NonRoad.UA <- landUse[landUse@data$CODE2012 %in% c(12230, 12300, 12400), ]
  Agriculture.UA <- landUse[landUse@data$CODE2012 %in% c(21000, 22000, 23000, 24000, 25000), ]
  
  Forest.UA <- landUse[landUse@data$CODE2012 %in% c(31000), ]
  Wetlands.UA <- landUse[landUse@data$CODE2012 %in% c(40000), ]
  Herbaceous.UA <- landUse[landUse@data$CODE2012 %in% c(32000), ]
  Green.UA <- landUse[landUse@data$CODE2012 %in% c(14100, 14200), ]
  
  Water.UA <- landUse[landUse@data$CODE2012 %in% c(50000), ]
  
  save(Residential.UA, NonResidential.UA, Road.UA, NonRoad.UA, Agriculture.UA, Forest.UA, Wetlands.UA, 
       Herbaceous.UA, Green.UA, Water.UA, file = paste0('RData/LU/LU_', i, '.RData'))
}

rm(Residential.UA, NonResidential.UA, Road.UA, NonRoad.UA, Agriculture.UA, Forest.UA, Wetlands.UA, 
   Herbaceous.UA, Green.UA, Water.UA)