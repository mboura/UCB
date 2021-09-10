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

for (y in 2010:2014) { dir.create(paste0('Out/CO2Sector_point_', y), showWarnings = F)}

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
  
  LU.pt <- list()
  
  for (l in layer[c(2, 4)]) {
    LU.pt[[l]] <- spTransform(readOGR(paste0('Out/LU_', y, '/TNO.pt_', l, '.shp')), crsLambert)
  }
  
  print(LU.pt)
  
  ### Extract point values and attribute it to raster cell from corresponding LU
  # - LU.stack.Residential[[i]]: cropped raster data for FUA i and LU Residential
  # - Residential.UA[[i]]$CO2: CO2 emissions for FUA i in Residential sector
  
  # ES008L2, ES025L2, ES045L0, ES062L0, ES072L0, ES074L0
  for (i in levels(BoundariesWGS$FUA)[-c(188, 204, 217, 226, 229, 231)]) {
    print(i)
    
    load(paste0('RData/LU.cropLambert/LU.cropLambert_', i, '.RData'))
    r <- raster(ext = extent(LU.cropLambert), res = 100, crs = crsLambert)
    r0 <- r; r0[] <- NA
    
    load(paste0('RData/LU/LU_', i, '.RData'))
    
    CO2.point.NonResidential <- intersect(LU.pt[['NonResidential']], LU.cropLambert)
    CO2.point.NonResidential <- if (dim(CO2.point.NonResidential) != 0) {
      rasterize(CO2.point.NonResidential, r, field = 'CO2', fun = 'sum')
      } else {r0}
    
    CO2.point.NonRoad <- intersect(LU.pt[['NonRoad']], LU.cropLambert)
    CO2.point.NonRoad <- if (dim(CO2.point.NonRoad) != 0) {
      rasterize(CO2.point.NonRoad, r, field = 'CO2', fun = 'sum')
    } else {r0}
    
    
    CO2.point <- stack(CO2.point.NonResidential, CO2.point.NonRoad)
    names(CO2.point) <- layer[c(2, 4)]
    print(CO2.point)
    
    writeRaster(CO2.point, filename = paste0('Out/CO2Sector_point_', y, '/Emissions_', i, '.tif'), overwrite = T)
    
  }
}