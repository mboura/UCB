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
library(tmap)
library(tmaptools)

# New directories ---------------------------------------------------------

for (y in 2010:2014) { dir.create(paste0('Maps/TNOraw_', y), showWarnings = F)}

# Loading environments ----------------------------------------------------

load('../Typo1/RData/Maps.RData') # loads the SpPolyDF Europe and Boundaries

# Spatial data ------------------------------------------------------------

res <- c(0.125, 0.0625)

layer <- c('Residential', 'NonResidential', 'Road', 'NonRoad', 'Agriculture')

# Processing the raster files -------------------------------------------------

year <- 2010:2014

for (y in year) {
  print(paste0('Processing year ', y))
  
  LU.stack <- list()
  for (l in layer) {
    LU.stack[[l]] <- raster(paste0('Out/LU_', y, '/TNO.stack_', l, '.tif'))
  }
  LU.stack <- stack(LU.stack)
  res(LU.stack) <- res
  print(LU.stack)
  
  LU.pt <- list()
  for (l in layer[c(2, 4)]) {
    LU.pt[[l]] <- readOGR(paste0('Out/LU_', y, '/TNO.pt_', l, '.shp'))
  }
  print(LU.pt)
  
  map <- tm_shape(LU.stack[['Residential']]) + 
    tm_raster(palette = '-magma', n = 10, style = 'kmeans',
              title = 'Residential LU \nkton CO2 per year per cell')
  tmap_save(map, filename = paste0('Maps/TNOraw_', y, '/TNO_Residential.pdf'), asp = 0)
  
  map <- tm_shape(LU.stack[['NonResidential']]) + 
    tm_raster(palette = '-magma', n = 10, style = 'kmeans',
              title = 'NonResidential LU \nkton CO2 per year per cell')
  tmap_save(map, filename = paste0('Maps/TNOraw_', y, '/TNO_NonResidential.pdf'), asp = 0)
  
  map <- tm_shape(LU.pt[['NonResidential']]) + 
    tm_dots('CO2', palette = '-magma', n = 10, style = 'kmeans',
            title = 'NonResidential LU \nkton CO2 per year per location')
  tmap_save(map, filename = paste0('Maps/TNOraw_', y, '/TNO_NonResidential_pt.pdf'), asp = 0)
  
  map <- tm_shape(LU.stack[['Road']]) + 
    tm_raster(palette = '-magma', n = 10, style = 'kmeans',
              title = 'Road LU \nkton CO2 per year per cell')
  tmap_save(map, filename = paste0('Maps/TNOraw_', y, '/TNO_Road.pdf'), asp = 0)
  
  map <- tm_shape(LU.stack[['NonRoad']]) + 
    tm_raster(palette = '-magma', n = 10, style = 'kmeans',
              title = 'NonRoad LU \nkton CO2 per year per cell')
  tmap_save(map, filename = paste0('Maps/TNOraw_', y, '/TNO_NonRoad.pdf'), asp = 0)
  
  map <- tm_shape(LU.pt[['NonRoad']]) + 
    tm_dots('CO2', palette = '-magma', n = 10, style = 'kmeans',
            title = 'NonRoad LU \nkton CO2 per year per location')
  tmap_save(map, filename = paste0('Maps/TNOraw_', y, '/TNO_NonRoad_pt.pdf'), asp = 0)
    
  map <- tm_shape(LU.stack[['Agriculture']]) + 
    tm_raster(palette = '-magma', n = 10, style = 'kmeans',
              title = 'Agriculture LU \nkton CO2 per year per cell')
  tmap_save(map, filename = paste0('Maps/TNOraw_', y, '/TNO_Agriculture.pdf'), asp = 0)
  
}
