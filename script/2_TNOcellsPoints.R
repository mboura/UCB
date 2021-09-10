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

# New directories ---------------------------------------------------------

dir.create('Maps', showWarnings = F)
dir.create('Maps/TNOpoints', showWarnings = F)
dir.create('Out', showWarnings = F)
for (y in 2010:2014) { dir.create(paste0('Out/SNAP_', y), showWarnings = F)}
for (y in 2010:2014) { dir.create(paste0('Out/LU_', y), showWarnings = F)}

# Loading environments ----------------------------------------------------

load('../Typo1/RData/Maps.RData') # loads the SpPolyDF Europe and Boundaries


# Spatial data ------------------------------------------------------------

crs <- CRS('+proj=longlat +datum=WGS84')
crsLambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

EuropeWGS <- spTransform(Europe, crs)
BoundariesWGS <- spTransform(Boundaries, crs)

res <- c(0.125, 0.0625)
nlat <- 672
nlon <- 720

layer <- c('Residential', 'NonResidential', 'Road', 'NonRoad', 'Agriculture')

# Processing the csv files -------------------------------------------------

year <- 2010:2014

for (y in year) {
  print(paste0('Processing year ', y))
  
  TNO <- read.csv(paste0('Tables/TNOraw/TNO_', y, '.csv'))
  print(head(TNO))
  

  # Areal data source -------------------------------------------------------
  
  # ----- Spatial pixel DF ----- #
  TNO.pxl <- subset(TNO, type == 'area')
  coordinates(TNO.pxl) <- ~ lon_value + lat_value
  proj4string(TNO.pxl) <- crs
  TNO.pxl$SNAP_name2 <- as.factor(as.character(TNO.pxl$SNAP_name))
  print(kable(cbind(NbCells = table(TNO.pxl$SNAP_name))))
  
  TNO.pxl <- SpatialPixelsDataFrame(points = TNO.pxl@coords, data = TNO.pxl@data, 
                                    tolerance = 1/16, proj4string = crs)
  print(summary(TNO.pxl))
  
  
  # ----- Exporting CO2 emissions per SNAP ----- #
  
  SNAP.stack <- list()
  for (i in unique(TNO.pxl$SNAP_code)) {
    SNAP.stack[[paste0('SNAP', i)]] <- stack(subset(TNO.pxl, SNAP_code == i))
    writeRaster(SNAP.stack[[paste0('SNAP', i)]]$CO2, 
                filename = paste0('Out/SNAP_', y, '/TNO.stack_SNAP', i, '.tif'), 
                'GTiff', overwrite = T)
  }
  
  # ----- Exporting CO2 emissions per LU category ----- #
  
  # WGS
  Resid <- overlay(SNAP.stack[['SNAP2']]$CO2, fun = sum)
  NonResid <- overlay(SNAP.stack[['SNAP34']]$CO2, SNAP.stack[['SNAP5']]$CO2, SNAP.stack[['SNAP6']]$CO2, fun = sum) # no cell for SNAP1, only points
  Road <- overlay(SNAP.stack[['SNAP71']]$CO2, SNAP.stack[['SNAP72']]$CO2, SNAP.stack[['SNAP73']]$CO2, fun = sum)
  NonRoad <- overlay(SNAP.stack[['SNAP8']]$CO2, fun = sum)
  Agri <- overlay(SNAP.stack[['SNAP10']]$CO2, fun = sum)
  
  LU.stack <- stack(Resid, NonResid, Road, NonRoad, Agri)
  names(LU.stack) <- layer
  res(LU.stack) <- res
  print(LU.stack)
  
  # Lambert
  LU.stackLambert <- stack(projectRaster(Resid, crs = crsLambert), 
                           projectRaster(NonResid, crs = crsLambert), 
                           projectRaster(Road, crs = crsLambert), 
                           projectRaster(NonRoad, crs = crsLambert), 
                           projectRaster(Agri, crs = crsLambert))
  names(LU.stackLambert) <- layer
  res(LU.stackLambert) <- res
  print(LU.stackLambert)
  
  # Exporting
  writeRaster(LU.stack, filename = paste0('Out/LU_', y, '/TNO.stack.tif'), 
              'GTiff', bylayer = T, suffix = 'names', overwrite = T)
  writeRaster(LU.stackLambert, filename = paste0('Out/LU_', y, '/TNO.stackLambert.tif'), 
              'GTiff', bylayer = T, suffix = 'names', overwrite = T)

  # Point data source -------------------------------------------------------

  TNO.pt <- subset(TNO, type == 'point')
  coordinates(TNO.pt) <- ~ lon_value + lat_value
  proj4string(TNO.pt) <- crs
  # View(TNO.pt@data)
  print(summary(TNO.pt@data))
  TNO.pt$SNAP_name2 <- as.factor(as.character(TNO.pt$SNAP_name))
  
  print(kable(cbind(NbPoints = table(TNO.pt$SNAP_name))))
  
  pdf(paste0('Maps/TNOpoints/quickMapTNO_', y, '.pdf'))
  spplot(TNO.pt, 'SNAP_name2', cex = .5)
  dev.off()
  
  # SNAP.pt is a list of spatial point data.frames for the 5 SNAP available for points: 
  # Energy, Production and Distribution, Industry, Non-road transport and Waste.
  # They are all exported as infividual shapefiles.
  SNAP.pt <- list()
  for (i in unique(TNO.pt$SNAP_code)) {
    SNAP.pt[[paste0('SNAP', i)]] <- subset(TNO.pt, SNAP_code == i)
    writeOGR(SNAP.pt[[paste0('SNAP', i)]], paste0('Out/SNAP_', y, '/'),
             layer = paste0('TNO.pt_SNAP', i), 
             driver = 'ESRI Shapefile', overwrite_layer = T)
  }
  
  # We plot the CO2 emissions kernel density with bandidth h = res * 10
  print(names(SNAP.pt))
  pdf(paste0('Maps/TNOpoints/quickMapTNO_', y, 'SNAP.pdf'))
  for (i in names(SNAP.pt)) {
    coord <- coordinates(SNAP.pt[[i]])
    k = kde2d(coord[, 1], coord[, 2], h = res * 10, n = c(nlon, nlat))
    r = raster(k)
    plot(r, main = i, col = brewer.pal(9, "Reds"), zlim = c(0, .055))
    plot(BoundariesWGS, border = 'lightgrey', lwd = .4, add = T)
    plot(EuropeWGS, add = T, cex = .5)
  }
  dev.off()
  
  # The point data are now gathered by LU categories: non-residential and non-road. 
  # The corresponding shapefile are then exported.
  # SNAP9 (waste) is not included in the analysis.
  LU.pt <- list()
  LU.pt[['NonResidential']] <- SNAP.pt[['SNAP1']]+ SNAP.pt[['SNAP34']] + SNAP.pt[['SNAP5']]
  LU.pt[['NonRoad']] <- SNAP.pt[['SNAP8']]
  
  writeOGR(LU.pt[['NonResidential']], paste0('Out/LU_', y, '/'),
           layer = 'TNO.pt_NonResidential', 
           driver = 'ESRI Shapefile', overwrite_layer = T)
  writeOGR(LU.pt[['NonRoad']], paste0('Out/LU_', y, '/'),
           layer = 'TNO.pt_NonRoad', 
           driver = 'ESRI Shapefile', overwrite_layer = T)
  
  
  pdf(paste0('Maps/TNOpoints/quickMapTNO_', y, 'LU.pdf'))
  for (i in names(LU.pt)) {
    coord <- coordinates(LU.pt[[i]])
    k = kde2d(coord[, 1], coord[, 2], h = res * 10, n = c(nlon, nlat))
    r = raster(k)
    plot(r, main = i, col = brewer.pal(9, "Reds"), zlim = c(0, .055))
    plot(EuropeWGS, add = T, cex = .5)
  }
  dev.off()

}





