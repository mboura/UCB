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


library(maptools)
library(terra)
library(exactextractr)
library(sf)
library(RColorBrewer)
library(tmap)
library(tmaptools)
library(classInt)
library(stargazer)
library(dplyr)

# New directories ---------------------------------------------------------

for (y in 2010:2014) { dir.create(paste0('Out/CO2Sector_', y), showWarnings = F)}
for (y in 2010:2014) { dir.create(paste0('Out/CO2All_', y), showWarnings = F)}

dir.create('../Out/AGB/', showWarnings = F)
dir.create('../Out/AGBg/', showWarnings = F)
dir.create('../Out/CF/', showWarnings = F)
dir.create('../Out/CF_fua/', showWarnings = F)
dir.create('../Out/DLT/', showWarnings = F)
dir.create('../Out/EZ/', showWarnings = F)
dir.create('../Out/LeafType/', showWarnings = F)
dir.create('../Out/Rforest/', showWarnings = F)
dir.create('../Out/Rforest_fua/', showWarnings = F)
dir.create('../Out/Rwetlands/', showWarnings = F)
dir.create('../Out/Rwetlands_fua/', showWarnings = F)
dir.create('../Out/Rherbaceous/', showWarnings = F)
dir.create('../Out/Rherbaceous_fua/', showWarnings = F)
dir.create('../Out/TCD/', showWarnings = F)

dir.create('../Maps/AGB/', showWarnings = F)
dir.create('../Maps/AGBg/', showWarnings = F)
dir.create('../Maps/CF/', showWarnings = F)
dir.create('../Maps/Climate/', showWarnings = F)
dir.create('../Maps/DLT/', showWarnings = F)
dir.create('../Maps/EZ/', showWarnings = F)
dir.create('../Maps/LeafType/', showWarnings = F)
dir.create('../Maps/Rforest/', showWarnings = F)
dir.create('../Maps/Rforest_fua/', showWarnings = F)
dir.create('../Maps/Rwetlands/', showWarnings = F)
dir.create('../Maps/Rwetlands_fua/', showWarnings = F)
dir.create('../Maps/Rherbaceous/', showWarnings = F)
dir.create('../Maps/Rherbaceous_fua/', showWarnings = F)
dir.create('../MAps/TCD/', showWarnings = F)

# Loading environments ----------------------------------------------------

load('../Typo1/RData/Maps.RData') # loads the SpPolyDF Europe and Boundaries

# Spatial data ------------------------------------------------------------

crs <- CRS('+proj=longlat +datum=WGS84')
crsLambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

EuropeWGS <- spTransform(Europe, crs)
BoundariesWGS <- spTransform(Boundaries, crs)

res <- c(0.125, 0.0625)

layer <- c('Residential', 'NonResidential', 'Road', 'NonRoad', 'Agriculture')

# Presenting the 10 FUAs of focus:
# Lecco (Italy), Kastamonu (Turkey), Belfort (France), Walbrzych (Poland), Plovdiv (Bulgaria), Cadiz (Spain), Subotica (Serbia), Rotterdam (Netherlands), Stoke-on-Trent (United-Kingdom), Bacau (Romania).
# Each of them belong to a disting UFES cluster.

fua <- c('IT060L1', 'TR015L1', 'FR076L2', 'PL511L2', 'BG002L2',
         'ES522L1', 'RS005L1', 'NL003L2', 'UK027L1', 'RO007L1')

Boundaries_fua <- list()
for(i in levels(Boundaries$FUA)) {
  Boundaries_fua[[i]] <- subset(Boundaries, FUA %in% i)
}

Boundaries_cases <- list()
for(i in fua) {
  Boundaries_cases[[i]] <- subset(Boundaries, FUA %in% i)
}



# Loading the characteristics of the vegetated lands ----------------------


## Ecological zones and climatic domains ----



EZ <- readOGR('../Data/GEZ 2010/gez_2010_wgs84.shp')
EZ <- gBuffer(EZ, byid = T, width = 0)
names(EZ)[1] <- 'EZ'
EZ$Climate <- stringr::str_split(EZ$EZ, ' ', simplify = T)[, 1]
EZ <- subset(EZ, Climate %in% c('Polar', 'Boreal', 'Temperate', 'Subtropical', 'Tropical'))
print(EZ)

# 20 ecological domains and 5 climatic domains across the globe.
print(length(unique(EZ$EZ)))
print(unique(EZ$EZ))



### Continental-level ----

# EZ_europe <- raster::intersect(EZ, EuropeWGS)
# EZ_europe <- raster::crop(EZ_europe, extent(-30, 60, 30, 72), snap = 'out')
# # Canary islands are not included in this extent, so they are added a bit lower
# EZ_europe$EZ <- as.factor(as.character(EZ_europe$EZ))
# EZ_europe$Climate <- as.factor(as.character(EZ_europe$Climate))
# EZ_europe <- subset(EZ_europe, select = EZ:Climate)
# tmp <- raster::intersect(EZ, subset(BoundariesWGS, FUA %in% c('ES008L2', 'ES025L2', 'ES072L0', 'ES074L0')))
# EZ_europe <- rbind(EZ_europe, subset(tmp, select = c(EZ:Climate)), makeUniqueIDs = T)
# rm(tmp)
# writeOGR(EZ_europe, 'Out/EZ/', layer = 'EZ_europe',
#            driver = 'ESRI Shapefile', overwrite_layer = T)


### City-level ----

# EZ_fua <- raster::intersect(EZ_europe, BoundariesWGS)
# EZ_fua <- subset(EZ_fua, select = c(EZ:Climate, FUA))
# writeOGR(EZ_fua, 'Out/EZ/', layer = 'EZ_fua',
#            driver = 'ESRI Shapefile', overwrite_layer = T)

EZ_europe <- readOGR('Out/EZ/EZ_europe.shp')
EZ_fua <- readOGR('Out/EZ/EZ_fua.shp')

EZ_caseStudy <- subset(EZ_fua, FUA %in% fua)
# writeOGR(EZ_caseStudy, 'Out/EZ/', layer = 'EZ_caseStudy',
#            driver = 'ESRI Shapefile', overwrite_layer = T)


EZ_fua2 <- list()
for (i in unique(EZ_fua$FUA)) {
  EZ_fua2[[i]] <- subset(EZ_fua, FUA == i)
}

EZ_caseStudy2 <- list()
for (i in unique(EZ_caseStudy$FUA)) {
  EZ_caseStudy2[[i]] <- subset(EZ_caseStudy, FUA == i)
}

EZ_fua2Lambert <- list()
for (i in names(EZ_fua2)) {
  EZ_fua2Lambert[[i]] <- spTransform(EZ_fua2[[i]], crsLambert)
}

## Leaf type ----

# - 0: non-tree covered areas
# - 1: broadleaved trees
# - 2: coniferous trees
# - 254: unclassifiable
# - 255: outside area

DLT <- raster('../Data/HRL 2012 - Forests - Dominant Leaf Type/DLT_2012_020m_eu_03035_d02_Full/DLT_2012_020m_eu_03035_d02_full.tif')


# For memory efficiency, the rasters are only saved locally as tif files, no more within a list as a R object.

for(i in names(Boundaries_fua)) {
  DLT_fua <- crop(DLT, extent(Boundaries_fua[[i]]))
  DLT_fua <- mask(DLT_fua, Boundaries_fua[[i]])
  
  DLT_fua[DLT_fua == 0] <- NA
  DLT_fua[DLT_fua == 254] <- NA
  DLT_fua[DLT_fua == 255] <- NA
  
  # DLT_fua[[i]] <- ratify(DLT_fua[[i]])
  # # leafType <- levels(DLT_fua[[i]])[[1]]
  # levels(DLT_fua[[i]]) <- merge(levels(DLT_fua[[i]])[[1]],
  #                            cbind(ID = c(1, 2), 
  #                                  leafType = c('Broadleaved', 'Coniferous')),
  #                            by = 'ID', all.y = F)
  names(DLT_fua) <- 'DLT'
  
  raster::writeRaster(DLT_fua, overwrite = T, 
                      filename = paste0('Out/DLT/DLT_', i, '.tif'))
}


#### Extract only the ones for the case study

DLT_cases <- list()
for(i in fua) {
  file <- list.files('Out/DLT', pattern = i , recursive = T, full.names = T)
  DLT_cases[[i]] <- raster(file)
}



LeafTypes_cases <- NULL
for (i in names(DLT_cases)) {
  LeafTypes_cases <- rbind(LeafTypes_cases, table(DLT_cases[[i]][]))
}
LeafTypes_cases <- data.frame(LeafTypes_cases)
rownames(LeafTypes_cases) <- names(DLT_cases)
colnames(LeafTypes_cases) <- c('Broadleaved', 'Coniferous')
LeafTypes_cases$All <- LeafTypes_cases$Broadleaved + LeafTypes_cases$Coniferous
LeafTypes_cases
plot(LeafTypes_cases)



LeafTypes_fua <- NULL
for (i in names(Boundaries_fua)) {
  tmp <- raster(list.files('../Out/DLT', pattern = i , recursive = T, full.names = T))
  LeafTypes_fua <- rbind(LeafTypes_fua, table(tmp[]), i)
}
LeafTypes_fua <- data.frame(LeafTypes_fua)
rownames(LeafTypes_fua) <- names(Boundaries_fua)
colnames(LeafTypes_fua) <- c('Broadleaved', 'Coniferous')
LeafTypes_fua$All <- LeafTypes_fua$Broadleaved + LeafTypes_fua$Coniferous
write.csv(LeafTypes_fua, file = 'Tables/LeafTypes_fua.csv')
LeafTypes_fua



## Tree cover ----

TCD <- raster('../Data/HRL 2012 - Forests - Tree Cover Density/TCD_2012_020m_eu_03035_d02_Full/TCD_2012_020m_eu_03035_d02_full.tif')


for(i in names(Boundaries_fua)) {
  TCD_fua <- crop(TCD, extent(Boundaries_fua[[i]]))
  TCD_fua <- mask(TCD_fua, Boundaries_fua[[i]])
  
  TCD_fua[TCD_fua == 254] <- NA
  TCD_fua[TCD_fua == 255] <- NA
  
  names(TCD_fua) <- 'TCD'
  
  raster::writeRaster(TCD_fua, overwrite = T, 
                      filename = paste0('Out/TCD/TCD_', i, '.tif'))
}


TCD_cases <- list()
for(i in fua) {
  file <- list.files('Out/TCD', pattern = i , recursive = T, full.names = T)
  TCD_cases[[i]] <- raster(file)
}


for (i in names(Boundaries_fua)) {
  tmp <- raster(list.files('Out/TCD', pattern = i , recursive = T, full.names = T))
  mapTCD <- tm_shape(tmp) +
    tm_raster(palette = brewer.pal(8, "Greens"),
              title = 'Tree density in %') +
    tm_shape(Boundaries_fua[[i]]) +
    tm_borders() +
    tm_layout(i, outer.margins = 0, legend.position = c('left', 'bottom'))
  tmap_save(mapTCD, paste0('Maps/TCD/TCD_', i, '.pdf'))
}


