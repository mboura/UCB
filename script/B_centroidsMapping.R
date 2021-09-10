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
library(tmap) # for data(World)
library(tmaptools)
library(sf) # for as_spatial()

# New directories ---------------------------------------------------------

dir.create('Maps/General', showWarnings = F)

# Loading environments ----------------------------------------------------

load('../Typo1/RData/Maps.RData') # loads the SpPolyDF Europe and Boundaries


# Spatial data ------------------------------------------------------------

crs <- CRS('+proj=longlat +datum=WGS84')
crsLambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

EuropeWGS <- spTransform(Europe, crs)
BoundariesWGS <- spTransform(Boundaries, crs)

res <- c(0.125, 0.0625)

layer <- c('Residential', 'NonResidential', 'Road', 'NonRoad', 'Agriculture')

# ----- Background map ----- #

data(World)
Europe <- subset(World, sovereignt %in% intersect(Boundaries$countryName, World$sovereignt) |  
                   continent == 'Europe', 
                 select = iso_a3:continent)
Europe <- spTransform(as_Spatial(Europe), proj4string(Boundaries))
NotEurope <- subset(World, continent == 'Africa' | continent == 'Asia', 
                    select = iso_a3:continent)
NotEurope <- spTransform(as_Spatial(NotEurope), proj4string(Boundaries))
rm(World)

Europe.WGS <- spTransform(Europe, crs)
NotEurope.WGS <- spTransform(NotEurope, crs)


# ----- Centroids of FUAs ----- #

Centroids <- gCentroid(Boundaries, byid = T)[-c(188, 204, 217, 226, 229, 231)]
CentroidsWGS <- gCentroid(BoundariesWGS, byid = T)[-c(188, 204, 217, 226, 229, 231)]
cities <- Boundaries$CITIES[-c(188, 204, 217, 226, 229, 231)]

# FUAs for case study
# Lecco (Italy), Kastamonu (Turkey), Belfort (France), Walbrzych (Poland), 
# Plovdiv (Bulgaria), Cadiz (Spain), Subotica (Serbia), Rotterdam (Netherlands), 
# Stoke-on-Trent (United-Kingdom), Bacau (Romania)
# Each of them belong to a disting UFES cluster.
fua <- c('IT060L1', 'TR015L1', 'FR076L2', 'PL511L2', 'BG002L2',
         'ES522L1', 'RS005L1', 'NL003L2', 'UK027L1', 'RO007L1')

Centroids_cases <- gCentroid(subset(Boundaries, FUA %in% fua), byid = T)
CentroidsWGS_cases <- gCentroid(subset(BoundariesWGS, FUA %in% fua), byid = T)
cities_cases <- subset(Boundaries, FUA %in% fua)$CITIES
cities_cases <- str_replace(cities_cases, '�', 'a')
print(cities_cases)

# ----- Mapping the study areas ----- #

pdf('Maps/General/All.pdf', width = 6, height = 4)
par(mai = rep(0, 4))
plot(NotEurope, col = 'darkgrey', border = NA, bg = 'lightgrey',
     xlim = c(2.3e6, 6.3e6), ylim = c(1e6, 5.5e6))
plot(Europe, add = T, col = 'white', border = 'lightgrey')
plot(Centroids, pch = 19, col = 2, cex = .4, add = T)
legend('bottomleft', legend = 'FUAs', pch = 19, col = 2, bg = 'white', cex = .6)
scalebar(1000000, label = c(0, '', '1000 km'), xy = c(2.7e6, .9e6),
         type = 'bar', divs = 2, cex = .6)
dev.off()

# ----- Mapping the cases of study ----- #

pdf('Maps/General/Cases.pdf', width = 6, height = 4)
par(mai = rep(0, 4))
plot(NotEurope, col = 'darkgrey', border = NA, bg = 'lightgrey',
     xlim = c(2.3e6, 6.3e6), ylim = c(1e6, 5.5e6))
plot(Europe, add = T, col = 'white', border = 'lightgrey')
plot(Centroids_cases, pch = 19, col = 2, add = T)
text(coordinates(Centroids_cases), as.character(cities_cases), pos = 4, cex = .7)
legend('bottomleft', legend = 'Case studies', pch = 19, col = 2, bg = 'white', cex = .6)
scalebar(1000000, label = c(0, '', '1000 km'), xy = c(2.7e6, .9e6),
         type = 'bar', divs = 2, cex = .6)
dev.off()

pdf('Maps/General/CasesWGS.pdf', width = 6, height = 4)
par(mai = rep(0, 4))
plot(NotEurope.WGS, col = 'darkgrey', border = NA, bg = 'lightgrey',
     xlim = c(-20, 40), ylim = c(35, 55))
plot(Europe.WGS, add = T, col = 'white', border = 'lightgrey')
plot(CentroidsWGS_cases, pch = 19, col = 2, add = T)
text(coordinates(CentroidsWGS_cases), as.character(cities_cases), pos = 4, cex = .7)
legend('bottomleft', legend = 'Case studies', pch = 19, col = 2, bg = 'white', cex = .6)
scalebar(1000000, label = c(0, '', '1000 km'), xy = c(18, 32),
         type = 'bar', divs = 2, cex = .6)
dev.off()