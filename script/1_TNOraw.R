rm(list = ls())


# Libraries ---------------------------------------------------------------

library(ncdf4)
library(knitr)

# New directories ---------------------------------------------------------

dir.create('Tables', showWarnings = F)
dir.create('Tables/TNOraw', showWarnings = F)

# Processing the ncdf files -----------------------------------------------

year <- 2010:2014

for (y in year) {
  print(paste0('Processing year ', y))
  

  # Opening the file for reading --------------------------------------------
  
  nc <- nc_open(paste0('../Data/TNO/TNO_CAMS_GHG_grids_', y, '.nc'), readunlim = F)
  
  # We are only interested in latitude, longitude. 
  # The other 10 dimensions information cannot be retrieved from here (error with the original dataset).
  
  # Latitude
  lat <- ncvar_get(nc, 'latitude') 
  nlat <- dim(lat)
  # Longitude
  lon <- ncvar_get(nc, 'longitude') 
  nlon <- dim(lon)
  # Latitude (672 degrees_north): regular grid over [30.000000,72.000000] with 0.062500 resolution (1/16 degrees)
  # Longitude (720 degrees_east): regular grid over [-30.000000,60.000000] with 0.125000 resolution (1/8 degrees)
  print(paste('Latitude:', nlat, ncatt_get(nc, 'latitude', 'units')$value, '-', ncatt_get(nc, 'latitude', 'description')$value))
  print(paste('Longitude:', nlon, ncatt_get(nc, 'longitude', 'units')$value, '-', ncatt_get(nc, 'longitude', 'description')$value))

  # Extracting the variables ------------------------------------------------
  
  
  # names(nc$var)
  
  # ----- Countries ----- #
  
  country_index <- ncvar_get(nc, 'country_index') # 1608665
  country_id <- ncvar_get(nc, 'country_id') # 48
  # country_name <- ncvar_get(nc, 'country_name') # 48
  
  N <- length(country_index)
  index <- 1:N
  # The variables are indexed over 1608665 for Y2012 values corresponding to each spatial unit present.
  
  temp <- data.frame(index = index, 
                     country_id = country_index)
  country <- data.frame(country_id = 1:length(country_id), 
                        country_code = country_id)
  country <- merge(country, temp, by = 'country_id')
  # unique(country$country_code)
  # In the data.frame *country*, the variable *country_code* is the ISO3 country code (48 modalities), 
  # *country_id* is the id number of the country code, 
  # *index* is the cell value index.

  
  
  # ----- Emission sectors (11 SNAP groups) ----- #
  
  emission_category_index <- ncvar_get(nc, 'emission_category_index') # 1608665
  emis_cat_shortsnap <- ncvar_get(nc, 'emis_cat_shortsnap') # 11
  emis_cat_name <- ncvar_get(nc, 'emis_cat_name') # 11
  
  temp <- data.frame(SNAP_id = 1:length(emis_cat_shortsnap),
                     SNAP_code = emis_cat_shortsnap,
                     SNAP_name = emis_cat_name)
  SNAP <- data.frame(index = index, 
                     SNAP_id = emission_category_index)
  SNAP <- merge(SNAP, temp, by = 'SNAP_id')
  
  print(kable(cbind(SNAPname = unique(SNAP$SNAP_name), 
                    SNAPcode = unique(SNAP$SNAP_code)), 
              caption = 'SNAP categories available'))
  
  
  # ----- Coordinates -----  #
  
  latitude_index <- ncvar_get(nc, 'latitude_index') # 1608665 - index in [1, 672] - Y2012: one weid value (-33), for index value 778753
  latitude_source <- ncvar_get(nc, 'latitude_source') # 1608664 - values in [30, 72] - Y2012: the last one is at 27.93286
  latitude_bounds <- t(ncvar_get(nc, 'latitude_bounds')) # 672 2

  latGrid <- data.frame(lat_index = 1:length(lat), 
                        latCenter = lat, 
                        latLower = latitude_bounds[,1], 
                        latUpper = latitude_bounds[,2])
  latitude <- data.frame(index = index, 
                         lat_index = latitude_index, 
                         lat_value = latitude_source)
  # latitude[latitude$index == 778753, ]
  latitude <- merge(latitude, latGrid, by = 'lat_index')

  longitude_index <- ncvar_get(nc, 'longitude_index') # 1608665 - index in [1, 720]
  longitude_source <- ncvar_get(nc, 'longitude_source') # 1608665 - values in [-30, 60]
  longitude_bounds <- t(ncvar_get(nc, 'longitude_bounds')) # 720 2

  lonGrid <- data.frame(lon_index = 1:length(lon), 
                        lonCenter = lon, 
                        lonLower = longitude_bounds[,1], 
                        lonUpper = longitude_bounds[,2])
  longitude <- data.frame(index = index, 
                          lon_index = longitude_index, 
                          lon_value = longitude_source)
  longitude <- merge(longitude, lonGrid, by = 'lon_index')
  
  

  # ----- CO2 emissions values ----- #
    
  co2_bf <- ncvar_get(nc, 'co2_bf') # 1608665
  co2_ff <- ncvar_get(nc, 'co2_ff') # 1608665
  # ncatt_get(nc, 'co2_bf', "units")$value
  # In kg per year per cell
  
  co2 <- data.frame(index = index, 
                    CO2_bf = co2_bf, 
                    CO2_ff = co2_ff, 
                    CO2 = co2_bf + co2_ff)
  print(summary(co2))
  
  
  # ----- Source type (cell or point) ----- #
  
  source_type_index <- ncvar_get(nc, 'source_type_index')
  source <- data.frame(index = 1:N,
                       type = ifelse(source_type_index == 1, 'area', 'point'))
  print(table(source$type))

  
  
  # Closing the file for reading --------------------------------------------
  
  nc_close(nc)

  
  # Formatting the data -----------------------------------------------------

  TNO <- Reduce(function(x, y) merge(x, y, by = 'index'), 
                list(co2, SNAP, country, latitude, longitude, source))
  
  # ----- Conversion ----- #
  
  TNO$CO2_ff <- TNO$CO2_ff / 1000000 # from kg to kton
  TNO$CO2_bf <- TNO$CO2_bf / 1000000 # from kg to kton
  TNO$CO2 <- TNO$CO2 / 1000000 # total amount of CO2 emitted
  
  print(head(TNO))

  # ----- Classes ----- #
  
  TNO$SNAP_name <- as.factor(TNO$SNAP_name)
  TNO$country_code <- as.factor(TNO$country_code)
  TNO$type <- as.factor(TNO$type)
  
  print(summary(TNO))

  # Exporting the data ------------------------------------------------------
  write.csv(TNO, file = paste0('Tables/TNOraw/TNO_', y, '.csv'), row.names = F)
  
}