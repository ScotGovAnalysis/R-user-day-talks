# to read in all the datasets for the highcharts

# read in the rds datasets
datasets = readRDS(census_data_path)  

# Read in the map files
datasets[["mapdata"]] <- sf::st_read(shapefile_data, crs = 4326) %>%
  sf::st_transform(crs = 3857)

datasets[["mapjson"]] <- jsonlite::fromJSON(mapsave, simplifyVector = FALSE)
