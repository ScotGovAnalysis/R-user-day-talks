# this script loads and sets all the variables for the presentation
# 
# Load libraries ----------------------------------------------------------
library(readxl)
library(tidyverse)
library(highcharter)
library(lubridate)
library(tidyr)
library(purrr)
library(dplyr)
library(sf)
library(rlist)

## Highcharts ------------------------------------------------------------

# Themes for highcharts
hc_theme <- hc_theme(
  chart = list(style = list(fontFamily = windowsFont("Arial")),
               animation = F),
  xAxis = list(labels = list(style = list(fontSize = "14pt")),
               title = list(style = list(fontSize = "14pt")),
               tickLength = 0,
               lineColor = "transparent"),
  yAxis = list(labels = list(style = list(fontSize = "14pt")),
               title = list(style = list(fontSize = "14pt")),
               gridLineWidth = 0),
  legend = list(symbolWidth = 40,
                itemStyle = list(fontSize = "14pt",
                                 fontStyle = "normal",
                                 fontWeight = "light")),
  plotOptions = list(line = list(marker = list(enabled = F),
                                 lineWidth = 4,
                                 clip = F),
                     arearange = list(lineWidth = 0,
                                      marker = list(enabled = F))))


my_theme <- hc_theme(
  chart = list(
    style = list(
      fontFamily = "Roboto"
    )
  )
)


# Globally format thousands separator for highcharts
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)

## Maps ------------------------------------------------------------

# This is a shapefile of Scottish local authorities taken from ONS and then simplified and converted to geojson (Nick fenton)
shapefile_data <- "data/scottish_local_authorities_simplified.geojson"
# There's a few things needed to show this map in Highcharts:
# 1. Need to convert from EPSG:4326 to EPSG:3857, otherwise it comes out stretched
# 2. Save to geojson
# 3. Read geosjson as list for highcharter
# 4. Need to make up some data to map, otherwise hc throws a fit
mapsave <- "data/shapefile.geojson"


## data set file paths ------------------------------------------------------------
census_data_path = "data/census_datasets.rds"

# Style colors ------------------------------------------------------------

col_grey_dark <- "#333333"
col_grey_light <- "#949494"
col_nrs_purple <- "#6C297F"
col_nrs_light <- "#BF78D3"
col_gss_orange <- "#F46A25"
col_nrs_adjusted <- "#4e0064"

gss_palette <- list(
  dark_blue	= "#12436D",
  turquoise	= "#28A197",
  dark_pink	= "#801650",
  orange = "#F46A25",
  dark_grey	= "#3D3D3D",
  light_purple = "#A285D1",
  census_blue = "#005390")
