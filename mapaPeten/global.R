library(shiny)
library(sp)
library(leaflet)
library(dplyr)
library(tidyr)
library(purrr)
library(sf)

# default data and settings
d <- list()
d$webtext <- read.csv("webtext.csv", 
                      stringsAsFactors = F)

# allows us to split up a cell in a data.frame by semi-colons
fn_split <- function(x){
  strsplit(x, ";")[[1]]
}

# this allows us to reduce the types of data into broad categories
d$var_sets <- read.csv("variable_subset.csv",
                              stringsAsFactors = F)

# this is the translation into the vernacular for variable types
d$dic <- read.csv("dictionary.csv",
                  stringsAsFactors = F)

d$tooltips <- read.csv("tooltips.csv",
                       stringsAsFactors = F)

# Spatial Data
cd <- list()
# peten boundary
cd$gov_b <- st_read(dsn="acdip_pop_project.gpkg", 
                    layer="gov_b",
                    quiet=T)

# census data - lugares poblados
cd$gov_lp_inclusive <- st_read(dsn="acdip_pop_project.gpkg", 
                               layer="gov_lp_inclusive",
                               quiet=T)

cd <- lapply(cd, function(x){
  if(is.na(st_crs(x)$epsg)) {
    x <- x %>%
      st_transform(crs=4326)
  } else if (st_crs(x)$epsg != 4326){
    x <- x %>%
      st_transform(crs=4326)
  }
})

source("fn.R")
source("petenexUI.R")
source("petenex.R")