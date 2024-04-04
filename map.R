## Mapping sheep density in each country 
## UK, Australia, and NZ

# Lucie de Seguins Pazzis 
# 27/03/2023

library(tidyverse) # contains dplyr and ggplot2 - useful for data wrangling and data visualisation
library(sf) # work with polygons 
library(terra) # work with raster files
library(geodata) # administrative data 

# UK Polygon ----

# Creating a function finding the region borders 
get_region_borders_l2 <- function() { # creating a function 
  main_path <- getwd() # main path is our working directory 
  region_borders <- geodata::gadm( # gadm stands for global administrative divisions
    country = "GBR", # ISO 3 code letters for the UK is GBR 
    level = "2", # regional level 
    path = main_path) %>%  # downloading the gadm files onto our laptop 
    
    sf::st_as_sf() # transforming the rds file (standard gadm format) into a sf object
  
  return(region_borders) # return the object we created 
}

# files stored also in the region_borders file  
region_borders <- get_region_borders_l2() 

# looking up the name of the different regions in the UK 
head(unique(region_borders$NAME_2))

# renaming 
uk <- region_borders

plot(sf::st_geometry(uk)) # mapping the UK for each region 


# NZ Polygon ----

get_region_borders_nz <- function() { # creating a function 
  main_path <- getwd() # main path is our working directory 
  region_borders <- geodata::gadm( # gadm stands for global administrative divisions
    country = "NZ", # ISO 3 code letters for the UK is GBR 
    level = "1", # regional level 
    path = main_path) %>%  # downloading the gadm files onto our laptop 
    
    sf::st_as_sf() # transforming the rds file (standard gadm format) into a sf object
  
  return(region_borders) # return the object we created 
}

# files stored also in the region_borders file  
region_borders_nz <- get_region_borders_nz() 

# looking up the name of the different regions in the UK 
head(unique(region_borders_nz$NAME_1))

nz_filtered <- region_borders_nz %>%
  dplyr::filter(!(NAME_1 %in% c("Northern Islands", "Southern Islands", "Chatham Islands")))


plot(sf::st_geometry(nz_filtered)) # mapping the NZ for each region 


# loading the density values for nz 
sheep_density_nz <- read.csv("nz_sheep_density.csv", header = TRUE)

merged_data <- merge(nz_filtered, sheep_density_nz, by.x = "NAME_1", by.y = "NAME_1")



