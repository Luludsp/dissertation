## Mapping sheep density in each country 
## UK, Australia, and NZ

# Lucie de Seguins Pazzis 
# 27/03/2023

library(tidyverse) # contains dplyr and ggplot2 - useful for data wrangling and data visualisation
library(sf) # work with polygons 
library(terra) # work with raster files
library(geodata) # administrative data 

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
sheep_density_nz <- read.csv("map/nz/nz_sheep_density.csv", header = TRUE)

merged_data <- merge(nz_filtered, sheep_density_nz, by.x = "NAME_1", by.y = "NAME_1")


sheep_density_nz_df <- merged_data %>%
  as.data.frame(
    xy = T)

head(sheep_density_nz_df)
names(sheep_density_nz_df) [12] <- "sheep"


# Mapping with ggplot

# Load your location data
location_data <- read.csv("map/nz/location_studies_nz.csv")  # Replace with your file name
# Remove rows with NA values
location_data <- location_data[complete.cases(location_data), ]


map_nz <- ggplot() +
  geom_sf(data = merged_data, aes(fill = sheep_density_animals_per_km2_farmland)) +
  geom_point(data = location_data, aes(x = long, y = lat), color = "red", size = 3, shape = 18) +  # Location points
  scale_fill_gradient(name = expression("Sheep Density per km" ^ "2" * " of farmland"), 
                      labels = scales::comma, 
                      breaks = seq(0, 350, by = 50),
                      low = "#C1E1C1", high = "darkgreen",
                      n.breaks = 7) +
  
  # Defining the legend 
  guides(fill = guide_legend(direction = "vertical", # defining legend for the fill aesthetics 
                             keyheight = unit(3, "mm"),keywidth = unit(3, "mm"), # defining the height and width of the legend widths
                             title.position = "top", label.position = "right", # legend title and labels positions
                             title.hjust = .5, label.hjust = .5, # horizontal title and labels legend position 
                             ncol = 1, # legend in 1 column 
                             byrow = F)) + # asking R to not define them by row 
  
  
  # Changing themes 
  theme_minimal() + # a R built in theme 
  # theme minimal does not remove some of the grids so we'll do that manually 
  theme(axis.line = element_blank(), # removing lines 
        axis.title.x = element_blank(),axis.title.y = element_blank(), # removing x and y titles 
        axis.text.x = element_blank(),axis.text.y = element_blank(), # removing the latitudes, longitudes, and degrees on the x and y axis 
        legend.position = "right", # legend on the right 
        # legend.title = element_text( # customizing the legend title text 
        #   size = 11, color = "grey10"),
        # legend.text = element_text(size = 10, color = "grey10"),
        # Making the background white so it's the same as in the rayshader package 
        panel.grid.major = element_line(color = "white"), panel.grid.minor = element_line(color = "white"), # white the background white 
        plot.background = element_rect(fill = "white", color = NA), legend.background = element_rect(fill = "white", color = NA), # white plot and legend background  
        panel.border = element_rect(fill = NA, color = "white"), # white border 
        plot.margin = unit(c(t = 0, r = 0, b = 0, l = 0), "lines")) # decreasing the white space around the margins 


# Australia Polygon ----

get_region_borders_aus <- function() { # creating a function 
  main_path <- getwd() # main path is our working directory 
  region_borders <- geodata::gadm( # gadm stands for global administrative divisions
    country = "AUS", # ISO 3 code letters for the UK is GBR 
    level = "1", # regional level 
    path = main_path) %>%  # downloading the gadm files onto our laptop 
    
    sf::st_as_sf() # transforming the rds file (standard gadm format) into a sf object
  
  return(region_borders) # return the object we created 
}

# files stored also in the region_borders file  
region_borders_aus <- get_region_borders_aus() 

# looking up the name of the different regions in the UK 
head(unique(region_borders_aus$NAME_1))

aus_filtered <- region_borders_aus %>%
  dplyr::filter(!(NAME_1 %in% c("Ashmore and Cartier Islands", "Coral Sea Islands Territory", "Jervis Bay Territory")))


plot(sf::st_geometry(aus_filtered)) # mapping the NZ for each region 

# loading the density values for aus 
sheep_density_aus <- read.csv("map/aus/sheep_density_aus_2.csv", header = TRUE)

merged_data_aus <- merge(aus_filtered, sheep_density_aus, by.x = "NAME_1", by.y = "NAME_1")


# Load your location data
location_data_aus <- read.csv("map/aus/location_study_aus.csv") 

map_aus <- ggplot() +
  geom_sf(data = merged_data_aus, aes(fill = sheep)) +
  geom_point(data = location_data_aus, aes(x = long, y = lat), color = "red", size = 3, shape = 18) +  # Location points
  scale_fill_gradient(name = expression("Sheep Density per km" ^ "2" * ""), 
                      labels = scales::comma, 
                      breaks = seq(0, 70, by = 20),
                      low = "#C1E1C1", high = "darkgreen",
                      n.breaks = 7) +
  
  # Defining the legend 
  guides(fill = guide_legend(direction = "vertical", # defining legend for the fill aesthetics 
                             keyheight = unit(3, "mm"),keywidth = unit(3, "mm"), # defining the height and width of the legend widths
                             title.position = "top", label.position = "right", # legend title and labels positions
                             title.hjust = .5, label.hjust = .5, # horizontal title and labels legend position 
                             ncol = 1, # legend in 1 column 
                             byrow = F)) + # asking R to not define them by row 
  
  
  # Changing themes 
  theme_minimal() + # a R built in theme 
  # theme minimal does not remove some of the grids so we'll do that manually 
  theme(axis.line = element_blank(), # removing lines 
        axis.title.x = element_blank(),axis.title.y = element_blank(), # removing x and y titles 
        axis.text.x = element_blank(),axis.text.y = element_blank(), # removing the latitudes, longitudes, and degrees on the x and y axis 
        legend.position = "right", # legend on the right 
        # legend.title = element_text( # customizing the legend title text 
        #   size = 11, color = "grey10"),
        # legend.text = element_text(size = 10, color = "grey10"),
        # Making the background white so it's the same as in the rayshader package 
        panel.grid.major = element_line(color = "white"), panel.grid.minor = element_line(color = "white"), # white the background white 
        plot.background = element_rect(fill = "white", color = NA), legend.background = element_rect(fill = "white", color = NA), # white plot and legend background  
        panel.border = element_rect(fill = NA, color = "white"), # white border 
        plot.margin = unit(c(t = 0, r = 0, b = 0, l = 0), "lines")) # decreasing the white space around the margins 


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

region_borders <- region_borders[complete.cases(region_borders), ]
region_borders[!is.na(region_borders)]

# renaming 
uk <- region_borders

plot(sf::st_geometry(uk)) # mapping the UK for each region 



