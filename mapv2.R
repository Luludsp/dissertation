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


sheep_density_nz_df <- merged_data %>%
  as.data.frame(
    xy = T)

head(sheep_density_nz_df)
names(sheep_density_nz_df) [12] <- "sheep"


# Mapping with ggplot

(map <- ggplot( # calling the ggplot package 
  
  # 1. Important bit, read through
  
  sheep_density_nz_df) + # our data frame  
    geom_raster( # command to map raster with ggplot 
      aes(x = x, y = y, # our coordinates 
          fill = sheep) )) # filling it with height

# Customising the filling with our colour gradient 
#  scale_fill_gradientn(name = "Height (m)", colors = texture, breaks = round(breaks, 0)) + # defining title, our palette, and our breaks rounded 

# Defining the coordinates   
#  coord_sf(crs = 3857)) # crs coordinates for our region

sheep_density_rasters <- lapply( # remember our object is a list so we need to use lapply here
  merged_data, # the list of files 
  function(x) { # creating the function 
    terra::crop( # crop function in terra 
      x, # our function's object 
      terra::vect( # way of telling terra that the following object is a vector
        nz_filtered), # the vector of the Abergyll and Bute polygon 
      snap = "in", # keeping eveyrhting that is in the polygon 
      mask = T )})# removing everything outside the polygon so it's not considered 


map_nz <- ggplot() +
  geom_sf(data = merged_data, aes(fill = sheep_density_animals_per_km2_farmland)) +
  scale_fill_gradient(name = expression("Sheep Density per km" ^ "2" * " of farmland"), 
                      labels = scales::comma, 
                      breaks = seq(0, 350, by = 50),
                      low = "#C1E1C1", high = "darkgreen",
                      n.breaks = 7) +
  labs(title = "Sheep Density in New Zealand") +
  
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


