## Analyzing sheep farming impacts in the UK, Australia, and NZ


# Lucie de Seguins Pazzis 
# 12/04/2023

##### PREPARATION

# loading libraries 
library(tidyverse)



# loading the data 
data <- read.csv("analysis/compiled_data_impacts_externalities.csv", header = TRUE)

##### RQ1 
# What are the different impacts between the three countries? How can they be categorized groups? 

hist(data$impact)

##### RQ2 

# Making a Marginal abatement cost curve (MACC)

install.packages("devtools")
devtools::install_github("aj-sykes92/ggmacc")


social_cost_of_carbon <- 0

full_macc <- uk_agroforestry %>%
  ggmacc(abatement = co2_tyear, mac = mac_gbp_tco2, fill = crop, cost_threshold = social_cost_of_carbon,
         zero_line = TRUE, threshold_line = TRUE, threshold_fade = 0.3)

full_macc

# install.packages("devtools")
devtools::install_github("G-Thomson/Manu")

full_macc +
  scale_x_continuous(labels = scales::number_format()) +
  scale_fill_manual(values = Manu::get_pal("Kea")) +
  labs(title = "Marginal abatement cost curve for UK agroforestry",
       fill = "Crop type",
       x = expression("Abatement (tonnes CO"[2]*"-eq)"),
       y = expression("Marginal abatement cost (GBP tonne CO"[2]*"-eq"^{-1}*")")
  ) +
  theme_classic()

# install.packages("devtools")
devtools::install_github("aj-sykes92/ggmacc")


#' @title A marginal abatement cost curve geom
#' @description A `ggplot2` geom with aesthetic mappings linked to the output of `macc_prep`. Add to
#' a `ggplot` object to build a marginal abatement cost curve. A pre-parameterised wrapper for
#' `geom_rect`.
#' @param fill An optional parameter to specify fill groupings to be used by `geom_macc`.
#' @param ... Any additional arguments to pass to `geom_rect`.
#' @import ggplot2
#' @export
geom_macc <- function(fill = NULL, ...) {
  geom_rect(
    aes(xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = {{ fill }},
    ),
    ...)
}

ggmacc <- function(data, mac, abatement, fill = NULL, cost_threshold = NULL,
                   zero_line = FALSE, threshold_line = FALSE, threshold_fade = 1) {
  
  # x-axis hline
  if (zero_line == TRUE) {
    zero_hline <- geom_hline(yintercept = 0, lty = 1, colour = "black")
  } else {
    zero_hline <- NULL
  }
  
  # cost theshold hline
  if (threshold_line == TRUE) {
    if (is.null(cost_threshold)) abort("No cost threshold supplied.")
    cost_hline <- geom_hline(yintercept = cost_threshold, lty = 2, colour = "black")
  } else {
    cost_hline <- NULL
  }
  
  # prepare data for macc
  data <-   data %>%
    macc_prep(mac = {{ mac }}, abatement = {{ abatement }})
  
  # set threshold alpha
  if (!is.null(cost_threshold)) {
    alpha <- ifelse(pull(data, {{ mac }}) >= cost_threshold, threshold_fade, 1)
  } else {
    alpha <- rep(1, nrow(data))
  }
  
  # plot
  data %>%
    ggplot() +
    geom_macc(fill = {{ fill }}, alpha = alpha) +
    zero_hline +
    cost_hline
}

macc_prep <- function(data, mac, abatement) {
  data %>%
    arrange({{ mac }}) %>%
    mutate(xmax = cumsum({{ abatement }}),
           xmin = lag(.data$xmax, default = 0),
           ymin = ifelse({{ mac }} < 0, {{ mac }}, 0),
           ymax = ifelse({{ mac }} > 0, {{ mac }}, 0))
}

small_example %>%
  macc_prep(mac = mac, abatement = abatement) %>%
  ggplot() +
  geom_macc(fill = cat)



# okay so i need to rank them first and then i can organise them 


# Assuming your dataframe is named 'data' and the column containing the values is named 'id_ha_yr'

# Create a new column for the rankings
data$ranking <- rank(data$id_ha_yr)

# If you want to sort the values before ranking, you can do it like this:
sorted_data <- data[order(data$id_ha_yr),]
sorted_data$ranking <- rank(sorted_data$id_ha_yr)

# If you want the rankings to be from 1 to the number of values (lowest to highest),
# you can use the option 'ties.method = "first"' in the rank() function
data$ranking <- rank(data$id_ha_yr, ties.method = "first")

# If you want the rankings to start from 1 for the lowest value and increase
# sequentially to the highest value (both negative and positive),
# you can do it like this:
sorted_data <- data[order(abs(data$id_ha_yr)),]
sorted_data$ranking <- seq_len(nrow(data))

data_no_mic <- data %>%
  filter(!(impact == "supports microbes"))
data_no_mic <- data %>%
  filter(impact != "supports microbes") %>%
  filter(impact != "supports microbes")

data_not_mic <- data[data$impact != "supports microbes", ]

rows_to_remove <- c(23, 32)  # Replace row_number_1 and row_number_2 with the actual row numbers you want to remove

# Remove the specified rows
data_not_mic <- data[-rows_to_remove, ]

data_not_mic <- data$impact

data_not_mic %>%
  macc_prep(mac = id_ha_yr, abatement = ranking) %>%
  ggplot() +
  geom_macc(fill = country)


full_macc <- data_not_mic %>%
  ggmacc(abatement = ranking, mac = id_ha_yr, fill = subtype, cost_threshold = social_cost_of_carbon,
         zero_line = TRUE, threshold_line = TRUE)

full_macc

full_macc +
  scale_x_continuous(labels = scales::number_format()) +
  scale_fill_manual(values = package) +
  labs(fill = "Impact",
       x = expression("Abatement (tonnes CO"[2]*"-eq)"),
       y = expression("Marginal abatement cost (GBP tonne CO"[2]*"-eq"^{-1}*")")
  ) +
  theme_classic()


# Define your initial color palette
initial_palette <- c("#7D9D33", "#CED38C", "#DCC949", "#BCA888", "#CD8862", "#775B24")

# Create a function to generate more colors based on the initial palette
generate_palette <- function(initial_palette, n) {
  ramp <- colorRamp(initial_palette)
  colors <- ramp(seq(0, 1, length.out = n))
  return(colors)
}

# Generate two additional colors that go well with the initial palette
additional_colors <- generate_palette(initial_palette, 2)

# Combine the initial palette with the additional colors
final_palette <- c(initial_palette, additional_colors)

# Create a vector with the names of the colors
palette_names <- c("color1", "color2", "color3", "color4", "color5", "color6", "color7", "color8")

# Create a named vector with the final palette
palette_vector <- setNames(final_palette, palette_names)

# Create a package with the palette
package <- list(palette = palette_vector)

# Print the package
print(package)


