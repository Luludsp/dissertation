## Analyzing sheep farming impacts in the UK, Australia, and NZ


# Lucie de Seguins Pazzis 
# 12/04/2023

##### PREPARATION

# loading libraries 
library(tidyverse)
library(nortest)
library(colorspace)
# other libraries? 

# loading the data 
data <- read.csv("analysis/compiled_data_impacts_externalities.csv", header = TRUE)
head(data)
str(data)

# Converting the charcater values into factors 
data <- data %>%
  mutate(
    impact = as.factor(impact), 
    difference_with_baseline = as.factor(difference_with_baseline),
    country = as.factor(country), 
    cause = as.factor(cause),
    type = as.factor(type), 
    subtype = as.factor(subtype),
    quality_assessment = as.factor(quality_assessment),
    sign = as.factor(sign),
    ecosystem = as.factor(ecosystem),
    pasture_type = as.factor(pasture_type),
    farming_management = as.factor(farming_management),
         )


## Data set without microbes values 

# Rows containing the impact "supports microbes" 
rows_to_remove <- c(23, 32) # rows 23 & 32 
# Remove the specified rows
data_not_mic <- data[-rows_to_remove, ] # creating a new data set without these rows 


## Aesthetics 
# Creating my palette from Manu package (they are inspired from pretty kiwi birds' plumage)
birds <- c("#44781E", "#7D9D33", "#CED38C", "#DCC949", "#BCA888", "#CD8862", "#775B24", "#7A3520")


##### RQ1 
# What are the different impacts between the three countries? How can they be categorized groups? 






##### RQ2 

### Do the externalities between the three countries differ? 

#  checking for normality 
# visually 
hist(data$id_ha_yr) # not normal at all 
hist(data_not_mic$id_ha_yr) # looks like it could be normal 

# test 
shapiro.test(p_resids)

#linear model 
country_lm <- lm(id_ha_yr~country, data=data_not_mic)
anova(country_lm)
#summary of the function 
summary(country_lm)

p_resids <- resid(country_lm)
#fct shapiro.test() used to test the normality of the residuals
shapiro.test(p_resids)
#fct bartlett.test() check equality of variances 
bartlett.test(id_ha_yr ~ country,data= data_not_mic)
plot(country_lm)

wilcox.test(data_not_mic$country, data_not_mic$id_ha_yr)

# summing externalities

sum_externalities <- data_not_mic %>%
  group_by(country) %>%
  summarize(Sum_Externalities = sum(id_ha_yr))

str(sum_externalities)

# Perform Kruskal-Wallis test
kruskal.test(sum_externalities ~ country, data = data_not_mic)

# 

plot <- ggplot(data_not_mic, aes(x = country, y = id_ha_yr)) +
  geom_boxplot() +  # Add box plot
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +  # Add error bars for mean +/- SE
  geom_jitter(aes(color = subtype), width = 0.2) +  # Add impact dots with color
  scale_color_manual(values = birds) +  # Set color palette for impact dots
  theme_minimal() +  # Apply a minimal theme
  labs(x = "Country", y = "id_ha_yr", title = "Box Plot of id_ha_yr by Country")  # Add axis labels and title

# Display the plot
print(plot)

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


# MACC

full_macc <- data_not_mic %>%
  ggmacc(abatement = ranking, mac = id_ha_yr, fill = subtype, cost_threshold = social_cost_of_carbon,
         zero_line = TRUE, threshold_line = TRUE) +
  scale_x_continuous(labels = scales::number_format()) +
  scale_fill_manual(values = birds) +
  labs(fill = "Impact",
       x = expression("Abatement (tonnes CO"[2]*"-eq)"),
       y = expression("Marginal abatement cost (GBP tonne CO"[2]*"-eq"^{-1}*")")
  ) +
  theme_classic()






