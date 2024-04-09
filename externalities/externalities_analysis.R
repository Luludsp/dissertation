## Analyzing externalities 
## UK, Australia, and NZ

# Lucie de Seguins Pazzis 
# 05/04/2023

### PREPARATION

library(tidyverse) # contains dplyr and ggplot2 - useful for data wrangling and data visualisation

# loading data
externalities <- read.csv("externalities/valuation1_r.csv", header = TRUE)
head(externalities)
str(externalities)

names(externalities)[7] <- "monetization" # changing the name

externalities <- externalities %>%
  mutate(impact = as.factor(impact), 
         country = as.factor(country), 
         type = as.factor(type), 
         subtype = as.factor(subtype),
         type = as.factor(type))

# Remove rows with NA values
externalities <- externalities[complete.cases(externalities), ]



sum_country <- externalities %>%
  group_by(country) %>%
  summarise(sum = sum(monetization))%>%
  ungroup()


# Without ant fauna 
filtered_aunt_externalities <- externalities %>% filter(row_number() != 35)

# Without ant and bacteria big one 
filtered_ant_bacteria_externalities <- externalities %>% filter(row_number() != 35 & row_number() != 33)

# Let's see now 
sum_country <- filtered_aunt_externalities %>%
  group_by(country) %>%
  summarise(sum = sum(monetization))%>%
  ungroup()

sum_country <- filtered_ant_bacteria_externalities %>%
  group_by(country) %>%
  summarise(sum = sum(monetization))%>%
  ungroup()


# Test for significant difference 


hist(externalities$log(monetization))

externalities$log_monetization <- log(externalities$monetization)
head(externalities)
     
hist(filtered_ant_bacteria_externalities$monetization)

ggplot(filtered_ant_bacteria_externalities, aes(x = country, y = monetization)) +
  geom_boxplot() +  # Customize fill and color
  labs(x = "Country",
       y = "Monetary Value in id$/ha") +
  theme_minimal()


sum_monetization_aunt_bacteria <- filtered_ant_bacteria_externalities %>%
  group_by(country) %>%
  summarise(Total_Monetization = sum(monetization))

ggplot(sum_monetization_aunt_bacteria, aes(x = country, y = Total_Monetization)) +
  geom_bar(stat = "identity", fill = "#4CAF50", color = "#388E3C") +  # Use geom_bar for bar plot
  labs(title = "Total Monetization Comparison",
       x = "Country",
       y = "Total Monetization") +
  theme_minimal()

ggplot(filtered_ant_bacteria_externalities, aes(x = country, y = monetization, fill = country)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Monetization Comparison",
       x = "Country",
       y = "Monetization") +
  theme_minimal()
