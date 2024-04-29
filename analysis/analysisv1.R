## Analyzing sheep farming impacts in the UK, Australia, and NZ


# Lucie de Seguins Pazzis 
# 12/04/2023

# PREPARATION ####

# loading libraries 
library(tidyverse)
library(nortest)
library(colorspace)
library(coin) # for the Kruskal-Wallis test 
library(rstatix)
library(ggpubr)
library(networkD3)

# other libraries? 


# loading the data 
data_1 <- read.csv("analysis/compiled_data_impacts_externalities1.csv", header = TRUE)
head(data_1)
str(data_1)

# Converting the character values into factors 
data_1 <- data_1 %>%
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

# Cleaning the spaces 
data_1 <- data_1 %>%
  mutate(country = str_replace_all(country, "\\s+", ""),
         subtype = str_replace_all(subtype, "\\s+", ""),
         type = str_replace_all(type, "\\s+", ""),
         ecosystem = str_replace_all(ecosystem, "\\s+", ""),
         impact = str_replace_all(impact, "\\s+", ""))


# changing the subtypes categories 
data_copycopy <- data_1 %>%
  mutate(subtype = case_when(
    impact %in% c("runoff", "soilstructurereduction") ~ "soil_degradation",
    impact == "soilacidification" ~ "soil_infertility",
    TRUE ~ subtype
  ))

# oopsi changing an error 
data_copycopy <- data_copycopy %>%
  mutate(high = ifelse(impact == "CH4sequestration" & country == "nz", 1.5313374, high))

data_copy <- data_copycopy

## Data set without microbes values 
data_not_mic <- data_copycopy %>%
  filter(trimws(impact) != "supportsmicrobes")

## Palettes 
# Creating my palette from Manu package (they are inspired from pretty kiwi birds' plumage)
birds <- c("#44781E", "#7D9D33", "#CED38C", "#DCC949", "#BCA888", "#CD8862", "#775B24")
countries <- c("nz" = "#6C803A", "uk" = "#7B5C34","australia" = "#CCAE42")

impacts <- c("#7D9D33", "#CED38C", "#DCC949", "#BCA888", "#CD8862", "#775B24","#44781E", "#A1B654", "#CCAE42","#3E4331", "#AD6B17", "#66743B", "#D0C471", "#CCB62F", "#BAC4C2")

test_impacts_2 <- c("#fde725", "#d0e11c", "#d0e11c", "#73d056", "#4ac16d", "#2db27d", "#1fa187", "#21918c","#277f8e",  "#2e6e8e", "#365c8d", "#3f4788", "#46327e", "#481b6d","#440154")


# RQ1 ####

# 1.1 What are the different impact? How can they be categorized groups? ----

overall_impact <- data_copy %>%
  group_by(impact) %>%
  summarise(overall_impact = sum(unique(length(impact)))) %>%
  ungroup()
# 19 impacts 

# super_group_table 
super_group_table <- data_copy %>%
  group_by(study_id, type, subtype) %>%
  summarise(impacts = list(impact))


# bar chart with the different impacts and and how they are distributed among the types and stacked bar chart per country 
data_grouped_study_id <- data_copy %>%
  group_by(impact, country) %>%
  summarise(study_count = n_distinct(study_id)) %>%
  ungroup()

# reorder factor 
impact_order <- c("vegetationdiversity","supportsearthworms", "supportsmicrobes","supportsantfauna",
                  "soilstructurereduction", "runoff",
                  "leachingN", "leachingP", "soilsolutionP",
                  "nutrientN", "extractableorganicN", "nutrientP", "organicmatter", 
                  "soilacidification", "soilbasification", 
                  "carbonsequestration", "N2Oemissions", "carbonemissions", "CH4sequestration")

data_grouped_study_id$impact <- factor(data_grouped_study_id$impact, levels = impact_order)

# stacked bar chart 
barchart_citation_impact <- ggplot(data_grouped_study_id, aes(y=study_count, 
                                                              x = impact, 
                                                              fill= country,)) + 
  geom_bar(position="stack", stat="identity", width = 0.7) +
  scale_fill_manual(name = "Country",
                    labels = c("Australia", "New Zealand", "United Kingdom"),
                    values = countries) +
  coord_flip() +
  ylab("Number of citations") +
  xlab(NULL) +
  theme_classic() +
  theme(panel.grid.major.x = element_line(color = "black"),  # Vertical gridlines
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.title.x = element_text(size = 20),
        legend.title = element_text(size = 20, color = "grey10"),
        legend.text = element_text(size = 14, color = "grey10")
        ) 



barchart_citation_impact

ggsave("figures/flipped_barchart_citations.png", plot = barchart_citation_impact, width = 6, height = 4, units = "in", dpi = 300)


# 1.2. How are they distributed among the countries? ====

data_grouped_study_id_per_country <- data_copy %>%
  group_by(impact, country, subtype) %>%
  summarise(study_count_country = n_distinct(study_id)) %>%
  ungroup()


barchart_percentage_country <- ggplot(data_grouped_study_id_per_country, 
                                      aes(x = country,y = study_count_country, fill = 
                                            factor(subtype, levels = c(
  "biodiversity", "nutrient_cycling", "nutrient_uncycling", "soil_infertility", "soil_degradation", "climate_mitigation", "climate_intensification"))
  )) +
  geom_col(position = "fill", width = 0.5)+ 
  scale_fill_manual(name = "Subtype",
                    values = birds,
                    labels = c("Biodiversity", "Nutrient cycling", "Nutrient uncycling", "Soil infertility","Soil degradation", "Climate mitigation", "Climate intensification")) +
  labs(x = "Country", 
       y = "Relative Proportion")+
  scale_x_discrete(labels = c("australia" = "Australia", "nz" = "New Zealand", "uk" = "United Kingdom")) +
  theme_classic() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 20, color = "grey10"),
        legend.text = element_text(size = 14, color = "grey10"))
# +
#   theme(legend.position = "none")

barchart_percentage_country 

ggsave("figures/barchart_percentage_citations_country.png", plot = barchart_percentage_country, width = 8, height = 6, units = "in", dpi = 300)

# calculating the percentages 
total_studies_per_subtype_per_country <- data_grouped_study_id_per_country %>%
  group_by(subtype, country) %>%
  summarise(total_studies = sum(study_count_country))

# Merge the total counts back to the original dataset
data_grouped_study_id_per_country <- left_join(data_grouped_study_id_per_country, total_studies_per_subtype_per_country, by = c("subtype", "country"))

# Calculate the percentage of each subtype per country
data_grouped_study_id_per_country <- data_grouped_study_id_per_country %>%
  mutate(percentage = (study_count_country / total_studies.x) * 100)

# Print the resulting dataset
print(data_grouped_study_id_per_country)

# per type
total_impacts_per_country <- data_copy %>%
  group_by(country) %>%
  summarise(total_impacts = n_distinct(impact))

# Filter data for supporting and regulating impacts
supporting_regulating_impacts <- data_copy %>%
  filter(type %in% c("supporting", "regulating"))

# Group the filtered data by country and type, and calculate the total count of cited impacts for each type in each country
total_supporting_regulating_impacts_per_country <- supporting_regulating_impacts %>%
  group_by(country, type) %>%
  summarise(total_impacts = n_distinct(impact))

# Merge the total counts of cited impacts per country with the total counts of supporting and regulating impacts per country
merged_data <- left_join(total_supporting_regulating_impacts_per_country, total_impacts_per_country, by = "country")

# Calculate the percentage of supporting and regulating impacts out of the overall number of cited impacts per country
merged_data <- merged_data %>%
  mutate(percentage = (total_impacts.x / total_impacts.y) * 100)

# Print the resulting dataset
print(merged_data)

# 1.3. How old are the papers? by country? ====

# number of papers per year 
data_year_study <- data_copy %>%
  group_by(year, study_id, country) %>%
  summarise(count = n()) 

# grouping the year - every two years 
data_year <- data_not_mic %>%
  mutate(grouped_year = ifelse(year %% 2 == 0, paste0(year - 1, "-", year), paste0(year, "-", year + 1)))

# counting the number of study per yer group 
data_year <- data_year %>%
  group_by(grouped_year, country) %>%
  summarise(num_studies = n_distinct(study_id))

#plotting the year distribution per number of publications with filled country 
barchart_year <- ggplot(data_year, aes(x = grouped_year, 
                                           y = num_studies, 
                                           fill = country)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(x = "Date", y = "Number of Publications") +
  scale_fill_manual(name = "Country",
                    labels = c("Australia", "New Zealand", "United Kingdom"),
                    values = countries) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14, color = "black"),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 20, color = "grey10"),
        legend.text = element_text(size = 14, color = "grey10"),
        ) +
        #panel.grid.major.y = element_line(color = "grey"),  # Vertical gridlines
       # panel.grid.minor = element_blank()) 
  scale_x_discrete(breaks = unique(data_year$grouped_year)[c(TRUE, FALSE)]) +
  xlab(NULL)


barchart_year
# Assuming your ggplot object is named 'map_nz'
ggsave("figures/year.png", plot = barchart_year, width = 6, height = 4, units = "in", dpi = 300)

# 1.4. Sign distribution across the countries ====

overall_impact2 <- data_copy %>%
  distinct(study_id, impact, sign, .keep_all = TRUE) %>%
  group_by(impact, sign) %>%
  summarise(impact_count = n()) %>%
  ungroup()

impact_counts_country <- data_copy %>%
  distinct(study_id, impact, sign, .keep_all = TRUE) %>%
  group_by(country, sign) %>%
  summarise(impact_count = n()) %>%
  ungroup()

# Calculate total impact counts for each country
total_impact_counts <- impact_counts_country %>%
  group_by(country) %>%
  summarise(total_impact_count = sum(impact_count))

# Join the total impact counts with the impact counts for each country and sign
impact_counts_country2 <- left_join(impact_counts_country, total_impact_counts, by = "country")

# Calculate percentage of positive and negative impacts relative to their total impact
impact_counts_country2 <- impact_counts_country2 %>%
  mutate(percentage = (impact_count / total_impact_count) * 100)

# View the results
print(impact_counts_country2)

# country   sign  impact_count
# <chr>     <fct>        <int>
#   1 australia neg           5
# 2 australia pos             5
# 3 nz        neg             5
# 4 nz        pos             4
# 5 uk        neg             4
# 6 uk        pos             9


# RQ2 ####

#  Checking for normality ----

# visually 
hist(data$id_ha_yr) # not normal at all 
hist(data_not_mic$id_ha_yr) # looks like it could be normal 

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

# test 
shapiro.test(p_resids) # not normal 


### 2.1. What are the externalities between the countries ----

# summing externalities
avg_externalities <- data_not_mic %>%
  group_by(country, impact) %>%
  summarize(
    avg_id_ha_yr = mean(id_ha_yr, na.rm = TRUE),
    avg_low = mean(low, na.rm = TRUE),
    avg_high = mean(high, na.rm = TRUE),
    .groups = 'drop'  # Drop grouping for next operations
  )

total_per_country <- avg_externalities %>%
  group_by(country) %>%
  summarize(
    total_avg_id_ha_yr = sum(avg_id_ha_yr),
    total_avg_low = sum(avg_low),
    total_avg_high = sum(avg_high),
    .groups = 'drop'  # Optional: Drop the group specification after summarizing
  )


sum_externalities <- total_per_country

sum_externalities <- data_not_mic %>%
  group_by(country, impact) %>%
  summarize(sum_externalities = sum(id_ha_yr),
            low_sum_externalities = sum(low),
            high_sum_externalities = sum(high))

str(sum_externalities)

# without_aus <- data_not_mic %>%
#   filter(study_id !=14)
# 
# sum_externalities_without_aus <- without_aus %>%
#   group_by(country) %>%
#   summarize(sum_externalities = sum(id_ha_yr),
#             low_sum_externalities = sum(low),
#             high_sum_externalities = sum(high))

str(sum_externalities)

uk_values <- sum_externalities %>% 
  filter(country == "uk") %>% 
  select(sum_externalities, low_sum_externalities, high_sum_externalities)

sum_externalities2 <- sum_externalities %>%
  mutate(
    percentage_difference_to_uk = abs((sum_externalities - uk_values$sum_externalities) / uk_values$sum_externalities) * 100,
    uncertainty_percentage_to_uk = ((high_sum_externalities - low_sum_externalities) / (high_sum_externalities + low_sum_externalities)) * 100,
    combined_percentage_to_uk = percentage_difference_to_uk + uncertainty_percentage_to_uk
  )

# View the results
print(sum_externalities)

# visualisation 
barchart_externalities_country <- ggplot(sum_externalities, aes(x = country, y = total_avg_id_ha_yr))+
  geom_bar(data = sum_externalities, aes(y= total_avg_id_ha_yr), stat = "identity", width = 0.5, fill = "#CD8862", alpha = 0.7, position = "dodge") +  
  geom_errorbar(data = sum_externalities, aes(ymin = total_avg_low, ymax = total_avg_high), width = 0.2) +  
  geom_point(data = data_not_mic, aes(x = country, y = id_ha_yr), alpha = 0.5, position = position_jitter(width = 0.1)) +
  # geom_text(data = sum_externalities, aes(label = round(sum_externalities, 2), y = sum_externalities), vjust = 0.1, hjust = -0.35, size = 5) +
  theme_minimal()+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 19, color = "black", hjust = 0.4),
        axis.title.y = element_text(size = 30),
        axis.text.y = element_text(size = 19)) +
        # aspect.ratio = 2/1) 
        scale_x_discrete(labels = c("australia" = "Australia", "uk" = "United Kingdom", "nz" = "New Zealand")) +  # something wrong with NZ 
        ylab("Monetary Value in id$/ha/yr") +
        xlab(NULL) 

r

barchart_externalities_country

ggsave("figures/bars_sum_externalities_country.png", plot = barchart_externalities_country, width = 11, height = 12, units = "in", dpi = 300)

# Externalities per type 

sum_externalities_country_type <- data_not_mic %>%
  group_by(country, type) %>%
  summarize(
    id_ha_yr_sum = sum(id_ha_yr),
    low_sum = sum(low),
    high_sum = sum(high)
  )

# country   type       id_ha_yr_sum  low_sum high_sum
# <chr>     <chr>             <dbl>    <dbl>    <dbl>
#   1 australia regulating      -562.    -524.    -601.  
# 2 australia supporting       310.     237.     379.  
# 3 nz        regulating        49.0     -1.03   134.  
# 4 nz        supporting      -726.    -419.   -1017.  
# 5 uk        regulating         7.78     6.60     8.96
# 6 uk        supporting     -1550.   -1333.   -2011.  


sum_externalities_country2 <- data_not_mic %>%
  group_by(country, type) %>%
  summarize(
    positive_sum = sum(ifelse(id_ha_yr > 0, id_ha_yr, 0)),
    negative_sum = sum(ifelse(id_ha_yr < 0, id_ha_yr, 0))
  ) %>%
  ungroup()

# Reshape the data into a format suitable for plotting
sum_externalities_country_long <- sum_externalities_country2 %>%
  pivot_longer(cols = c(positive_sum, negative_sum),
               names_to = "variable",
               values_to = "value") %>%
  separate(variable, into = c("variable_prefix", "variable_suffix"), sep = "_") %>%
  mutate(value = ifelse(variable_suffix == "sum", value, -value)) %>%
  pivot_wider(names_from = variable_suffix,
              values_from = value)

# Plot the data
ggplot(sum_externalities_country_long, aes(x = type, y = sum, fill = country)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) + 
  scale_fill_manual(name = "Country",
                    labels = c("Australia", "New Zealand", "United Kingdom"),
                    values = countries) +
  labs(title = "Sum of Positive and Negative Values of id_ha_yr per Country",
       x = "Ecosystem Service / Disservice", y = "Sum of id_ha_yr") +
  scale_x_discrete(labels = c("regulating" = "Regulating", "supporting" = "Supporting", "regulating" = "Regulating")) +
  theme_classic() 




## 1. regulating 
sum_externalities_country_regulating <- sum_externalities_country_type[sum_externalities_country_type$type == "regulating", ]

# test for significance 
kruskal.test(id_ha_yr_sum ~ country, data = sum_externalities_country_regulating)
# Kruskal-Wallis chi-squared = 2, df = 2, p-value = 0.3679

## 2. supporting 
sum_externalities_country_supporting <- sum_externalities_country_type[sum_externalities_country_type$type == "supporting", ]

# test for significance 
kruskal.test(id_ha_yr_sum ~ country, data = sum_externalities_country_supporting)
# Kruskal-Wallis chi-squared = 2, df = 2, p-value = 0.3679

### 2.2. Do the externalities between the three countries differ? - NO ----

# testing if the sum of values differ among categories.
# Using the Kruskal-wallis test - non-parametric alternative to ANOVA
# Used to determine whether there are statistically significant differences between the medians of three or more independent groups
kw_test_countries <- kruskal_test(id_ha_yr ~ country, data_not_mic)
print(kw_test_countries) # chi-squared = 1.8805, df = 2, p-value = 0.3905

# testing the difference between the two countries
# 1. UK & AUS
data_uk_aus <- data_not_mic %>%
  filter(trimws(country) != "nz")
head(data_uk_aus)
kw_test_country_ukaus <- kruskal_test(id_ha_yr ~ country, data_uk_aus)
print(kw_test_country_ukaus) # chi-squared = 0.64047, df = 1, p-value = 0.4235

mwu_test_country_ukaus <- wilcox.test(id_ha_yr ~ country, data = data_uk_aus) # could not use the Wilcoxon rank sum test because we had ties in the data (two or more observations havnig the same value)
print(mwu_test_country_ukaus) # W = 79, p-value = 0.4416

# 2. UK & NZ 
data_uk_nz <- data_not_mic %>%
  filter(trimws(country) != "australia")
head(data_uk_nz)
kw_test_country_uknz <- kruskal_test(id_ha_yr ~ country, data_uk_nz)
print(kw_test_country_uknz) # chi-squared = 1.9755, df = 1, p-value = 0.1599

# 3. AUS & NZ
data_aus_nz <- data_not_mic %>%
  filter(trimws(country) != "uk")
head(data_aus_nz)
kw_test_country_ausnz <- kruskal_test(id_ha_yr ~ country, data_aus_nz)
print(kw_test_country_ausnz) # chi-squared = 0.24764, df = 1, p-value = 0.6187

## Do they differ between their costs and benefits 
# 1. pos
data_pos <- data_not_mic %>%
  filter(sign == "pos")

kw_test_countries_pos <- kruskal_test(id_ha_yr ~ country, data_pos)
print(kw_test_countries_pos) # chi-squared = 0.37218, df = 2, p-value = 0.8302

# 2. neg
data_neg <- data_not_mic %>%
  filter(sign == "neg")

kw_test_countries_neg <- kruskal_test(id_ha_yr ~ country, data_neg)
print(kw_test_countries_neg) # chi-squared = 3.5936, df = 2, p-value = 0.1658


### 2.3. How are the externalities distributed for each countries according to each ecosystem services and subtypes ----

# creating dataframe for the subtypes 
sum_externalities_subtype <- data_not_mic %>%
  group_by(country, subtype) %>%
  summarize(sum_subtype = sum(id_ha_yr),
            low_sum_subtype = sum(low),
            high_sum_subtype = sum(high))

str(sum_externalities_subtype)

# abs
data_abs <- data_not_mic
data_abs$id_ha_yr <- abs(data_abs$id_ha_yr)

# 
total_externalities_per_country <- data_abs %>%
  group_by(country) %>%
  summarise(total_impacts = sum(id_ha_yr))

# Group the filtered data by country and type, and calculate the total count of cited impacts for each type in each country
total_subtype_impacts_per_country <- data_abs %>%
  group_by(country, subtype) %>%
  summarise(total_impacts = sum(id_ha_yr))

# Merge the total counts of cited impacts per country with the total counts of supporting and regulating impacts per country
merged_data1 <- left_join(total_subtype_impacts_per_country, total_externalities_per_country, by = "country")

# Calculate the percentage of supporting and regulating impacts out of the overall number of cited impacts per country
merged_data1 <- merged_data1 %>%
  mutate(percentage = (total_impacts.x / total_impacts.y) * 100)

# Print the resulting dataset
print(merged_data1)

# Group the filtered data by country and type, and calculate the total count of cited impacts for each type in each country
total_type_impacts_per_country <- data_abs %>%
  group_by(country, type) %>%
  summarise(total_impacts = sum(id_ha_yr))

# Merge the total counts of cited impacts per country with the total counts of supporting and regulating impacts per country
merged_data2 <- left_join(total_type_impacts_per_country, total_externalities_per_country, by = "country")

# Calculate the percentage of supporting and regulating impacts out of the overall number of cited impacts per country
merged_data2 <- merged_data2 %>%
  mutate(percentage = (total_impacts.x / total_impacts.y) * 100)

# Print the resulting dataset
print(merged_data1)


# 1. visualisation 1 - percentages of the subtypes for each country ==== The one we kept!

data_700_subtype <- data_not_mic
data_700_subtype$id_ha_yr <- data_not_mic$id_ha_yr + 700

data_700_subtype$id_ha_yr <- abs(data_not_mic$id_ha_yr)

sum_externalities_subtype <- data_700_subtype %>%
  group_by(country, subtype) %>%
  summarize(sum_subtype = sum(id_ha_yr),
            low_sum_subtype = sum(low),
            high_sum_subtype = sum(high))

str(sum_externalities_subtype)


barchart_subtype <- ggplot(sum_externalities_subtype, aes(x = country, y = sum_subtype, fill = 
                                                             factor(subtype, levels = c(
  # "soil_degradation", "climate_intensification", "soil_infertility", "nutrient_uncycling", "nutrient_cycling", "climate_mitigation", "biodiversity"
  "biodiversity", "nutrient_cycling", "nutrient_uncycling", "soil_infertility", "soil_degradation", "climate_mitigation", "climate_intensification"))
)) +
    geom_col(position = "fill", width = 0.5)+ 
    scale_fill_manual(name = "Subtype",
                    values = birds,
                    labels = c("Biodiversity", "Nutrient cycling", "Nutrient uncycling", "Soil infertility","Soil degradation", "Climate mitigation", "Climate intensification")) +
  labs(x = "Country", 
       y = "Relative Proportion")+
  scale_x_discrete(labels = c("australia" = "Australia", "nz" = "New Zealand", "uk" = "United Kingdom")) +
  theme_classic() + 
  theme(axis.ticks.x = element_blank(),
                       axis.text.x = element_text(size = 14, color = "black"),
                       axis.title.x = element_text(size = 20),
                       axis.title.y = element_text(size = 20),
                       legend.title = element_text(size = 20, color = "grey10"),
                       legend.text = element_text(size = 14, color = "grey10")) 

barchart_subtype 

ggsave("figures/barchart_percentage_externalities_country.png", plot = barchart_subtype, width = 8, height = 6, units = "in", dpi = 300)

# arrange this so we have one legend and so that the barcharts are the same width for the ratio


# 2. visualisation 2 - still percentages but you also see the actual value for each ====

# a. both positive and negative 
ggplot(sum_externalities_subtype, aes(x = subtype, y = sum_subtype, fill = country)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(name = "Country",
                    values = countries) +
  theme_classic()

# b. Positive externalities 
sum_externalities_subtype_pos <- data_pos %>%
  group_by(country, subtype) %>%
  summarize(sum_subtype = sum(id_ha_yr),
            low_sum_subtype = sum(low),
            high_sum_subtype = sum(high))

str(sum_externalities_subtype_pos)

ggplot(sum_externalities_subtype_pos, aes(x = subtype, y = sum_subtype, fill = country)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(name = "Country",
                    values = birds) +
  theme_classic()


# c. Negative externalities 
sum_externalities_subtype_neg <- data_neg %>%
  group_by(country, subtype) %>%
  summarize(sum_subtype = sum(id_ha_yr),
            low_sum_subtype = sum(low),
            high_sum_subtype = sum(high))

str(sum_externalities_subtype_neg)

neg_subtype <- ggplot(sum_externalities_subtype_neg, aes(x = subtype, y = sum_subtype, fill = country)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(data = sum_externalities_subtype_neg, aes(ymin = low_sum_subtype, ymax = high_sum_subtype), position = position_dodge(width = 0.9), width = 0.25) +  
  scale_fill_manual(name = "Country",
                    values = countries) +
  theme_classic() 

# snapchot 
sum_externalities_subtype_neg_snapchot <- data_neg %>%
  group_by(country, subtype) %>%
  filter(!(subtype %in% c("toxicity", "soil_structure"))) %>%
  summarize(sum_subtype = sum(id_ha_yr),
            low_sum_subtype = sum(low),
            high_sum_subtype = sum(high))

str(sum_externalities_subtype_neg_snapchot)

neg_subtype_snapchot <- ggplot(sum_externalities_subtype_neg_snapchot, aes(x = subtype, y = sum_subtype, fill = country)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(data = sum_externalities_subtype_neg_snapchot, aes(ymin = low_sum_subtype, ymax = high_sum_subtype), position = position_dodge(width = 0.9), width = 0.25) +  
  scale_fill_manual(name = "Country",
                    values = countries) +
  theme_classic() 

# same for impacts 

summarized_data_pos <- data_pos %>%
  group_by(country, impact) %>%
  summarise(total_value = sum(id_ha_yr),
            total_low = sum(low),  # You may adjust this to calculate the appropriate low and high values
            total_high = sum(high))  # You may adjust this to calculate the appropriate low and high values


macc_pos <- ggplot(summarized_data_pos, aes(x = reorder(impact, total_value), y = total_value, fill = country)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = total_low, ymax = total_high), width = 0.2, position = position_dodge(width = 0.9)) +
  scale_fill_manual(name = "Country",
                    values = countries) +
  theme_classic() +
  theme(
    # axis.text.x = element_blank(),  # Remove x-axis text
    #     axis.ticks.x = element_blank(),  # Remove x-axis ticks
    #     axis.title.x = element_blank(),
    #     axis.line.x = element_blank(),
        axis.title.y = element_text(size = 30),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 20, color = "grey10"),
        legend.text = element_text(size = 14, color = "grey10")) +
  ylab("Monetary Value in id$/ha/yr") +
  xlab(NULL)

macc_pos

ggsave("figures/macc_pos_general.png", plot = macc_pos, width = 8, height = 6, units = "in", dpi = 300)

# snapchot positive 

snapchot_pos <- summarized_data_pos %>%
  filter(total_value > -12)

macc_pos_snapchot <- ggplot(snapchot_pos, aes(x = reorder(impact, total_value), y = total_value, fill = country)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_errorbar(aes(ymin = total_low, ymax = total_high), width = 0.2, position = position_dodge(width = 0.9)) +
  scale_fill_manual(name = "Country",
                    values = countries) +
  theme_classic() +
  theme(
    # axis.text.x = element_blank(),  # Remove x-axis text
    #     axis.ticks.x = element_blank(),  # Remove x-axis ticks
    #     axis.title.x = element_blank(),
    #     axis.line.x = element_blank(),
        axis.title.y = element_text(size = 30),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 20, color = "grey10"),
        legend.text = element_text(size = 14, color = "grey10")) +
  ylab("Monetary Value in id$/ha/yr") +
  xlab(NULL)

macc_pos_snapchot

ggsave("figures/macc_pos_snapchot.png", plot = macc_pos_snapchot, width = 8, height = 6, units = "in", dpi = 300)

# negative imapcts 
summarized_data_neg <- data_neg %>%
  group_by(country, impact) %>%
  summarise(total_value = sum(id_ha_yr),
            total_low = sum(low),  # You may adjust this to calculate the appropriate low and high values
            total_high = sum(high))  # You may adjust this to calculate the appropriate low and high values


macc_neg <- ggplot(summarized_data_neg, aes(x = reorder(impact, total_value), y = total_value, fill = country)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = total_low, ymax = total_high), width = 0.2, position = position_dodge(width = 0.9)) +
  scale_fill_manual(name = "Country",
                    values = countries) +
  theme_classic() +
  theme(
    # axis.text.x = element_blank(),  # Remove x-axis text
    #     axis.ticks.x = element_blank(),  # Remove x-axis ticks
    #     axis.title.x = element_blank(),
    #     axis.line.x = element_blank(),
        axis.title.y = element_text(size = 30),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 20, color = "grey10"),
        legend.text = element_text(size = 14, color = "grey10")) +
  ylab("Monetary Value in id$/ha/yr") +
  scale_y_continuous(position = "right", limits = c(0, 2000)) +
  xlab(NULL)

macc_neg
 
ggsave("figures/macc_neg_general1.png", plot = macc_neg, width = 8, height = 6, units = "in", dpi = 300)

# macc_neg_snapchot

snapchot_neg <- summarized_data_neg %>%
  filter(total_value < 7.779620e+00)

macc_neg_snapchot <- ggplot(snapchot_neg, aes(x = reorder(impact, total_value), y = total_value, fill = country)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = total_low, ymax = total_high), width = 0.2, position = position_dodge(width = 0.9)) +
  scale_fill_manual(name = "Country",
                    values = countries) +
  theme_classic() +
  theme(axis.text.x = element_blank(),  # Remove x-axis text
        axis.ticks.x = element_blank(),  # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(size = 30),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 20, color = "grey10"),
        legend.text = element_text(size = 14, color = "grey10")) +
  ylab("Monetary Value in id$/ha/yr") +
  scale_y_continuous(position = "right")+
  xlab(NULL)

macc_neg_snapchot

ggsave("figures/macc_neg_snapchot.png", plot = macc_neg_snapchot, width = 8, height = 6, units = "in", dpi = 300)


# testing ====
# testing for negative externalities 
kruskal_test_neg_subtype <- sum_externalities_subtype_neg %>%
  group_by(country) %>%
  kruskal_test(sum_subtype ~ subtype)

print(kruskal_test_neg_subtype) # no significant difference in the distributions of the sum of externalities across subtypes for each country.

# testing for positive etxternalities 
kruskal_test_pos_subtype <- sum_externalities_subtype_pos %>%
  group_by(country) %>%
  kruskal_test(sum_subtype ~ subtype)

print(kruskal_test_pos_subtype) # no significant difference in the distributions of the sum of externalities across subtypes for each country.

# visualisation 3 - box plot of the externalities per country ==== include this one maybe not include this one? 
boxplot_ext_country <- ggplot(data_not_mic, aes(x = country, y = id_ha_yr, fill = country)) +
  
  geom_boxplot(width=0.7) +  # Add box plot
  # geom_jitter(aes(color = subtype), width = 0.2, position = position_jitterdodge()) +  # Add impact dots with color
  # geom_jitter(aes(color = subtype, fill = country), width = 0.2, position = position_jitterdodge(dodge.width = 0.75)) +  # Add impact dots with color and fill by country  scale_fill_manual(values = countries) +
  # geom_point(aes(colour = subtype, group = country),
  #            position = position_dodge(width = .75), size = 3
  # )+
  geom_point(position = position_jitterdodge(), aes(color = subtype)) +
  
  scale_color_manual(values = birds,
                     name = "Subtype",
                     labels = c("Biodiversity", "Climate Intensification", "Climate mitigation", "Nutrient cyclying", "Nutrient uncycling", "Soil degradation", "Soil infertility")) +
  scale_fill_manual(values = countries,
                    labels = c("Australia", "New Zealand", "United Kingdom"),
                    name = "Country") +
  scale_x_discrete(labels= c("Australia", "New Zealand", "United Kingdom"))+
  
  facet_wrap(~type, labeller = labeller(type = c("regulating" = "Regulating", "supporting" = "Supporting"))) +  # Change facet labels
  
  theme_classic() +  # Apply a minimal theme
  theme(axis.text.x = element_text(size = 15),  # Remove x-axis text
        axis.ticks.x = element_blank(),  # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 30),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 20, color = "grey10"),
        legend.text = element_text(size = 14, color = "grey10"),
        strip.text = element_text(size = 25)) +
  ylab("Monetary Value in id$/ha/yr") 

print(boxplot_ext_country)

ggsave("figures/boxplot_countries_type.png", plot = boxplot_ext_country, width = 13, height = 9, units = "in", dpi = 300)

## visualisation 4 - making a Marginal Abatement Cost Curve (MACC) ====

# Ranking the impacts depending on their benefit/cost
# Create a new column for the rankings
data_not_mic$ranking <- rank(data_not_mic$id_ha_yr)

# Removing small values 
data_without_small_values <- data_not_mic %>%
  filter(id_ha_yr >= 1 | id_ha_yr <= -1)

data_without_small_values$ranking <- rank(data_without_small_values$id_ha_yr)

# MACC
social_cost_of_carbon <- 0 # setting a cost for social carbon 

full_macc <- data_without_small_values %>%
  ggmacc(abatement = ranking, mac = id_ha_yr, 
         fill = impact,
         cost_threshold = social_cost_of_carbon,
         zero_line = TRUE, threshold_line = TRUE) +
  scale_x_continuous(labels = scales::number_format()) +
  labs(
    # fill = "Country",
       y = expression("Monetary Value (id$ ha"^{-1}*"yr"^{-1}*")"),
       fill = ""
       ) +
  scale_fill_manual(values = test_impacts,
                    # labels = c("Australia", "New Zealand", "United Kingdom")
  ) +
  theme_classic() 
# +
#   theme(axis.text.x = element_blank(),
#         axis.title.x = element_blank(),
#         axis.ticks.x = element_blank()) 

full_macc 

# improvements on this graph:
# 1. find a way to separate the bars with black contour 
# 2. put labels for each of the subtypes 



### 2.4. Does stocking rate impact affect externalities value? ----

data_stocking <- data_not_mic %>%
  filter(! is.na(stocking_rate_sheep_ha) | ! is.na(stocking_rate_sheep_ha.1) ) # 23 rows so 23 values left 

data_stocking_filtered <- data_stocking %>%
  mutate(
    stocking_rate_mean = ifelse(is.na(stocking_rate_sheep_ha.1),
                                stocking_rate_sheep_ha,
                                (stocking_rate_sheep_ha + stocking_rate_sheep_ha.1) / 2),
    uncertainty = ifelse(is.na(stocking_rate_sheep_ha.1),
                         abs(stocking_rate_mean - stocking_rate_sheep_ha),
                         abs(stocking_rate_mean - stocking_rate_sheep_ha.1) / 2)
  )

ggplot(data_stocking_filtered, aes(x = country, y = stocking_rate_mean)) +
  geom_boxplot(
    #aes(ymin = lower, ymax = upper), width = 0.5, fill = "lightblue"
    ) +
  geom_point(position = position_jitter(width = 0.2), color = "blue") +  # Add jittered points
  labs(x = "Country", y = "Stocking Rate", title = "Stocking Rate with Uncertainty per Country") +
  theme_minimal()

# testing for normality 
hist(data_stocking_filtered$stocking_rate_mean) # not normal 

kruskal.test(stocking_rate_mean ~ country, data = data_stocking_filtered)
# Kruskal-Wallis chi-squared = 14.822, df = 2, p-value = 0.0006045

# Perform Dunn's test with Bonferroni correction
pairwise.wilcox.test(data_stocking_filtered$stocking_rate_mean, data_stocking_filtered$country, p.adjust.method = "bonferroni")
# australia nz    
# nz 0.0028    -     
#   uk 0.6636    0.0087
# Warning messages:
#   1: In wilcox.test.default(xi, xj, paired = paired, ...) :
#   cannot compute exact p-value with ties
# 2: In wilcox.test.default(xi, xj, paired = paired, ...) :
#   cannot compute exact p-value with ties
# 3: In wilcox.test.default(xi, xj, paired = paired, ...) :
#   cannot compute exact p-value with ties

# Spearman rank correlation test overall 
sp_test_stocking <- cor.test(data_stocking_filtered$id_ha_yr, data_stocking_filtered$stocking_rate_mean, method = "spearman")
# Warning message:
#   In cor.test.default(data_stocking_filtered$id_ha_yr, data_stocking_filtered$stocking_rate_mean,  :
#                         Cannot compute exact p-value with ties
sp_test_stocking
# S = 2139.9, p-value = 0.7953
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# -0.05724928 

# Spearman rank test per country 
p_values <- data_stocking_filtered %>%
  group_by(country) %>%
  summarize(p.value = cor.test(stocking_rate_mean, id_ha_yr, method = "spearman")$p.value)

p_values
# country   p.value
# <chr>       <dbl>
#   1 australia  0.307 
# 2 nz         0.264 
# 3 uk         0.0269 # uk is significant!!! 

# Plot with trendline for the uk
# data set with just the UK 
data_uk <- data_stocking_filtered %>% 
  filter(country == "uk")
# plot 
(ggplot(data_uk, aes(x = stocking_rate_mean, y = id_ha_yr, color = country)) +
  geom_point(show.legend = FALSE) +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "#7A3520") +  # Add trendline
  labs(x = "Stocking Rate (Mean)",
       y = "Adjusted id_ha_yr (After adding 1000)") +
    scale_color_manual(values = countries) +
    theme_minimal())

# Plot with all the countries 
ggplot(data_stocking_filtered, aes(x = stocking_rate_mean, y = id_ha_yr, color = country)) +
  geom_point() +  # Add points with conditional coloring
  geom_smooth(data = subset(data_stocking_filtered, country == "uk"), 
              aes(group = 1), 
              method = "lm", 
              se = FALSE, 
              color = "#7A3520") +  # Add trendline for UK
  labs(x = "Stocking Rate (Mean)",
       y = "Adjusted id_ha_yr (After adding 1000)",
       color = "Country") +
  scale_color_manual(values = countries) +
  theme_minimal()


### 2.5. Do externalities vary with ecosystem types? - NO ====

# removing NAs
data_ecosystem <- data_not_mic[complete.cases(data_not_mic$ecosystem), ]

table(data_ecosystem$ecosystem)
# chenopod_shrublands             dryland             pasture                peat 
# 2                   1                  22                   6 

# testing for the significance among the ecosystems with the Kruskal-Wallis test 
kruskal.test(id_ha_yr ~ ecosystem, data = data_ecosystem)
# Kruskal-Wallis chi-squared = 3.7565, df = 3, p-value = 0.289

# Boxplot of the four ecosystems 
ggplot(data_ecosystem, aes(x = ecosystem, y = id_ha_yr, fill = ecosystem)) +
  geom_boxplot() +
  scale_fill_manual(values = birds) + 
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(x = "Ecosystem", y = "id_ha_yr") +
  theme_minimal()

data_ecosystem_pasture_peatland <- data_ecosystem %>%
  filter(ecosystem %in% c("pasture", "peat"))

wilcox.test(id_ha_yr ~ ecosystem, data = data_ecosystem_pasture_peatland)
# W = 49, p-value = 0.3652

#Plot of just pasture and peat 
ggplot(data_ecosystem_pasture_peatland, aes(x = ecosystem, y = id_ha_yr, fill = ecosystem)) +
  geom_boxplot() +
  scale_fill_manual(values = birds) + 
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(x = "Ecosystem", y = "id_ha_yr") +
  theme_minimal()


### 2.6. How are the causes and the externalities linked? - Sankey Diagram ====
# see the other script 

