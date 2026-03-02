library(tidyverse)
library(magrittr)
library(survminer)
library(patchwork)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/HADS/")

source("data_cleaning.R")

# Theme
custom_theme = theme_minimal() + 
  theme(
    title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "top"
  )


# Anxiety ####

data_anxiety_soft_long %>%
  ggplot(aes(x = month, y = anxiety_hads)) + 
  geom_violin(alpha = 0.5, fill = 'blue') +
  geom_boxplot(alpha = 0.5, fill = 'blue', outliers = FALSE) +
  custom_theme

## Soft ####
data_anxiety_soft_long %>%
  dplyr::mutate(
    DiseaseFlareYN = case_when(
      DiseaseFlareYN == 0 ~ "No",
      DiseaseFlareYN == 1 ~ "Yes"
    ),
    month = as.character(month)
  ) %>%
  ggplot(aes(x = month, y = anxiety_hads, fill = DiseaseFlareYN)) + 
  geom_violin(alpha = 0.5, width = 0.7) +
  geom_boxplot(alpha = 0.5, outliers = FALSE, width = 0.7) +
  custom_theme

## Hard
data_anxiety_hard_long %>%
  dplyr::mutate(
    DiseaseFlareYN = case_when(
      DiseaseFlareYN == 0 ~ "No",
      DiseaseFlareYN == 1 ~ "Yes"
    ),
    month = as.character(month)
  ) %>%
  ggplot(aes(x = month, y = anxiety_hads, fill = DiseaseFlareYN)) + 
  geom_violin(alpha = 0.5, width = 0.7) +
  geom_boxplot(alpha = 0.5, outliers = FALSE, width = 0.7) +
  custom_theme

