library(tidyverse)
library(magrittr)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal data analysis/HADS/")

source("data_cleaning.R")
source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

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

# Change in HADS between 0 and 12m and flare in next 12m

# Anxiety ####
## Soft ####
data_anxiety_soft <- data_anxiety_soft %>%
  # Calculate change
  dplyr::mutate(
    anxiety_change = anxiety_hads_12 - anxiety_hads
  ) %>%
  # Anxiety change binary positive or negative
  dplyr::mutate(
    anxiety_change_sign = sign(anxiety_change),
    anxiety_change_sign = dplyr::case_match(
      anxiety_change_sign, 
      -1 ~ "Better",
      0 ~ "Same",
      1 ~ "Worse"
    )
  ) %>%
  # Flare in 0 to 12m
  dplyr::mutate(
    flare0to12 = dplyr::case_when(
      DiseaseFlareYN == 1 & time <= 365 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  # Remove patients censored before 12m
  dplyr::filter(
    !(DiseaseFlareYN == 0 & time < 365)
  )

# Do patients who flare get worse anxiety scores?
data_anxiety_soft %>%
  group_by(flare0to12) %>%
  dplyr::count(anxiety_change_sign) %>%
  dplyr::group_by(flare0to12) %>%
  dplyr::mutate(p = n/sum(n)) %>%
  ggplot(aes(x = flare0to12, y = p, fill = anxiety_change_sign)) +
  geom_col(position = 'dodge', alpha = 0.8) +
  xlab("Flare between 0 and 12 months") +
  ylab("Percentage") +
  labs(fill = "Change in anxiety") +
  ggtitle("Change in anxiety and patient-reported flare") +
  custom_theme


# Hard ####
data_anxiety_hard <- data_anxiety_hard %>%
  # Calculate change
  dplyr::mutate(
    anxiety_change = anxiety_hads_12 - anxiety_hads
  ) %>%
  # Anxiety change binary positive or negative
  dplyr::mutate(
    anxiety_change_sign = sign(anxiety_change),
    anxiety_change_sign = dplyr::case_match(
      anxiety_change_sign, 
      -1 ~ "Better",
      0 ~ "Same",
      1 ~ "Worse"
    )
  ) %>%
  # Flare in 0 to 12m
  dplyr::mutate(
    flare0to12 = dplyr::case_when(
      DiseaseFlareYN == 1 & time <= 365 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  # Remove patients censored before 12m
  dplyr::filter(
    !(DiseaseFlareYN == 0 & time < 365)
  )

# Do patients who flare get worse anxiety scores?
data_anxiety_hard %>%
  group_by(flare0to12) %>%
  dplyr::count(anxiety_change_sign) %>%
  dplyr::group_by(flare0to12) %>%
  dplyr::mutate(p = n/sum(n)) %>%
  ggplot(aes(x = flare0to12, y = p, fill = anxiety_change_sign)) +
  geom_col(position = 'dodge', alpha = 0.8) +
  xlab("Flare between 0 and 12 months") +
  ylab("Percentage") +
  labs(fill = "Change in anxiety") +
  ggtitle("Change in anxiety and objective flare") +
  custom_theme


# Depression ####
## Soft ####
data_depression_soft <- data_depression_soft %>%
  # Calculate change
  dplyr::mutate(
    depression_change = depression_hads_12 - depression_hads
  ) %>%
  # depression change binary positive or negative
  dplyr::mutate(
    depression_change_sign = sign(depression_change),
    depression_change_sign = dplyr::case_match(
      depression_change_sign, 
      -1 ~ "Better",
      0 ~ "Same",
      1 ~ "Worse"
    )
  ) %>%
  # Flare in 0 to 12m
  dplyr::mutate(
    flare0to12 = dplyr::case_when(
      DiseaseFlareYN == 1 & time <= 365 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  # Remove patients censored before 12m
  dplyr::filter(
    !(DiseaseFlareYN == 0 & time < 365)
  )

# Do patients who flare get worse depression scores?
data_depression_soft %>%
  group_by(flare0to12) %>%
  dplyr::count(depression_change_sign) %>%
  dplyr::group_by(flare0to12) %>%
  dplyr::mutate(p = n/sum(n)) %>%
  ggplot(aes(x = flare0to12, y = p, fill = depression_change_sign)) +
  geom_col(position = 'dodge', alpha = 0.8) +
  xlab("Flare between 0 and 12 months") +
  ylab("Percentage") +
  labs(fill = "Change in depression") +
  ggtitle("Change in depression and patient-reported flare") +
  custom_theme


# Hard
data_depression_hard <- data_depression_hard %>%
  # Calculate change
  dplyr::mutate(
    depression_change = depression_hads_12 - depression_hads
  ) %>%
  # depression change binary positive or negative
  dplyr::mutate(
    depression_change_sign = sign(depression_change),
    depression_change_sign = dplyr::case_match(
      depression_change_sign, 
      -1 ~ "Better",
      0 ~ "Same",
      1 ~ "Worse"
    )
  ) %>%
  # Flare in 0 to 12m
  dplyr::mutate(
    flare0to12 = dplyr::case_when(
      DiseaseFlareYN == 1 & time <= 365 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  # Remove patients censored before 12m
  dplyr::filter(
    !(DiseaseFlareYN == 0 & time < 365)
  )

# Do patients who flare get worse depression scores?
data_depression_hard %>%
  group_by(flare0to12) %>%
  dplyr::count(depression_change_sign) %>%
  dplyr::group_by(flare0to12) %>%
  dplyr::mutate(p = n/sum(n)) %>%
  ggplot(aes(x = flare0to12, y = p, fill = depression_change_sign)) +
  geom_col(position = 'dodge', alpha = 0.8) +
  xlab("Flare between 0 and 12 months") +
  ylab("Percentage") +
  labs(fill = "Change in depression") +
  ggtitle("Change in depression and objective flare") +
  custom_theme

# Too much missingness