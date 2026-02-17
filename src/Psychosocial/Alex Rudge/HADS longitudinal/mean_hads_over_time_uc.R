library(tidyverse)
library(magrittr)
library(survminer)
library(patchwork)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/HADS longitudinal/")

source("data_cleaning.R")
source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

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

# UC

data_survival_anxiety_soft_long <- data_survival_anxiety_soft_long %>%
  dplyr::filter(diagnosis2 == "UC/IBDU")

data_survival_anxiety_hard_long <- data_survival_anxiety_hard_long %>%
  dplyr::filter(diagnosis2 == "UC/IBDU")

data_survival_depression_soft_long <- data_survival_depression_soft_long %>%
  dplyr::filter(diagnosis2 == "UC/IBDU")

data_survival_depression_hard_long <- data_survival_depression_hard_long %>%
  dplyr::filter(diagnosis2 == "UC/IBDU")
  

# Anxiety ####
# Soft
plot_anxiety_soft <- data_survival_anxiety_soft_long %>%
  dplyr::mutate(DiseaseFlareYN = forcats::fct_recode(as.character(DiseaseFlareYN), "Yes" = '1', "No" = '0')) %>%
  dplyr::mutate(month = as.numeric(month)) %>%
  dplyr::group_by(month, DiseaseFlareYN) %>%
  dplyr::summarise(
    mean_val = mean(anxiety_hads, na.rm = TRUE),
    sd_val = sd(anxiety_hads, na.rm = TRUE),
    n_obs = sum(!is.na(anxiety_hads)), # Count only non-NA values
    se = sd_val / sqrt(n_obs),
    lower = mean_val - 1.96 * se,
    upper = mean_val + 1.96 * se,
    .groups = "drop"
  ) %>%
  ggplot(
    aes(
      x = month,
      y = mean_val,
      group = DiseaseFlareYN,
      color = DiseaseFlareYN,
      fill = DiseaseFlareYN
    )
  ) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  ylim(0, 8) +
  scale_x_continuous(breaks = c(0, 12, 24)) +
  labs(color = "Patient-reported flare",
       fill = "Patient-reported flare",
       x = "Month",
       y = "Mean Anxiety HADS") +
  #ggtitle("Anxiety HADS over time in UC/IBDU by patient-reported flare status") +
  custom_theme

plot_anxiety_soft

# Hard
plot_anxiety_hard <- data_survival_anxiety_hard_long %>%
  dplyr::mutate(DiseaseFlareYN = forcats::fct_recode(as.character(DiseaseFlareYN), "Yes" = '1', "No" = '0')) %>%
  dplyr::mutate(month = as.numeric(month)) %>%
  dplyr::group_by(month, DiseaseFlareYN) %>%
  dplyr::summarise(
    mean_val = mean(anxiety_hads, na.rm = TRUE),
    sd_val = sd(anxiety_hads, na.rm = TRUE),
    n_obs = sum(!is.na(anxiety_hads)), # Count only non-NA values
    se = sd_val / sqrt(n_obs),
    lower = mean_val - 1.96 * se,
    upper = mean_val + 1.96 * se,
    .groups = "drop"
  ) %>%
  ggplot(
    aes(
      x = month,
      y = mean_val,
      group = DiseaseFlareYN,
      color = DiseaseFlareYN,
      fill = DiseaseFlareYN
    )
  ) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  ylim(0, 8) +
  scale_x_continuous(breaks = c(0, 12, 24)) +
  labs(color = "Objective flare",
       fill = "Objective flare",
       x = "Month",
       y = "Mean Anxiety HADS") +
  #ggtitle("Anxiety HADS over time in UC/IBDU by objective flare status") +
  custom_theme

plot_anxiety_hard

# Depression ####
# Soft
plot_depression_soft <- data_survival_depression_soft_long %>%
  dplyr::mutate(DiseaseFlareYN = forcats::fct_recode(as.character(DiseaseFlareYN), "Yes" = '1', "No" = '0')) %>%
  dplyr::mutate(month = as.numeric(month)) %>%
  dplyr::group_by(month, DiseaseFlareYN) %>%
  dplyr::summarise(
    mean_val = mean(depression_hads, na.rm = TRUE),
    sd_val = sd(depression_hads, na.rm = TRUE),
    n_obs = sum(!is.na(depression_hads)), # Count only non-NA values
    se = sd_val / sqrt(n_obs),
    lower = mean_val - 1.96 * se,
    upper = mean_val + 1.96 * se,
    .groups = "drop"
  ) %>%
  ggplot(
    aes(
      x = month,
      y = mean_val,
      group = DiseaseFlareYN,
      color = DiseaseFlareYN,
      fill = DiseaseFlareYN
    )
  ) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  ylim(0, 6.5) +
  scale_x_continuous(breaks = c(0, 12, 24)) +
  labs(color = "Patient-reported flare",
       fill = "Patient-reported flare",
       x = "Month",
       y = "Mean Depression HADS") +
  #ggtitle("Depression HADS over time in UC/IBDU by patient-reported flare status") +
  custom_theme
 

plot_depression_soft


# Hard
plot_depression_hard <- data_survival_depression_hard_long %>%
  dplyr::mutate(DiseaseFlareYN = forcats::fct_recode(as.character(DiseaseFlareYN), "Yes" = '1', "No" = '0')) %>%
  dplyr::mutate(month = as.numeric(month)) %>%
  dplyr::group_by(month, DiseaseFlareYN) %>%
  dplyr::summarise(
    mean_val = mean(depression_hads, na.rm = TRUE),
    sd_val = sd(depression_hads, na.rm = TRUE),
    n_obs = sum(!is.na(depression_hads)),
    # Count only non-NA values
    se = sd_val / sqrt(n_obs),
    lower = mean_val - 1.96 * se,
    upper = mean_val + 1.96 * se,
    .groups = "drop"
  ) %>%
  ggplot(
    aes(
      x = month,
      y = mean_val,
      group = DiseaseFlareYN,
      color = DiseaseFlareYN,
      fill = DiseaseFlareYN
    )
  ) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.2,
              color = NA) +
  ylim(0, 6.5) +
  scale_x_continuous(breaks = c(0, 12, 24)) +
  labs(color = "Objective flare",
       fill = "Objective flare",
       x = "Month",
       y = "Mean Depression HADS") +
  #ggtitle("Depression HADS over time in UC/IBDU by objective flare status") +
  custom_theme

plot_depression_hard


# 2x2 plot
plot_anxiety_soft + plot_anxiety_hard +
  plot_depression_soft + plot_depression_hard +
  plot_layout(ncol = 2, nrow = 2) +
  plot_annotation(
    title = "HADS scores over time in UC/IBDU by flare type"
  ) &
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
    ) 
  



