library(tidyverse)
library(magrittr)
library(survminer)
library(patchwork)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/HADS/")

source("data_cleaning.R")
#source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

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

# Mean HADS between 0 and 12 months by flare occurring between 0 and 12 months

# UC
data_anxiety_soft_long %>% dplyr::filter(diagnosis2 == 'UC/IBDU')
data_anxiety_hard_long %>% dplyr::filter(diagnosis2 == 'UC/IBDU')
data_depression_soft_long %>% dplyr::filter(diagnosis2 == 'UC/IBDU')
data_depression_hard_long %>% dplyr::filter(diagnosis2 == 'UC/IBDU')

# Anxiety ####
## Soft ####
data_anxiety_soft_long_0to12 <- data_anxiety_soft_long %>%
  dplyr::mutate(
    flare0to12 = dplyr::case_when(
      (DiseaseFlareYN == 1) & (time <= 365) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  dplyr::mutate(flare0to12 = forcats::fct_recode(as.character(flare0to12), "Yes" = '1', "No" = '0')) %>%
  dplyr::mutate(month = as.numeric(month)) %>%
  # 0 and 12 months only
  dplyr::filter(month != 24) %>%
  dplyr::group_by(month, flare0to12) %>%
  dplyr::summarise(
    mean_val = mean(anxiety_hads, na.rm = TRUE),
    sd_val = sd(anxiety_hads, na.rm = TRUE),
    n_obs = sum(!is.na(anxiety_hads)), # Count only non-NA values
    se = sd_val / sqrt(n_obs),
    lower = mean_val - 1.96 * se,
    upper = mean_val + 1.96 * se,
    .groups = "drop"
  ) %>%
  dplyr::mutate(segment = "0 to 12 months") %>%
  dplyr::rename(flare_status = flare0to12)
  
# 12 to 24 months
data_anxiety_soft_long_12to24 <- data_anxiety_soft_long %>%
  # Filter anyone flaring or censored before 12 months
  dplyr::filter(time > 365) %>%
  # Flare between 12 and 24
  dplyr::mutate(
    flare12to24 = dplyr::case_when(
      (DiseaseFlareYN == 1) & (time > 365) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  dplyr::mutate(flare12to24 = forcats::fct_recode(as.character(flare12to24), "Yes" = '1', "No" = '0')) %>%
  dplyr::mutate(month = as.numeric(month)) %>%
  # 12 and 24 months only
  dplyr::filter(month != 0) %>%
  dplyr::group_by(month, flare12to24) %>%
  dplyr::summarise(
    mean_val = mean(anxiety_hads, na.rm = TRUE),
    sd_val = sd(anxiety_hads, na.rm = TRUE),
    n_obs = sum(!is.na(anxiety_hads)), # Count only non-NA values
    se = sd_val / sqrt(n_obs),
    lower = mean_val - 1.96 * se,
    upper = mean_val + 1.96 * se,
    .groups = "drop"
  ) %>%
  dplyr::mutate(segment = "12 to 24 months") %>%
  dplyr::rename(flare_status = flare12to24)

data_anxiety_soft_long_0to12 %>%
  dplyr::bind_rows(data_anxiety_soft_long_12to24) %>%
  dplyr::mutate(group = interaction(flare_status, segment)) %>%
  ggplot(aes(
    x = month, 
    y = mean_val, 
    group = group, 
    colour = flare_status,
    fill = flare_status)) +  
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  scale_colour_manual(
    labels = c("No", "Yes", "No", "Yes"),
    values = c("#F8766D", "#00BFC4", "#F8766D", "#00BFC4")) +
  scale_fill_manual(
    labels = c("No", "Yes", "No", "Yes"),
    values = c("#F8766D", "#00BFC4", "#F8766D", "#00BFC4")) +
  scale_x_continuous(breaks = c(0, 12, 24)) +
  labs(color = "Patient-reported flare",
       fill = "Patient-reported flare",
       x = "Month",
       y = "Mean (95% CI) HADS anxiety score") +
  #ggtitle("Somatisation over time in IBD by patient-reported flare status") +
  custom_theme



## Hard ####

data_anxiety_hard_long_0to12 <- data_anxiety_hard_long %>%
  dplyr::mutate(
    flare0to12 = dplyr::case_when(
      (DiseaseFlareYN == 1) & (time <= 365) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  dplyr::mutate(flare0to12 = forcats::fct_recode(as.character(flare0to12), "Yes" = '1', "No" = '0')) %>%
  dplyr::mutate(month = as.numeric(month)) %>%
  # 0 and 12 months only
  dplyr::filter(month != 24) %>%
  dplyr::group_by(month, flare0to12) %>%
  dplyr::summarise(
    mean_val = mean(anxiety_hads, na.rm = TRUE),
    sd_val = sd(anxiety_hads, na.rm = TRUE),
    n_obs = sum(!is.na(anxiety_hads)), # Count only non-NA values
    se = sd_val / sqrt(n_obs),
    lower = mean_val - 1.96 * se,
    upper = mean_val + 1.96 * se,
    .groups = "drop"
  ) %>%
  dplyr::mutate(segment = "0 to 12 months") %>%
  dplyr::rename(flare_status = flare0to12)

# 12 to 24 months
data_anxiety_hard_long_12to24 <- data_anxiety_hard_long %>%
  # Filter anyone flaring or censored before 12 months
  dplyr::filter(time > 365) %>%
  # Flare between 12 and 24
  dplyr::mutate(
    flare12to24 = dplyr::case_when(
      (DiseaseFlareYN == 1) & (time > 365) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  dplyr::mutate(flare12to24 = forcats::fct_recode(as.character(flare12to24), "Yes" = '1', "No" = '0')) %>%
  dplyr::mutate(month = as.numeric(month)) %>%
  # 12 and 24 months only
  dplyr::filter(month != 0) %>%
  dplyr::group_by(month, flare12to24) %>%
  dplyr::summarise(
    mean_val = mean(anxiety_hads, na.rm = TRUE),
    sd_val = sd(anxiety_hads, na.rm = TRUE),
    n_obs = sum(!is.na(anxiety_hads)), # Count only non-NA values
    se = sd_val / sqrt(n_obs),
    lower = mean_val - 1.96 * se,
    upper = mean_val + 1.96 * se,
    .groups = "drop"
  ) %>%
  dplyr::mutate(segment = "12 to 24 months") %>%
  dplyr::rename(flare_status = flare12to24)

data_anxiety_hard_long_0to12 %>%
  dplyr::bind_rows(data_anxiety_hard_long_12to24) %>%
  dplyr::mutate(group = interaction(flare_status, segment)) %>%
  ggplot(aes(
    x = month, 
    y = mean_val, 
    group = group, 
    colour = flare_status,
    fill = flare_status)) +  
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  scale_colour_manual(
    labels = c("No", "Yes", "No", "Yes"),
    values = c("#F8766D", "#00BFC4", "#F8766D", "#00BFC4")) +
  scale_fill_manual(
    labels = c("No", "Yes", "No", "Yes"),
    values = c("#F8766D", "#00BFC4", "#F8766D", "#00BFC4")) +
  scale_x_continuous(breaks = c(0, 12, 24)) +
  labs(color = "Objective flare",
       fill = "Objective flare",
       x = "Month",
       y = "Mean (95% CI) HADS anxiety score") +
  #ggtitle("Somatisation over time in IBD by patient-reported flare status") +
  custom_theme


# Depression ####
## Soft ####
data_depression_soft_long_0to12 <- data_depression_soft_long %>%
  dplyr::mutate(
    flare0to12 = dplyr::case_when(
      (DiseaseFlareYN == 1) & (time <= 365) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  dplyr::mutate(flare0to12 = forcats::fct_recode(as.character(flare0to12), "Yes" = '1', "No" = '0')) %>%
  dplyr::mutate(month = as.numeric(month)) %>%
  # 0 and 12 months only
  dplyr::filter(month != 24) %>%
  dplyr::group_by(month, flare0to12) %>%
  dplyr::summarise(
    mean_val = mean(depression_hads, na.rm = TRUE),
    sd_val = sd(depression_hads, na.rm = TRUE),
    n_obs = sum(!is.na(depression_hads)), # Count only non-NA values
    se = sd_val / sqrt(n_obs),
    lower = mean_val - 1.96 * se,
    upper = mean_val + 1.96 * se,
    .groups = "drop"
  ) %>%
  dplyr::mutate(segment = "0 to 12 months") %>%
  dplyr::rename(flare_status = flare0to12)

# 12 to 24 months
data_depression_soft_long_12to24 <- data_depression_soft_long %>%
  # Filter anyone flaring or censored before 12 months
  dplyr::filter(time > 365) %>%
  # Flare between 12 and 24
  dplyr::mutate(
    flare12to24 = dplyr::case_when(
      (DiseaseFlareYN == 1) & (time > 365) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  dplyr::mutate(flare12to24 = forcats::fct_recode(as.character(flare12to24), "Yes" = '1', "No" = '0')) %>%
  dplyr::mutate(month = as.numeric(month)) %>%
  # 12 and 24 months only
  dplyr::filter(month != 0) %>%
  dplyr::group_by(month, flare12to24) %>%
  dplyr::summarise(
    mean_val = mean(depression_hads, na.rm = TRUE),
    sd_val = sd(depression_hads, na.rm = TRUE),
    n_obs = sum(!is.na(depression_hads)), # Count only non-NA values
    se = sd_val / sqrt(n_obs),
    lower = mean_val - 1.96 * se,
    upper = mean_val + 1.96 * se,
    .groups = "drop"
  ) %>%
  dplyr::mutate(segment = "12 to 24 months") %>%
  dplyr::rename(flare_status = flare12to24)

data_depression_soft_long_0to12 %>%
  dplyr::bind_rows(data_depression_soft_long_12to24) %>%
  dplyr::mutate(group = interaction(flare_status, segment)) %>%
  ggplot(aes(
    x = month, 
    y = mean_val, 
    group = group, 
    colour = flare_status,
    fill = flare_status)) +  
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  scale_colour_manual(
    labels = c("No", "Yes", "No", "Yes"),
    values = c("#F8766D", "#00BFC4", "#F8766D", "#00BFC4")) +
  scale_fill_manual(
    labels = c("No", "Yes", "No", "Yes"),
    values = c("#F8766D", "#00BFC4", "#F8766D", "#00BFC4")) +
  scale_x_continuous(breaks = c(0, 12, 24)) +
  labs(color = "Patient-reported flare",
       fill = "Patient-reported flare",
       x = "Month",
       y = "Mean (95% CI) HADS depression score") +
  #ggtitle("Somatisation over time in IBD by patient-reported flare status") +
  custom_theme



## Hard ####

data_depression_hard_long_0to12 <- data_depression_hard_long %>%
  dplyr::mutate(
    flare0to12 = dplyr::case_when(
      (DiseaseFlareYN == 1) & (time <= 365) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  dplyr::mutate(flare0to12 = forcats::fct_recode(as.character(flare0to12), "Yes" = '1', "No" = '0')) %>%
  dplyr::mutate(month = as.numeric(month)) %>%
  # 0 and 12 months only
  dplyr::filter(month != 24) %>%
  dplyr::group_by(month, flare0to12) %>%
  dplyr::summarise(
    mean_val = mean(depression_hads, na.rm = TRUE),
    sd_val = sd(depression_hads, na.rm = TRUE),
    n_obs = sum(!is.na(depression_hads)), # Count only non-NA values
    se = sd_val / sqrt(n_obs),
    lower = mean_val - 1.96 * se,
    upper = mean_val + 1.96 * se,
    .groups = "drop"
  ) %>%
  dplyr::mutate(segment = "0 to 12 months") %>%
  dplyr::rename(flare_status = flare0to12)

# 12 to 24 months
data_depression_hard_long_12to24 <- data_depression_hard_long %>%
  # Filter anyone flaring or censored before 12 months
  dplyr::filter(time > 365) %>%
  # Flare between 12 and 24
  dplyr::mutate(
    flare12to24 = dplyr::case_when(
      (DiseaseFlareYN == 1) & (time > 365) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  dplyr::mutate(flare12to24 = forcats::fct_recode(as.character(flare12to24), "Yes" = '1', "No" = '0')) %>%
  dplyr::mutate(month = as.numeric(month)) %>%
  # 12 and 24 months only
  dplyr::filter(month != 0) %>%
  dplyr::group_by(month, flare12to24) %>%
  dplyr::summarise(
    mean_val = mean(depression_hads, na.rm = TRUE),
    sd_val = sd(depression_hads, na.rm = TRUE),
    n_obs = sum(!is.na(depression_hads)), # Count only non-NA values
    se = sd_val / sqrt(n_obs),
    lower = mean_val - 1.96 * se,
    upper = mean_val + 1.96 * se,
    .groups = "drop"
  ) %>%
  dplyr::mutate(segment = "12 to 24 months") %>%
  dplyr::rename(flare_status = flare12to24)

data_depression_hard_long_0to12 %>%
  dplyr::bind_rows(data_depression_hard_long_12to24) %>%
  dplyr::mutate(group = interaction(flare_status, segment)) %>%
  ggplot(aes(
    x = month, 
    y = mean_val, 
    group = group, 
    colour = flare_status,
    fill = flare_status)) +  
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  scale_colour_manual(
    labels = c("No", "Yes", "No", "Yes"),
    values = c("#F8766D", "#00BFC4", "#F8766D", "#00BFC4")) +
  scale_fill_manual(
    labels = c("No", "Yes", "No", "Yes"),
    values = c("#F8766D", "#00BFC4", "#F8766D", "#00BFC4")) +
  scale_x_continuous(breaks = c(0, 12, 24)) +
  labs(color = "Objective flare",
       fill = "Objective flare",
       x = "Month",
       y = "Mean (95% CI) HADS depression score") +
  #ggtitle("Somatisation over time in IBD by patient-reported flare status") +
  custom_theme

