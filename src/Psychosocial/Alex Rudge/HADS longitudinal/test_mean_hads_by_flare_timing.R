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

# Mean HADS between 0 and 12 months by flare occurring between 0 and 12 months
data_anxiety_soft_long %<>%
  dplyr::mutate(
    flare_status = dplyr::case_when(
      (DiseaseFlareYN == 1) & (time <= 365) ~ "Flare between 0 and 12m",
      (DiseaseFlareYN == 1) & (time > 365) ~ "Flare between 12 and 24m",
      DiseaseFlareYN == 0 ~ "No flare"
    )
  ) %>%
  dplyr::mutate(flare_status = forcats::fct_relevel(flare_status, "No flare", "Flare between 0 and 12m"))

# HADS measurements by flare status
data_anxiety_soft_long %>%
  dplyr::group_by(month, flare_status) %>%
  dplyr::summarise(
    n = sum(!is.na(anxiety_hads))
  ) %>%
  ggplot(aes(x = month, y = n, fill = flare_status)) +
  geom_col() +
  ylab("Number of responses") +
  xlab("Month") +
  labs(fill = "") + 
  ggtitle("Anxiety HADS and patient-reported flare")


data_anxiety_hard_long %<>%
  dplyr::mutate(
    flare_status = dplyr::case_when(
      (DiseaseFlareYN == 1) & (time <= 365) ~ "Flare between 0 and 12m",
      (DiseaseFlareYN == 1) & (time > 365) ~ "Flare between 12 and 24m",
      DiseaseFlareYN == 0 ~ "No flare"
    )
  ) %>%
  dplyr::mutate(flare_status = forcats::fct_relevel(flare_status, "No flare", "Flare between 0 and 12m"))

# HADS measurements by flare status
data_anxiety_hard_long %>%
  dplyr::group_by(month, flare_status) %>%
  dplyr::summarise(
    n = sum(!is.na(anxiety_hads))
  ) %>%
  ggplot(aes(x = month, y = n, fill = flare_status)) +
  geom_col() +
  ylab("Number of responses") +
  xlab("Month") +
  labs(fill = "") + 
  ggtitle("Anxiety HADS and patient-reported flare")




# Mean anxiety hads
plot_anxiety_soft <- data_anxiety_soft_long %>%
  dplyr::mutate(month = as.numeric(month)) %>%
  dplyr::group_by(month, flare_status) %>%
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
      group = flare_status,
      color = flare_status,
      fill = flare_status
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
  #ggtitle("Anxiety HADS over time in IBD by patient-reported flare status") +
  custom_theme

plot_anxiety_soft


plot_anxiety_hard <- data_anxiety_hard_long %>%
  dplyr::mutate(month = as.numeric(month)) %>%
  dplyr::group_by(month, flare_status) %>%
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
      group = flare_status,
      color = flare_status,
      fill = flare_status
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
  #ggtitle("Anxiety HADS over time in IBD by patient-reported flare status") +
  custom_theme

plot_anxiety_hard
