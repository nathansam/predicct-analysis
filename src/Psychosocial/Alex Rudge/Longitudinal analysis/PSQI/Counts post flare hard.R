library(tidyverse)
library(magrittr)
library(survival)
library(binom)
library(survminer)
library(patchwork)


# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/PSQI/")

source("data_cleaning.R")

custom_theme = theme_minimal() + 
  theme(
    title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "top",
    plot.tag.position = c(0.55, 0.85),
    plot.tag = element_text(size = 12)
  )

palette <- c("#FFA500", "#0072B2", "grey")


# Complete for missing
data_hard_long %<>%
  tidyr::complete(ParticipantNo, month)

# Fill in the DiseaseFlare and time
data_hard_long %<>%
  dplyr::group_by(ParticipantNo) %>%
  tidyr::fill(DiseaseFlareYN, time) %>%
  dplyr::ungroup()

# Post flare flag
data_hard_long %<>%
  dplyr::mutate(
    post_flare_flag = (DiseaseFlareYN == 1 & time < as.numeric(month)*365/12)
    ) %>%
  # still at risk
  dplyr::mutate(
    at_risk_flag = (time >= as.numeric(month)*365/12)
  )

# Month
data_hard_long %<>%
  dplyr::mutate(month = factor(month))


# No previous flare
plot_hard_pre <- data_hard_long %>%
  dplyr::filter(
    month != 0,
    at_risk_flag == TRUE) %>%
  dplyr::mutate(
    SleepDisturbance = forcats::fct_na_value_to_level(SleepDisturbance, "Missing"),
    SleepDisturbance = forcats::fct_relevel(SleepDisturbance, "Missing")
  ) %>%
  dplyr::count(month, SleepDisturbance) %>%
  # Calculate percentage
  dplyr::group_by(month) %>%
  dplyr::mutate(
    p = n/sum(n[SleepDisturbance != "Missing"]),
    mid = rev(cumsum(rev(n))) - n/2,
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(    
    percent = glue::glue("{round(p*100)}%")
  ) %>%
  dplyr::mutate(
    percent = dplyr::case_when(
      SleepDisturbance == "Missing" ~ NA,
      TRUE ~ percent
    )
  ) %>%
  ggplot(aes(x = month, y = n, fill = SleepDisturbance)) +
  geom_col(width = 0.8, alpha = 0.5) +
  geom_text(
    aes(x = month, y = mid, label = percent),
    size = 3.5
  ) +
  scale_y_continuous(
    breaks = seq(0, 1750, 250),
    limits = c(0, 1750)
  ) +
  scale_fill_manual(
    values = palette,
    breaks = c("No", "Yes", "Missing")  
    ) +
  labs(
    fill = "Sleep disturbance",
    tag = "Still at risk of objective flare"
  ) +
  xlab("Month") +
  ylab("Count") +
  custom_theme

plot_hard_pre

# Post flare
plot_hard_post <- data_hard_long %>%
  dplyr::filter(
    month != 0,
    post_flare_flag == TRUE) %>%
  dplyr::mutate(
    SleepDisturbance = forcats::fct_na_value_to_level(SleepDisturbance, "Missing"),
    SleepDisturbance = forcats::fct_relevel(SleepDisturbance, "Missing")
  ) %>%
  dplyr::count(month, SleepDisturbance) %>%
  # Calculate percentage
  dplyr::group_by(month) %>%
  dplyr::mutate(
    p = n/sum(n[SleepDisturbance != "Missing"]),
    mid = rev(cumsum(rev(n))) - n/2,
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(    
    percent = glue::glue("{round(p*100)}%")
  ) %>%
  dplyr::mutate(
    percent = dplyr::case_when(
      SleepDisturbance == "Missing" ~ NA,
      TRUE ~ percent
    )
  ) %>%
  ggplot(aes(x = month, y = n, fill = SleepDisturbance)) +
  geom_col(width = 0.8, alpha = 0.5) +
  geom_text(
    aes(x = month, y = mid, label = percent),
    size = 3.5
  ) +
  scale_y_continuous(
    breaks = seq(0, 250, 50),
    limits = c(0, 250)
  ) +
  scale_fill_manual(
    values = palette,
    breaks = c("No", "Yes", "Missing")  
  ) +
  labs(
    fill = "Sleep disturbance",
    tag = "Post objective flare"
  ) +
  xlab("Month") +
  ylab("Count") +
  custom_theme

plot_hard_post


# Combine
plot_hard_pre + plot_hard_post +
  patchwork::plot_layout(
    ncol = 1, axes = 'collect', guides = 'collect'
  ) &
  theme(legend.position = "top")

# Save 7x7 landscape


# Together
plot_soft_pre + plot_hard_pre + plot_soft_post + plot_hard_post +
  patchwork::plot_layout(
    ncol = 2, guides = 'collect'
  ) &
  theme(legend.position = "top")

# Save 7.5x7.5