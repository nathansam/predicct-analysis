library(tidyverse)
library(magrittr)
library(survival)
library(binom)
library(survminer)
library(patchwork)


# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/HADS/")

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

palette <- c("#FFA500", "#0072B2", "grey", "#009E73")

data_anxiety_hard_long %<>% 
  # Anxiety groups
  dplyr::mutate(
    score_group = cut(
      anxiety_hads, 
      breaks = c(0, 7, 21), 
      labels = c("0-7", "8-21"), 
      include.lowest = TRUE)
  )

data_depression_hard_long %<>% 
  # Anxiety groups
  dplyr::mutate(
    score_group = cut(
      depression_hads, 
      breaks = c(0, 7, 21), 
      labels = c("0-7", "8-21"), 
      include.lowest = TRUE)
  )


# Post flare flag
data_anxiety_hard_long %<>%
  dplyr::mutate(
    post_flare_flag = (DiseaseFlareYN == 1 & time < as.numeric(month)*365/12)
  ) %>%
  # still at risk
  dplyr::mutate(
    at_risk_flag = (time >= as.numeric(month)*365/12)
  )

data_depression_hard_long %<>%
  dplyr::mutate(
    post_flare_flag = (DiseaseFlareYN == 1 & time < as.numeric(month)*365/12)
  ) %>%
  # still at risk
  dplyr::mutate(
    at_risk_flag = (time >= as.numeric(month)*365/12)
  )

# Anxiety ####
# No previous flare
plot_anxiety_hard_pre <- data_anxiety_hard_long %>%
  dplyr::filter(
    month != 0,
    at_risk_flag == TRUE) %>%
  dplyr::mutate(
    score_group = forcats::fct_na_value_to_level(score_group, "Missing"),
    score_group = forcats::fct_relevel(score_group, "Missing")
  ) %>%
  dplyr::count(month, score_group) %>%
  # Calculate percentage
  dplyr::group_by(month) %>%
  dplyr::mutate(
    p = n/sum(n[score_group != "Missing"]),
    mid = rev(cumsum(rev(n))) - n/2,
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(    
    percent = glue::glue("{round(p*100)}%")
    ) %>%
  dplyr::mutate(
    percent = dplyr::case_when(
      score_group == "Missing" ~ NA,
      TRUE ~ percent
    )
  ) %>%
  ggplot(aes(x = month, y = n, fill = score_group)) +
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
    breaks = c("0-7", "8-21", "Missing")
  ) +
  labs(
    fill = "Anxiety HADS",
    tag = "Still at risk of objective flare"
    ) +
  xlab("Month") +
  ylab("Count") +
  custom_theme

plot_anxiety_hard_pre

# Post flare
plot_anxiety_hard_post <- data_anxiety_hard_long %>%
  dplyr::filter(
    month != 0,
    post_flare_flag == TRUE) %>%
  dplyr::mutate(
    score_group = forcats::fct_na_value_to_level(score_group, "Missing"),
    score_group = forcats::fct_relevel(score_group, "Missing")
  ) %>%
  dplyr::count(month, score_group) %>%
  # Calculate percentage
  dplyr::group_by(month) %>%
  dplyr::mutate(
    p = n/sum(n[score_group != "Missing"]),
    mid = rev(cumsum(rev(n))) - n/2,
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(    
    percent = glue::glue("{round(p*100)}%")
  ) %>%
  dplyr::mutate(
    percent = dplyr::case_when(
      score_group == "Missing" ~ NA,
      TRUE ~ percent
    )
  ) %>%
  ggplot(aes(x = month, y = n, fill = score_group)) +
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
    breaks = c("0-7", "8-21", "Missing")
  ) +
  labs(
    fill = "Anxiety HADS",
    tag = "Post objective flare"
  ) +
  xlab("Month") +
  ylab("Count") +
  custom_theme

plot_anxiety_hard_post

# Depression ####
# No previous flare
plot_depression_hard_pre <- data_depression_hard_long %>%
  dplyr::filter(
    month != 0,
    at_risk_flag == TRUE) %>%
  dplyr::mutate(
    score_group = forcats::fct_na_value_to_level(score_group, "Missing"),
    score_group = forcats::fct_relevel(score_group, "Missing")
  ) %>%
  dplyr::count(month, score_group) %>%
  # Calculate percentage
  dplyr::group_by(month) %>%
  dplyr::mutate(
    p = n/sum(n[score_group != "Missing"]),
    mid = rev(cumsum(rev(n))) - n/2,
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(    
    percent = glue::glue("{round(p*100)}%")
  ) %>%
  dplyr::mutate(
    percent = dplyr::case_when(
      score_group == "Missing" ~ NA,
      TRUE ~ percent
    )
  ) %>%
  ggplot(aes(x = month, y = n, fill = score_group)) +
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
    breaks = c("0-7", "8-21", "Missing")
  ) +
  labs(
    fill = "Depression HADS",
    tag = "Still at risk of objective flare"
  ) +
  xlab("Month") +
  ylab("Count") +
  custom_theme

plot_depression_hard_pre

# Post flare
plot_depression_hard_post <- data_depression_hard_long %>%
  dplyr::filter(
    month != 0,
    post_flare_flag == TRUE) %>%
  dplyr::mutate(
    score_group = forcats::fct_na_value_to_level(score_group, "Missing"),
    score_group = forcats::fct_relevel(score_group, "Missing")
  ) %>%
  dplyr::count(month, score_group) %>%
  # Calculate percentage
  dplyr::group_by(month) %>%
  dplyr::mutate(
    p = n/sum(n[score_group != "Missing"]),
    mid = rev(cumsum(rev(n))) - n/2,
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(    
    percent = glue::glue("{round(p*100)}%")
  ) %>%
  dplyr::mutate(
    percent = dplyr::case_when(
      score_group == "Missing" ~ NA,
      TRUE ~ percent
    )
  ) %>%
  ggplot(aes(x = month, y = n, fill = score_group)) +
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
    breaks = c("0-7", "8-21", "Missing")
  ) +
  labs(
    fill = "Depression HADS",
    tag = "Post objective flare"
  ) +
  xlab("Month") +
  ylab("Count") +
  custom_theme

plot_depression_hard_post


# Save
plot_anxiety_hard_pre + plot_depression_hard_pre +
  plot_anxiety_hard_post + plot_depression_hard_post +
  patchwork::plot_layout(
    ncol = 2, axes = 'collect'
  )

# Save 8.5x7.5 landscape

# Together
plot_anxiety_soft_pre + plot_anxiety_hard_pre +
  plot_anxiety_soft_post + plot_anxiety_hard_post +
  patchwork::plot_layout(
    ncol = 2, 
    guides = "collect"
  ) &
  theme(legend.position = "top")

plot_depression_soft_pre + plot_depression_hard_pre +
  plot_depression_soft_post + plot_depression_hard_post +
  patchwork::plot_layout(
    ncol = 2, 
    guides = "collect"
  ) &
  theme(legend.position = "top")

# Save 7.5x7.5