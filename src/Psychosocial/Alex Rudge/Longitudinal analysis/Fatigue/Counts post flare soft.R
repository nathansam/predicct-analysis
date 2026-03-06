library(tidyverse)
library(magrittr)
library(survival)
library(binom)
library(survminer)
library(patchwork)


# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/Fatigue/")

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


# Complete for missing
data_soft_long %<>%
  tidyr::complete(ParticipantNo, month)

# Fill in the DiseaseFlare and time
data_soft_long %<>%
  dplyr::group_by(ParticipantNo) %>%
  tidyr::fill(DiseaseFlareYN, time) %>%
  dplyr::ungroup()

# Post flare flag
data_soft_long %<>%
  dplyr::mutate(
    post_flare_flag = (DiseaseFlareYN == 1 & time < as.numeric(month)*365/12)
    ) %>%
  # still at risk
  dplyr::mutate(
    at_risk_flag = (time >= as.numeric(month)*365/12)
  )

# Month
data_soft_long %<>%
  dplyr::mutate(month = factor(month))

  

# Anxiety ####
# No previous flare
plot_soft_pre <- data_soft_long %>%
  dplyr::filter(
    month != 0,
    at_risk_flag == TRUE) %>%
  dplyr::mutate(
    OftenLackEnergy = forcats::fct_na_value_to_level(OftenLackEnergy, "Missing"),
    OftenLackEnergy = forcats::fct_relevel(OftenLackEnergy, "Missing")
  ) %>%
  dplyr::count(month, OftenLackEnergy) %>%
  # Calculate percentage
  dplyr::group_by(month) %>%
  dplyr::mutate(
    p = n/sum(n[OftenLackEnergy != "Missing"]),
    mid = rev(cumsum(rev(n))) - n/2,
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(    
    percent = glue::glue("{round(p*100)}%")
  ) %>%
  dplyr::mutate(
    percent = dplyr::case_when(
      OftenLackEnergy == "Missing" ~ NA,
      TRUE ~ percent
    )
  ) %>%
  ggplot(aes(x = month, y = n, fill = OftenLackEnergy)) +
  geom_col(width = 0.8, alpha = 0.5) +
  geom_text(
    aes(x = month, y = mid, label = percent),
    size = 3
  ) +
  scale_y_continuous(
    breaks = seq(0, 2000, 250),
    limits = c(0, 2000)
  ) +
  scale_fill_manual(
    values = palette,
    breaks = c("No", "Yes", "Missing")
  ) +
  labs(
    fill = "Fatigue",
    tag = "Still at risk of patient-reported flare"
  ) +
  xlab("Month") +
  ylab("Count") +
  custom_theme

plot_soft_pre

# Post flare
plot_soft_post <- data_soft_long %>%
  dplyr::filter(
    month != 0,
    post_flare_flag == TRUE) %>%
  dplyr::mutate(
    OftenLackEnergy = forcats::fct_na_value_to_level(OftenLackEnergy, "Missing"),
    OftenLackEnergy = forcats::fct_relevel(OftenLackEnergy, "Missing")
  ) %>%
  dplyr::count(month, OftenLackEnergy) %>%
  # Calculate percentage
  dplyr::group_by(month) %>%
  dplyr::mutate(
    p = n/sum(n[OftenLackEnergy != "Missing"]),
    mid = rev(cumsum(rev(n))) - n/2,
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(    
    percent = glue::glue("{round(p*100)}%")
  ) %>%
  dplyr::mutate(
    percent = dplyr::case_when(
      OftenLackEnergy == "Missing" ~ NA,
      TRUE ~ percent
    )
  ) %>%
  ggplot(aes(x = month, y = n, fill = OftenLackEnergy)) +
  geom_col(width = 0.8, alpha = 0.5) +
  geom_text(
    aes(x = month, y = mid, label = percent),
    size = 3
  ) +
  scale_y_continuous(
    breaks = seq(0, 700, 100),
    limits = c(0, 700)
  ) +
  scale_fill_manual(
    values = palette,
    breaks = c("No", "Yes", "Missing")
  ) +
  labs(
    fill = "Fatigue",
    tag = "Post patient-reported flare"
  ) +
  xlab("Month") +
  ylab("Count") +
  custom_theme

plot_soft_post


# Combine
plot_soft_pre + plot_soft_post +
  patchwork::plot_layout(
    ncol = 1, guides = 'collect'
  )

# Save 7.5x7.5 landscape