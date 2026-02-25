library(tidyverse)
library(magrittr)
library(survival)
library(binom)
library(survminer)

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
## Soft ####

data_anxiety_soft_long %<>%
  dplyr::mutate(
    flare_in_year1 = as.numeric((DiseaseFlareYN == 1 & time <= 365)),
    flare_in_year2 = as.numeric((DiseaseFlareYN == 1 & time > 365))
  ) %>% 
  dplyr::mutate(
    flare_in_year1 = dplyr::case_match(
      flare_in_year1,
      1 ~ "Yes",
      0 ~ "No"
    ),
    flare_in_year2 = dplyr::case_match(
      flare_in_year2,
      1 ~ "Yes",
      0 ~ "No"
    )
  ) %>%
  # as factor
  dplyr::mutate(
    flare_in_year1 = factor(flare_in_year1),
    flare_in_year2 = factor(flare_in_year2)
  ) %>%
  # Anxiety groups
  dplyr::mutate(
    score_group = cut(
      anxiety_hads, 
      breaks = c(0, 7, 21), 
      labels = c("0-7", "8-21"), 
      include.lowest = TRUE)
  )


data_anxiety_soft_proportion <- data_anxiety_soft_long %>%
  # Baseline
  dplyr::filter(month == 0) %>%
  dplyr::count(flare_in_year1, score_group) %>%
  dplyr::group_by(flare_in_year1) %>%
  dplyr::mutate(
    p = n/sum(n),
    lower = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$lower,
    upper = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$upper
  ) %>%
  dplyr::filter(score_group == '8-21') %>%
  dplyr::mutate(
    month = 0,
    segment = 1) %>%
  dplyr::rename(flare_status = flare_in_year1) %>%
  dplyr::bind_rows(
    # 12 month, segment 1
    data_anxiety_soft_long %>%
      # Baseline
      dplyr::filter(month == 12) %>%
      dplyr::count(flare_in_year1, score_group) %>%
      dplyr::group_by(flare_in_year1) %>%
      dplyr::mutate(
        p = n/sum(n),
        lower = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$lower,
        upper = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$upper
      ) %>%
      dplyr::filter(score_group == '8-21') %>%
      dplyr::mutate(
        month = 12,
        segment = 1) %>%
      dplyr::rename(flare_status = flare_in_year1)
  ) %>%
  dplyr::bind_rows (
    # 12 months segment 2
    data_anxiety_soft_long %>%
      # Baseline
      dplyr::filter(month == 12) %>%
      dplyr::count(flare_in_year2, score_group) %>%
      dplyr::group_by(flare_in_year2) %>%
      dplyr::mutate(
        p = n/sum(n),
        lower = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$lower,
        upper = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$upper
      ) %>%
      dplyr::filter(score_group == '8-21') %>%
      dplyr::mutate(
        month = 12,
        segment = 2) %>%
      dplyr::rename(flare_status = flare_in_year2)
  ) %>%
  dplyr::bind_rows(
    # 24 months, segment 2
    data_anxiety_soft_long %>%
      # Baseline
      dplyr::filter(month == 24) %>%
      dplyr::count(flare_in_year2, score_group) %>%
      dplyr::group_by(flare_in_year2) %>%
      dplyr::mutate(
        p = n/sum(n),
        lower = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$lower,
        upper = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$upper
      ) %>%
      dplyr::filter(score_group == '8-21') %>%
      dplyr::mutate(
        month = 24, 
        segment = 2) %>%
      dplyr::rename(flare_status = flare_in_year2)
  )
  
## Hard ####
data_anxiety_hard_long %<>%
  dplyr::mutate(
    flare_in_year1 = as.numeric((DiseaseFlareYN == 1 & time <= 365)),
    flare_in_year2 = as.numeric((DiseaseFlareYN == 1 & time > 365))
  ) %>% 
  dplyr::mutate(
    flare_in_year1 = dplyr::case_match(
      flare_in_year1,
      1 ~ "Yes",
      0 ~ "No"
    ),
    flare_in_year2 = dplyr::case_match(
      flare_in_year2,
      1 ~ "Yes",
      0 ~ "No"
    )
  ) %>%
  # as factor
  dplyr::mutate(
    flare_in_year1 = factor(flare_in_year1),
    flare_in_year2 = factor(flare_in_year2)
  ) %>%
  # Anxiety groups
  dplyr::mutate(
    score_group = cut(
      anxiety_hads, 
      breaks = c(0, 7, 21), 
      labels = c("0-7", "8-21"), 
      include.lowest = TRUE)
  )


data_anxiety_hard_proportion <- data_anxiety_hard_long %>%
  # Baseline
  dplyr::filter(month == 0) %>%
  dplyr::count(flare_in_year1, score_group, .drop = FALSE) %>%
  dplyr::group_by(flare_in_year1) %>%
  dplyr::mutate(
    p = n/sum(n),
    lower = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$lower,
    upper = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$upper
  ) %>%
  dplyr::filter(score_group == '8-21') %>%
  dplyr::mutate(
    month = 0,
    segment = 1) %>%
  dplyr::rename(flare_status = flare_in_year1) %>%
  dplyr::bind_rows(
    # 12 month, segment 1
    data_anxiety_hard_long %>%
      # Baseline
      dplyr::filter(month == 12) %>%
      dplyr::count(flare_in_year1, score_group, .drop = FALSE) %>%
      dplyr::group_by(flare_in_year1) %>%
      dplyr::mutate(
        p = n/sum(n),
        lower = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$lower,
        upper = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$upper
      ) %>%
      dplyr::filter(score_group == '8-21') %>%
      dplyr::mutate(
        month = 12,
        segment = 1) %>%
      dplyr::rename(flare_status = flare_in_year1)
  ) %>%
  dplyr::bind_rows (
    # 12 months segment 2
    data_anxiety_hard_long %>%
      # Baseline
      dplyr::filter(month == 12) %>%
      dplyr::count(flare_in_year2, score_group, .drop = FALSE) %>%
      dplyr::group_by(flare_in_year2) %>%
      dplyr::mutate(
        p = n/sum(n),
        lower = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$lower,
        upper = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$upper
      ) %>%
      dplyr::filter(score_group == '8-21') %>%
      dplyr::mutate(
        month = 12,
        segment = 2) %>%
      dplyr::rename(flare_status = flare_in_year2)
  ) %>%
  dplyr::bind_rows(
    # 24 months, segment 2
    data_anxiety_hard_long %>%
      # Baseline
      dplyr::filter(month == 24) %>%
      dplyr::count(flare_in_year2, score_group, .drop = FALSE) %>%
      dplyr::group_by(flare_in_year2) %>%
      dplyr::mutate(
        p = n/sum(n),
        lower = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$lower,
        upper = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$upper
      ) %>%
      dplyr::filter(score_group == '8-21') %>%
      dplyr::mutate(
        month = 24, 
        segment = 2) %>%
      dplyr::rename(flare_status = flare_in_year2)
  )

# Depression ####
## Soft ####

data_depression_soft_long %<>%
  dplyr::mutate(
    flare_in_year1 = as.numeric((DiseaseFlareYN == 1 & time <= 365)),
    flare_in_year2 = as.numeric((DiseaseFlareYN == 1 & time > 365))
  ) %>% 
  dplyr::mutate(
    flare_in_year1 = dplyr::case_match(
      flare_in_year1,
      1 ~ "Yes",
      0 ~ "No"
    ),
    flare_in_year2 = dplyr::case_match(
      flare_in_year2,
      1 ~ "Yes",
      0 ~ "No"
    )
  ) %>%
  # depression groups
  dplyr::mutate(
    score_group = cut(
      depression_hads, 
      breaks = c(0, 7, 21), 
      labels = c("0-7", "8-21"), 
      include.lowest = TRUE)
  )


data_depression_soft_proportion <- data_depression_soft_long %>%
  # Baseline
  dplyr::filter(month == 0) %>%
  dplyr::count(flare_in_year1, score_group, .drop = FALSE) %>%
  dplyr::group_by(flare_in_year1) %>%
  dplyr::mutate(
    p = n/sum(n),
    lower = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$lower,
    upper = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$upper
  ) %>%
  dplyr::filter(score_group == '8-21') %>%
  dplyr::mutate(
    month = 0,
    segment = 1) %>%
  dplyr::rename(flare_status = flare_in_year1) %>%
  dplyr::bind_rows(
    # 12 month, segment 1
    data_depression_soft_long %>%
      dplyr::filter(month == 12) %>%
      dplyr::count(flare_in_year1, score_group, .drop = FALSE) %>%
      dplyr::group_by(flare_in_year1) %>%
      dplyr::mutate(
        p = n/sum(n),
        lower = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$lower,
        upper = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$upper
      ) %>%
      dplyr::filter(score_group == '8-21') %>%
      dplyr::mutate(
        month = 12,
        segment = 1) %>%
      dplyr::rename(flare_status = flare_in_year1)
  ) %>%
  dplyr::bind_rows (
    # 12 months segment 2
    data_depression_soft_long %>%
      dplyr::filter(month == 12) %>%
      dplyr::count(flare_in_year2, score_group, .drop = FALSE) %>%
      dplyr::group_by(flare_in_year2) %>%
      dplyr::mutate(
        p = n/sum(n),
        lower = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$lower,
        upper = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$upper
      ) %>%
      dplyr::filter(score_group == '8-21') %>%
      dplyr::mutate(
        month = 12,
        segment = 2) %>%
      dplyr::rename(flare_status = flare_in_year2)
  ) %>%
  dplyr::bind_rows(
    # 24 months, segment 2
    data_depression_soft_long %>%
      dplyr::filter(month == 24) %>%
      dplyr::count(flare_in_year2, score_group, .drop = FALSE) %>%
      dplyr::group_by(flare_in_year2) %>%
      dplyr::mutate(
        p = n/sum(n),
        lower = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$lower,
        upper = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$upper
      ) %>%
      dplyr::filter(score_group == '8-21') %>%
      dplyr::mutate(
        month = 24, 
        segment = 2) %>%
      dplyr::rename(flare_status = flare_in_year2)
  )

## Hard ####
data_depression_hard_long %<>%
  dplyr::mutate(
    flare_in_year1 = as.numeric((DiseaseFlareYN == 1 & time <= 365)),
    flare_in_year2 = as.numeric((DiseaseFlareYN == 1 & time > 365))
  ) %>% 
  dplyr::mutate(
    flare_in_year1 = dplyr::case_match(
      flare_in_year1,
      1 ~ "Yes",
      0 ~ "No"
    ),
    flare_in_year2 = dplyr::case_match(
      flare_in_year2,
      1 ~ "Yes",
      0 ~ "No"
    )
  ) %>%
  # depression groups
  dplyr::mutate(
    score_group = cut(
      depression_hads, 
      breaks = c(0, 7, 21), 
      labels = c("0-7", "8-21"), 
      include.lowest = TRUE)
  )


data_depression_hard_proportion <- data_depression_hard_long %>%
  # Baseline
  dplyr::filter(month == 0) %>%
  dplyr::count(flare_in_year1, score_group, .drop = FALSE) %>%
  dplyr::group_by(flare_in_year1) %>%
  dplyr::mutate(
    p = n/sum(n),
    lower = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$lower,
    upper = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$upper
  ) %>%
  dplyr::filter(score_group == '8-21') %>%
  dplyr::mutate(
    month = 0,
    segment = 1) %>%
  dplyr::rename(flare_status = flare_in_year1) %>%
  dplyr::bind_rows(
    # 12 month, segment 1
    data_depression_hard_long %>%
      # Baseline
      dplyr::filter(month == 12) %>%
      dplyr::count(flare_in_year1, score_group, .drop = FALSE) %>%
      dplyr::group_by(flare_in_year1) %>%
      dplyr::mutate(
        p = n/sum(n),
        lower = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$lower,
        upper = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$upper
      ) %>%
      dplyr::filter(score_group == '8-21') %>%
      dplyr::mutate(
        month = 12,
        segment = 1) %>%
      dplyr::rename(flare_status = flare_in_year1)
  ) %>%
  dplyr::bind_rows (
    # 12 months segment 2
    data_depression_hard_long %>%
      # Baseline
      dplyr::filter(month == 12) %>%
      dplyr::count(flare_in_year2, score_group, .drop = FALSE) %>%
      dplyr::group_by(flare_in_year2) %>%
      dplyr::mutate(
        p = n/sum(n),
        lower = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$lower,
        upper = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$upper
      ) %>%
      dplyr::filter(score_group == '8-21') %>%
      dplyr::mutate(
        month = 12,
        segment = 2) %>%
      dplyr::rename(flare_status = flare_in_year2)
  ) %>%
  dplyr::bind_rows(
    # 24 months, segment 2
    data_depression_hard_long %>%
      # Baseline
      dplyr::filter(month == 24) %>%
      dplyr::count(flare_in_year2, score_group, .drop = FALSE) %>%
      dplyr::group_by(flare_in_year2) %>%
      dplyr::mutate(
        p = n/sum(n),
        lower = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$lower,
        upper = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$upper
      ) %>%
      dplyr::filter(score_group == '8-21') %>%
      dplyr::mutate(
        month = 24, 
        segment = 2) %>%
      dplyr::rename(flare_status = flare_in_year2)
  )



# Plots ####
# Anxiety
# Soft
data_anxiety_soft_proportion %>%
  dplyr::mutate(group = interaction(flare_status, segment)) %>%
  ggplot(aes(
    x = month, 
    y = p, 
    group = group, 
    colour = flare_status,
    fill = flare_status
    )
  ) +  
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
  scale_x_continuous(breaks = c(0, 12, 24)) +
  xlab("Month") +
  ylab("Proportion with elevated anxiety") +
  labs(
    colour = "Patient-reported flare in interval",
    fill = "Patient-reported flare in interval") +
  custom_theme

# Hard
data_anxiety_hard_proportion %>%
  dplyr::mutate(group = interaction(flare_status, segment)) %>%
  ggplot(aes(
    x = month, 
    y = p, 
    group = group, 
    colour = flare_status,
    fill = flare_status
  )
  ) +  
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
  scale_x_continuous(breaks = c(0, 12, 24)) +
  xlab("Month") +
  ylab("Proportion with elevated anxiety") +
  labs(
    colour = "Objective flare in interval",
    fill = "Objective flare in interval") +
  custom_theme

# Depression
# Soft
data_depression_soft_proportion %>%
  dplyr::mutate(group = interaction(flare_status, segment)) %>%
  ggplot(aes(
    x = month, 
    y = p, 
    group = group, 
    colour = flare_status,
    fill = flare_status
  )
  ) +  
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
  scale_x_continuous(breaks = c(0, 12, 24)) +
  xlab("Month") +
  ylab("Proportion with elevated depression") +
  labs(
    colour = "Patient-reported flare in interval",
    fill = "Patient-reported flare in interval") +
  custom_theme

# Hard
data_depression_hard_proportion %>%
  dplyr::mutate(group = interaction(flare_status, segment)) %>%
  ggplot(aes(
    x = month, 
    y = p, 
    group = group, 
    colour = flare_status,
    fill = flare_status
  )
  ) +  
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
  scale_x_continuous(breaks = c(0, 12, 24)) +
  xlab("Month") +
  ylab("Proportion with elevated depression") +
  labs(
    colour = "Objective flare in interval",
    fill = "Objective flare in interval") +
  custom_theme
