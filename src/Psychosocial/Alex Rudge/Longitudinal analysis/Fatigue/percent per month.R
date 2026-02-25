library(tidyverse)
library(magrittr)
library(survival)
library(binom)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/Fatigue/")

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

# Percentage of patients reporting fatigue per month stratified by flare

# Soft ####
data_soft_long %<>%
  dplyr::mutate(
    flare_in_last_month = (DiseaseFlareYN == 1 & time <= month*30 & time > month*30 - 30),
    flare_in_next_month = (DiseaseFlareYN == 1 & time <= month*30 + 30 & time > month*30)
  ) %>%
  dplyr::mutate(
    flare_in_last_month = as.numeric(flare_in_last_month),
    flare_in_next_month = as.numeric(flare_in_next_month)
  ) %>% 
  dplyr::mutate(
    flare_in_last_month = dplyr::case_match(
      flare_in_last_month,
      1 ~ "Yes",
      0 ~ "No"
    ),
    flare_in_next_month = dplyr::case_match(
      flare_in_next_month,
      1 ~ "Yes",
      0 ~ "No"
    )
  ) %>%
  # as factor
  dplyr::mutate(
    flare_in_last_month = factor(flare_in_last_month),
    flare_in_next_month = factor(flare_in_next_month)
  )





data_soft_proportion <- tibble()

for (m in 1:24){

data_temp1 <- data_soft_long %>%
  dplyr::filter(month == m-1) %>%
  dplyr::count(flare_in_next_month, OftenLackEnergy, .drop = FALSE) %>%
  dplyr::group_by(flare_in_next_month) %>%
  dplyr::mutate(
    p = n/sum(n),
    lower = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$lower,
    upper = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$upper
    ) %>%
  dplyr::filter(OftenLackEnergy == 'Yes') %>%
  dplyr::mutate(
    month = m-1,
    segment = m
    ) %>%
  dplyr::rename(flare_status = flare_in_next_month)

data_temp2 <- data_soft_long %>%
  dplyr::filter(month == m) %>%
  dplyr::count(flare_in_last_month, OftenLackEnergy, .drop = FALSE) %>%
  dplyr::group_by(flare_in_last_month) %>%
  dplyr::mutate(
    p = n/sum(n),
    lower = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$lower,
    upper = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$upper
    ) %>%
  dplyr::filter(OftenLackEnergy == 'Yes') %>%
  dplyr::mutate(
    month = m,
    segment = m
  ) %>%
  dplyr::rename(flare_status = flare_in_last_month)

data_soft_proportion %<>%
  dplyr::bind_rows(
    data_temp1
  ) %>%
  dplyr::bind_rows(
    data_temp2
  )

}

# Hard ####
data_hard_long %<>%
  dplyr::mutate(
    flare_in_last_month = (DiseaseFlareYN == 1 & time <= month*30 & time > month*30 - 30),
    flare_in_next_month = (DiseaseFlareYN == 1 & time <= month*30 + 30 & time > month*30)
  ) %>%
  dplyr::mutate(
    flare_in_last_month = as.numeric(flare_in_last_month),
    flare_in_next_month = as.numeric(flare_in_next_month)
  ) %>% 
  dplyr::mutate(
    flare_in_last_month = dplyr::case_match(
      flare_in_last_month,
      1 ~ "Yes",
      0 ~ "No"
    ),
    flare_in_next_month = dplyr::case_match(
      flare_in_next_month,
      1 ~ "Yes",
      0 ~ "No"
    )
  ) %>%
  # as factor
  dplyr::mutate(
    flare_in_last_month = factor(flare_in_last_month),
    flare_in_next_month = factor(flare_in_next_month)
  )


data_hard_proportion <- tibble()

for (m in 1:24){
  
  data_temp1 <- data_hard_long %>%
    dplyr::filter(month == m-1) %>%
    dplyr::count(flare_in_next_month, OftenLackEnergy, .drop = FALSE) %>%
    dplyr::group_by(flare_in_next_month) %>%
    dplyr::mutate(
      p = n/sum(n),
      lower = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$lower,
      upper = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$upper
    ) %>%
    dplyr::filter(OftenLackEnergy == 'Yes') %>%
    dplyr::mutate(
      month = m-1,
      segment = m
    ) %>%
    dplyr::rename(flare_status = flare_in_next_month)
  
  data_temp2 <- data_hard_long %>%
    dplyr::filter(month == m) %>%
    dplyr::count(flare_in_last_month, OftenLackEnergy, .drop = FALSE) %>%
    dplyr::group_by(flare_in_last_month) %>%
    dplyr::mutate(
      p = n/sum(n),
      lower = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$lower,
      upper = binom::binom.wilson(n, sum(n), methods = 'wilson', conf.level = 0.95)$upper
    ) %>%
    dplyr::filter(OftenLackEnergy == 'Yes') %>%
    dplyr::mutate(
      month = m,
      segment = m
    ) %>%
    dplyr::rename(flare_status = flare_in_last_month)
  
  data_hard_proportion %<>%
    dplyr::bind_rows(
      data_temp1
    ) %>%
    dplyr::bind_rows(
      data_temp2
    )
  
}

# Plots ####
# Soft
data_soft_proportion %>%
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
  scale_x_continuous(breaks = seq(0, 24, 1)) +
  xlab("Month") +
  ylab("Proportion reporting fatigue") +
  labs(
    colour = "Patient-reported flare in interval",
    fill = "Patient-reported flare in interval") +
  custom_theme

# Hard
data_hard_proportion %>%
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
  scale_x_continuous(breaks = seq(0, 24, 1)) +
  xlab("Month") +
  ylab("Proportion reporting fatigue") +
  labs(
    colour = "Objective flare in interval",
    fill = "Objective flare in interval") +
  custom_theme
