library(tidyverse)
library(magrittr)
library(survival)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/Fatigue/")

source("data_cleaning.R")


# Percentage of patients reporting fatigue per month stratified by flare

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
  )


data_proportion <- tibble()

for (m in 1:24){

data_temp1 <- data_soft_long %>%
  dplyr::filter(month == m-1) %>%
  dplyr::count(flare_in_next_month, OftenLackEnergy) %>%
  dplyr::group_by(flare_in_next_month) %>%
  dplyr::mutate(p = n/sum(n)) %>%
  dplyr::filter(OftenLackEnergy == 'Yes') %>%
  dplyr::mutate(
    month = m-1,
    segment = m
    ) %>%
  dplyr::rename(flare_status = flare_in_next_month)

data_temp2 <- data_soft_long %>%
  dplyr::filter(month == m) %>%
  dplyr::count(flare_in_last_month, OftenLackEnergy) %>%
  dplyr::group_by(flare_in_last_month) %>%
  dplyr::mutate(p = n/sum(n)) %>%
  dplyr::filter(OftenLackEnergy == 'Yes') %>%
  dplyr::mutate(
    month = m,
    segment = m
  ) %>%
  dplyr::rename(flare_status = flare_in_last_month)

data_proportion %<>%
  dplyr::bind_rows(
    data_temp1
  ) %>%
  dplyr::bind_rows(
    data_temp2
  )

}


# Plot
data_proportion %>%
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
  geom_line()
