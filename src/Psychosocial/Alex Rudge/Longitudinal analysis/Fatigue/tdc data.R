library(tidyverse)
library(magrittr)
library(survminer)

# Run data cleaning

setwd("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/Fatigue/")

source("data_cleaning.R")


data_soft_static <- data_soft_long %>%
  dplyr::select(
    ParticipantNo, SiteNo, diagnosis2, Sex, age_decade, FC, Smoke, IMD, time, DiseaseFlareYN
  ) %>%
  # One row per person
  dplyr::group_by(ParticipantNo) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  # Remove people with time 0
  dplyr::filter(
    time > 0
  )

data_soft_td <- data_soft_long %>%
  dplyr::select(
    ParticipantNo, month, OftenLackEnergy
  ) %>%
  # Last observation carried forward
  dplyr::arrange(ParticipantNo, month) %>%
  dplyr::group_by(ParticipantNo) %>%
  tidyr::fill(OftenLackEnergy, .direction = "down") %>%
  # Convert month to days to match time
  dplyr::mutate(
    month = as.numeric(month),
    day = 365*month/12)

# Start stop
data_soft_merged <- tmerge(
  data_soft_static, 
  data_soft_static, 
  id = ParticipantNo, 
  endpoint = event(time, DiseaseFlareYN)) %>%
  tmerge(
    data2 = data_soft_td,
    id = ParticipantNo,
    OftenLackEnergy = tdc(day, OftenLackEnergy)
  )




## Hard ####
data_hard_static <- data_hard_long %>%
  dplyr::select(
    ParticipantNo, SiteNo, diagnosis2, Sex, age_decade, FC, Smoke, IMD, time, DiseaseFlareYN
  ) %>%
  # One row per person
  dplyr::group_by(ParticipantNo) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  # Remove people with time 0
  dplyr::filter(
    time > 0
  )

data_hard_td <- data_hard_long %>%
  dplyr::select(
    ParticipantNo, month, OftenLackEnergy
  ) %>%
  # Last observation carried forward
  dplyr::arrange(ParticipantNo, month) %>%
  dplyr::group_by(ParticipantNo) %>%
  tidyr::fill(OftenLackEnergy, .direction = "down") %>%
  # Convert month to days to match time
  dplyr::mutate(
    month = as.numeric(month),
    day = 365*month/12)

# Start stop
data_hard_merged <- tmerge(
  data_hard_static, 
  data_hard_static, 
  id = ParticipantNo, 
  endpoint = event(time, DiseaseFlareYN)) %>%
  tmerge(
    data2 = data_hard_td,
    id = ParticipantNo,
    OftenLackEnergy = tdc(day, OftenLackEnergy)
  )

