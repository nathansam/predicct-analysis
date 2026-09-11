library(tidyverse)
library(magrittr)
library(readxl)
library(openxlsx)


# File paths
data.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20221004/"
chiara <- "/Volumes/igmm/cvallejo-predicct/people/chiara/"
alex_data <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data"


# Baseline fatigue
fatigue <- readr::read_rds(
  glue::glue("{alex_data}/Fatigue.rds")
)

# Monthly questionnaires
fatigue_followup <- read.xlsx(
  paste0(data.path, "Followup/monthlyQ.xlsx")
)

flares_hard <- readRDS(paste0(chiara, "flares_hard.RDS"))
flares_soft <- readRDS(paste0(chiara, "flares_soft.RDS"))


# Monthly fatigue
fatigue_followup %<>%
  dplyr::select(ParticipantNo, OftenLackEnergy, Q_month) %>%
  # Remove NA energy
  dplyr::filter(!is.na(OftenLackEnergy)) %>%
  dplyr::mutate(
    OftenLackEnergy = dplyr::case_match(
      OftenLackEnergy,
      1 ~ "Yes",
      2 ~ "No",
      3 ~ "Not sure"
    )
  ) %>%
  # Group "Not sure" with "No" due to low counts
  dplyr::mutate(
    OftenLackEnergy = dplyr::case_when(
      OftenLackEnergy == "Not sure" ~ "No",
      TRUE ~ OftenLackEnergy
    )
  ) %>%
  dplyr::mutate(
    OftenLackEnergy = forcats::fct_relevel(OftenLackEnergy, "No", "Yes")
  )


# Combine with baseline
fatigue_followup %<>%
  dplyr::filter(ParticipantNo %in% fatigue$ParticipantNo)

fatigue %<>%
  dplyr::mutate(Q_month = 0) %>%
  dplyr::bind_rows(fatigue_followup) %>%
  dplyr::rename(month = Q_month) %>%
  dplyr::group_by(ParticipantNo) %>%
  tidyr::fill(SiteNo) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(ParticipantNo, month)


# Survival data
data_soft_long <- fatigue %>%
  dplyr::inner_join(
    flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
    by = "ParticipantNo"
  ) %>%
  dplyr::rename(DiseaseFlareYN = softflare, time = softflare_time)

data_hard_long <- fatigue %>%
  dplyr::inner_join(
    flares_hard %>% dplyr::select(ParticipantNo, hardflare, hardflare_time),
    by = "ParticipantNo"
  ) %>%
  dplyr::rename(DiseaseFlareYN = hardflare, time = hardflare_time)
