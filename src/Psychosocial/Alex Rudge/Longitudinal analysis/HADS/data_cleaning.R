library(tidyverse)
library(magrittr)
library(survival)
library(patchwork)


# Load data ####
chiara <- "/Volumes/igmm/cvallejo-predicct/people/chiara/"
alex_data <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data"

# Baseline HADS
hads <- readr::read_rds(
  glue::glue("{alex_data}/HADS.rds")
)

# Longitudinal HADS
hads_followup <- readr::read_rds(
  glue::glue("{alex_data}/hads_followup.rds")
)

# Flare data
flares_soft <- readRDS(paste0(chiara, "flares_soft.RDS"))
flares_hard <- readRDS(paste0(chiara, "flares_hard.RDS"))


# Prepare longitudinal data ####
# HADS in wide format
hads %<>%
  dplyr::select(-score_group) %>%
  tidyr::pivot_wider(
    names_from = hads_type,
    values_from = hads_score
  )

# Add the follow-up HADS
hads %<>%
  dplyr::left_join(
    hads_followup,
    by = "ParticipantNo"
  )


# Survival data
data_anxiety_soft <- hads %>%
  dplyr::select(-tidyselect::starts_with("depression")) %>%
  dplyr::inner_join(
    flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
    by = "ParticipantNo"
  ) %>%
  dplyr::rename(DiseaseFlareYN = softflare, time = softflare_time)

data_anxiety_hard <- hads %>%
  dplyr::select(-tidyselect::starts_with("depression")) %>%
  dplyr::inner_join(
    flares_hard %>% dplyr::select(ParticipantNo, hardflare, hardflare_time),
    by = "ParticipantNo"
  ) %>%
  dplyr::rename(DiseaseFlareYN = hardflare, time = hardflare_time)

# Depression
data_depression_soft <- hads %>%
  dplyr::select(-tidyselect::starts_with("anxiety")) %>%
  dplyr::inner_join(
    flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
    by = "ParticipantNo"
  ) %>%
  dplyr::rename(DiseaseFlareYN = softflare, time = softflare_time)

data_depression_hard <- hads %>%
  dplyr::select(-tidyselect::starts_with("anxiety")) %>%
  dplyr::inner_join(
    flares_hard %>% dplyr::select(ParticipantNo, hardflare, hardflare_time),
    by = "ParticipantNo"
  ) %>%
  dplyr::rename(DiseaseFlareYN = hardflare, time = hardflare_time)


# Long format
# Anxiety
data_anxiety_soft_long <- data_anxiety_soft %>%
  dplyr::rename(anxiety_hads_0 = anxiety_hads) %>%
  tidyr::pivot_longer(
    cols = c(anxiety_hads_0, anxiety_hads_12, anxiety_hads_24),
    names_to = "month",
    names_prefix = "anxiety_hads_",
    values_to = "anxiety_hads"
  )

data_anxiety_hard_long <- data_anxiety_hard %>%
  dplyr::rename(anxiety_hads_0 = anxiety_hads) %>%
  tidyr::pivot_longer(
    cols = c(anxiety_hads_0, anxiety_hads_12, anxiety_hads_24),
    names_to = "month",
    names_prefix = "anxiety_hads_",
    values_to = "anxiety_hads"
  )

# Depression
data_depression_soft_long <- data_depression_soft %>%
  dplyr::rename(depression_hads_0 = depression_hads) %>%
  tidyr::pivot_longer(
    cols = c(depression_hads_0, depression_hads_12, depression_hads_24),
    names_to = "month",
    names_prefix = "depression_hads_",
    values_to = "depression_hads"
  )

data_depression_hard_long <- data_depression_hard %>%
  dplyr::rename(depression_hads_0 = depression_hads) %>%
  tidyr::pivot_longer(
    cols = c(depression_hads_0, depression_hads_12, depression_hads_24),
    names_to = "month",
    names_prefix = "depression_hads_",
    values_to = "depression_hads"
  )
