library(tidyverse)
library(magrittr)


# Filepaths
chiara <- "/Volumes/igmm/cvallejo-predicct/people/chiara/"
alex_data <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data"


# Load data
phq <- readr::read_rds(
  glue::glue("{alex_data}/PHQ.rds")
)

phq_followup <- readr::read_rds(
  glue::glue("{alex_data}/phq_followup.rds")
)

flares_hard <- readRDS(paste0(chiara, "flares_hard.RDS"))
flares_soft <- readRDS(paste0(chiara, "flares_soft.RDS"))



# Data cleaning

phq <- phq %>% 
  filter(!is.na(ParticipantId))

# Join longitudinal and baseline
phq %<>%
  dplyr::left_join(
    phq_followup, by = "ParticipantNo"
  )

# Survival data
data_soft <- phq %>%
  dplyr::inner_join(
    flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::rename(DiseaseFlareYN = softflare, time = softflare_time)


data_hard <- phq %>%
  dplyr::inner_join(
    flares_hard %>% dplyr::select(ParticipantNo, hardflare, hardflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::rename(DiseaseFlareYN = hardflare, time = hardflare_time)


# Create generic long-format PHQ data

phq_long <- phq %>%
  dplyr::rename(
    somatisation_0 = somatisation,
    TotalPHQ_0 = TotalPHQ
  ) %>%
  tidyr::pivot_longer(
    cols = c(somatisation_0, somatisation_12, somatisation_24, TotalPHQ_0, TotalPHQ_12, TotalPHQ_24),
    names_to = c(".value", "month"),
    names_sep = "_"
  ) %>%
  # Month numeric
  dplyr::mutate(
    month = as.numeric(month)
  )

filepath_save <- '/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Longitudinal analysis/'

readr::write_rds(
  x = phq_long,
  file = glue::glue("{filepath_save}phq_long.rds")
)

# Add soft-flare information to the long-format PHQ data
data_soft_long <- phq_long %>%
  dplyr::inner_join(
    flares_soft %>% dplyr::select(
      ParticipantNo,
      softflare,
      softflare_time
    ),
    by = "ParticipantNo"
  ) %>%
  dplyr::rename(
    DiseaseFlareYN = softflare,
    time = softflare_time
  )

# Add hard-flare information to the long-format PHQ data
data_hard_long <- phq_long %>%
  dplyr::inner_join(
    flares_hard %>% dplyr::select(
      ParticipantNo,
      hardflare,
      hardflare_time
    ),
    by = "ParticipantNo"
  ) %>%
  dplyr::rename(
    DiseaseFlareYN = hardflare,
    time = hardflare_time
  )


