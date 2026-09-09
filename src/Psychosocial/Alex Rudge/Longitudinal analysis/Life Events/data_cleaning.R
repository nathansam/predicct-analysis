library(tidyverse)
library(magrittr)


# Filepaths
chiara <- "/Volumes/igmm/cvallejo-predicct/people/chiara/"
alex_data <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data"


# Load data
lifeevents <- readr::read_rds(
  glue::glue("{alex_data}/Life events.rds")
)

lifeevents_followup <- readr::read_rds(
  glue::glue("{alex_data}/lifeevents_followup.rds")
)

flares_soft <- readRDS(paste0(chiara, "flares_soft.RDS"))
flares_hard <- readRDS(paste0(chiara, "flares_hard.RDS"))


# Prepare longitudinal data
lifeevents_followup <- lifeevents_followup %>%
  dplyr::filter(ParticipantNo %in% lifeevents$ParticipantNo) %>%
  dplyr::rename(month = Q_month)

lifeevents <- lifeevents %>%
  dplyr::mutate(month = 0) %>%
  dplyr::bind_rows(lifeevents_followup) %>%
  dplyr::group_by(ParticipantNo) %>%
  tidyr::fill(SiteNo) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(ParticipantNo, month)


# Survival data
data_soft_long <- lifeevents %>%
  dplyr::inner_join(
    flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
    by = "ParticipantNo"
  ) %>%
  dplyr::rename(DiseaseFlareYN = softflare, time = softflare_time)

data_hard_long <- lifeevents %>%
  dplyr::inner_join(
    flares_hard %>% dplyr::select(ParticipantNo, hardflare, hardflare_time),
    by = "ParticipantNo"
  ) %>%
  dplyr::rename(DiseaseFlareYN = hardflare, time = hardflare_time)
