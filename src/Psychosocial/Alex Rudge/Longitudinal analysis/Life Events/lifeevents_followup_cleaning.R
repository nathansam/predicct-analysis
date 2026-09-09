# Tidy Life Events follow-up data

library(tidyverse)
library(magrittr)
library(readxl)


# Filepaths
data.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20221004/"


# Load follow-up Life Events data
lifeevents_followup <- readxl::read_xlsx(
  paste0(data.path, "Followup/lifeevents.xlsx")
)


# Data cleaning
lifeevents_followup %<>%
  dplyr::filter(
    !is.na(ParticipantId),
    !is.na(AnyLifeEvents)
  ) %>%
  dplyr::mutate(
    AnyLifeEvents = factor(
      AnyLifeEvents,
      levels = c(1, 2),
      labels = c("Yes", "No")
    ),
    AnyLifeEvents = forcats::fct_relevel(AnyLifeEvents, "No", "Yes")
  ) %>%
  dplyr::select(
    ParticipantNo,
    AnyLifeEvents,
    Q_month
  )


# Save tidy follow-up Life Events data
alex_data <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data"

readr::write_rds(
  x = lifeevents_followup,
  file = glue::glue("{alex_data}/lifeevents_followup.rds")
)
