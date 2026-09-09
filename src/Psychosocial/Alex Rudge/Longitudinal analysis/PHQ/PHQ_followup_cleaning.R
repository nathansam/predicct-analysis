# Tidy PHQ follow-up data

library(tidyverse)
library(magrittr)
library(readxl)


# Filepaths
data.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20221004/"


# Load follow-up PHQ data
phq_followup <- readxl::read_xlsx(
  paste0(data.path, "Followup/phq.xlsx")
)


# Data cleaning ####

# Clean longitudinal PHQ
phq_followup %<>%
  dplyr::filter(!is.na(ParticipantId))

# PHQ numbering scale was shifted by 1 - fix here
phq_followup %<>%
  dplyr::mutate(
    dplyr::across(
      .cols = StomachPain:TroubleSleeping,
      .fns = ~ . - 1
    )
  )

# Menstrual cramps replace NA with 0 (men)
phq_followup %<>%
  tidyr::replace_na(
    list(MenstrualCramps = 0)
  )

# Add all PHQ-15 columns to make the final score
phq_followup %<>%
  dplyr::mutate(
    TotalPHQ = StomachPain +
      BackPain +
      PainInArms +
      MenstrualCramps +
      Headaches +
      ChestPain +
      Dizziness +
      FaintingSpells +
      HeartPound +
      ShortnessBreath +
      SexualIntercourse +
      ConstipationDiarrhoea +
      NauseaIndigestion +
      FeelingTired +
      TroubleSleeping
  )

# Remove patients with any missing values
phq_followup %<>%
  dplyr::filter(!is.na(TotalPHQ))

# Group somatisation severity
phq_followup %<>%
  dplyr::mutate(
    somatisation = cut(
      TotalPHQ,
      breaks = c(0, 4, 9, 30),
      labels = c("None", "Mild", "ModSev"),
      include.lowest = TRUE
    )
  )

# Keep the follow-up scores and pivot them to wide format
phq_followup %<>%
  dplyr::select(
    ParticipantNo,
    TotalPHQ,
    somatisation,
    Q_month
  ) %>%
  tidyr::pivot_wider(
    names_from = Q_month,
    values_from = c(TotalPHQ, somatisation)
  )


# Save tidy PHQ follow-up data alongside HADS.rds
filepath_save <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/"

readr::write_rds(
  x = phq_followup,
  file = glue::glue("{filepath_save}phq_followup.rds")
)
