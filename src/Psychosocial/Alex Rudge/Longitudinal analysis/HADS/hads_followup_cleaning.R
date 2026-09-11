# Tidy HADS follow-up data

library(tidyverse)
library(magrittr)
library(readxl)


# Filepaths
data.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20221004/"


# Load follow-up HADS data
hads_followup <- readxl::read_xlsx(
  paste0(data.path, "Followup/hads.xlsx")
)


# Data cleaning ####

# Cleaning the longitudinal hads
hads_followup %<>% 
  filter(!is.na(ParticipantId))

# Rescale
hads_followup %<>%
  dplyr::mutate(
    dplyr::across(
      .cols = FeelTense:CanEnjoyBookTV,
      .fns = ~. - 1)
    )

#correcting inversely coded HADS
hads_followup %<>%
  dplyr::mutate(
    dplyr::across(
      .cols = c("FeelTense", 
                "FrightenedFeelingSomethingAwful", 
                "WorryingThoughts", 
                "FeelCheerful", 
                "FeelSlowedDown",
                "LostInterestAppearance",
                "FeelRestless",
                "SuddenFeelingsPanic"),
      .fns = ~ 3 - .
    )
  )

#anxiety HADS composite score
hads_followup %<>% 
  dplyr::mutate(anxiety_hads = FeelTense + 
                  FrightenedFeelingSomethingAwful + 
                  WorryingThoughts +
                  SitAtEase +
                  FrightenedFeelingButterflies +
                  FeelRestless +
                  SuddenFeelingsPanic)

#depression HADS composite score
hads_followup %<>% 
  dplyr::mutate(depression_hads = EnjoyThings + 
                  CanLaugh +
                  FeelCheerful +
                  FeelSlowedDown +
                  LostInterestAppearance +
                  LookForward +
                  CanEnjoyBookTV)

#remove breakdown hads columns
hads_followup %<>%
  dplyr::select(ParticipantNo, Q_month, anxiety_hads, depression_hads)

# Wide format
hads_followup %<>%
  tidyr::pivot_wider(names_from = Q_month, values_from = c(anxiety_hads, depression_hads))


# Save tidy HADS follow-up data alongside HADS.rds
filepath_save <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/"

readr::write_rds(
  x = hads_followup,
  file = glue::glue("{filepath_save}hads_followup.rds")
)
