library(tidyverse)
library(magrittr)
library(readxl)
library(openxlsx)


# File paths
data.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20221004/"
chiara <- "/Volumes/igmm/cvallejo-predicct/people/chiara/"
alex_data <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data"


# Baseline control
control <- read.xlsx(
  paste0(data.path, "Baseline2022/IBD.xlsx")
)

# Monthly questionnaires
control_followup <- read.xlsx(
  paste0(data.path, "Followup/monthlyQ.xlsx")
)

flares_hard <- readRDS(paste0(chiara, "flares_hard.RDS"))
flares_soft <- readRDS(paste0(chiara, "flares_soft.RDS"))


# Calculating control score
control %<>%
  dplyr::select(
    ParticipantNo,
    SiteNo,
    TreatmentUseful,
    MissPlannedActivities,
    WakeUpAtNight,
    SignificantPain,
    OftenLackEnergy,
    AnxiousDepressed,
    NeedChangeTreatment,
    OverallControl
  )


# Scoring system: Negative answer = 0, not sure = 1, positive = 2
# Variables coded as Yes, No, Not sure = 1, 2, 3

# DiseaseControlled
# At baseline all supposed to be controlled
control %<>%
  dplyr::mutate(
    DiseaseControlled = "Yes"
  )


# TreatmentUseful - also has 4, not taking any treatment; worth 2 points.
control %<>%
  dplyr::mutate(
    TreatmentUseful = dplyr::case_match(
      TreatmentUseful,
      1 ~ "Yes",
      2 ~ "No",
      3 ~ "Not sure",
      4 ~ "Not on any treatment",
      .default = NA_character_
    )
  )

# MissPlannedActivities
control %<>%
  dplyr::mutate(
    MissPlannedActivities = dplyr::case_match(
      MissPlannedActivities,
      1 ~ "Yes",
      2 ~ "No",
      3 ~ "Not sure",
      .default = NA_character_
    )
  )

# WakeUpAtNight
control %<>%
  dplyr::mutate(
    WakeUpAtNight = dplyr::case_match(
      WakeUpAtNight,
      1 ~ "Yes",
      2 ~ "No",
      3 ~ "Not sure",
      .default = NA_character_
    )
  )

# SignificantPain
control %<>%
  dplyr::mutate(
    SignificantPain = dplyr::case_match(
      SignificantPain,
      1 ~ "Yes",
      2 ~ "No",
      3 ~ "Not sure",
      .default = NA_character_
    )
  )

# OftenLackEnergy
control %<>%
  dplyr::mutate(
    OftenLackEnergy = dplyr::case_match(
      OftenLackEnergy,
      1 ~ "Yes",
      2 ~ "No",
      3 ~ "Not sure",
      .default = NA_character_
    )
  )

# AnxiousDepressed
control %<>%
  dplyr::mutate(
    AnxiousDepressed = dplyr::case_match(
      AnxiousDepressed,
      1 ~ "Yes",
      2 ~ "No",
      3 ~ "Not sure",
      .default = NA_character_
    )
  )

# NeedChangeTreatment
control %<>%
  dplyr::mutate(
    NeedChangeTreatment = dplyr::case_match(
      NeedChangeTreatment,
      1 ~ "Yes",
      2 ~ "No",
      3 ~ "Not sure",
      .default = NA_character_
    )
  )

# Add control score components and final score
control %<>%
  dplyr::mutate(
    DiseaseControlled_score = dplyr::case_match(
      DiseaseControlled,
      "Yes" ~ 2,
      "No" ~ 0,
      "Not sure" ~ 1,
      .default = NA_real_
    ),
    TreatmentUseful_score = dplyr::case_match(
      TreatmentUseful,
      "Yes" ~ 2,
      "No" ~ 0,
      "Not sure" ~ 1,
      "Not on any treatment" ~ 2,
      .default = NA_real_
    ),
    MissPlannedActivities_score = dplyr::case_match(
      MissPlannedActivities,
      "Yes" ~ 0,
      "No" ~ 2,
      "Not sure" ~ 1,
      .default = NA_real_
    ),
    WakeUpAtNight_score = dplyr::case_match(
      WakeUpAtNight,
      "Yes" ~ 0,
      "No" ~ 2,
      "Not sure" ~ 1,
      .default = NA_real_
    ),
    SignificantPain_score = dplyr::case_match(
      SignificantPain,
      "Yes" ~ 0,
      "No" ~ 2,
      "Not sure" ~ 1,
      .default = NA_real_
    ),
    OftenLackEnergy_score = dplyr::case_match(
      OftenLackEnergy,
      "Yes" ~ 0,
      "No" ~ 2,
      "Not sure" ~ 1,
      .default = NA_real_
    ),
    AnxiousDepressed_score = dplyr::case_match(
      AnxiousDepressed,
      "Yes" ~ 0,
      "No" ~ 2,
      "Not sure" ~ 1,
      .default = NA_real_
    ),
    NeedChangeTreatment_score = dplyr::case_match(
      NeedChangeTreatment,
      "Yes" ~ 0,
      "No" ~ 2,
      "Not sure" ~ 1,
      .default = NA_real_
    ),
    control_8 = rowSums(
      dplyr::across(
        c(
          DiseaseControlled_score,
          TreatmentUseful_score,
          MissPlannedActivities_score,
          WakeUpAtNight_score,
          SignificantPain_score,
          OftenLackEnergy_score,
          AnxiousDepressed_score,
          NeedChangeTreatment_score
        )
      ),
      na.rm = FALSE
    )
  ) %>%
  dplyr::select(-tidyselect::ends_with("_score"))

# Follow up
control_followup %<>%
  dplyr::select(
    ParticipantNo,
    TimepointId,
    Q_month,
    DiseaseControlled,
    TreatmentUseful,
    MissPlannedActivities,
    WakeUpAtNight,
    SignificantPain,
    OftenLackEnergy,
    AnxiousDepressed,
    NeedChangeTreatment,
    OverallControl
  )

# DiseaseControlled
control_followup %<>%
  dplyr::mutate(
    DiseaseControlled = dplyr::case_match(
      DiseaseControlled,
      1 ~ "Yes",
      2 ~ "No",
      3 ~ "Not sure",
      .default = NA_character_
    )
  )

# TreatmentUseful - also has 4, not taking any treatment; worth 2 points.
control_followup %<>%
  dplyr::mutate(
    TreatmentUseful = dplyr::case_match(
      TreatmentUseful,
      1 ~ "Yes",
      2 ~ "No",
      3 ~ "Not sure",
      4 ~ "Not on any treatment",
      .default = NA_character_
    )
  )

# MissPlannedActivities
control_followup %<>%
  dplyr::mutate(
    MissPlannedActivities = dplyr::case_match(
      MissPlannedActivities,
      1 ~ "Yes",
      2 ~ "No",
      3 ~ "Not sure",
      .default = NA_character_
    )
  )

# WakeUpAtNight
control_followup %<>%
  dplyr::mutate(
    WakeUpAtNight = dplyr::case_match(
      WakeUpAtNight,
      1 ~ "Yes",
      2 ~ "No",
      3 ~ "Not sure",
      .default = NA_character_
    )
  )

# SignificantPain
control_followup %<>%
  dplyr::mutate(
    SignificantPain = dplyr::case_match(
      SignificantPain,
      1 ~ "Yes",
      2 ~ "No",
      3 ~ "Not sure",
      .default = NA_character_
    )
  )

# OftenLackEnergy
control_followup %<>%
  dplyr::mutate(
    OftenLackEnergy = dplyr::case_match(
      OftenLackEnergy,
      1 ~ "Yes",
      2 ~ "No",
      3 ~ "Not sure",
      .default = NA_character_
    )
  )

# AnxiousDepressed
control_followup %<>%
  dplyr::mutate(
    AnxiousDepressed = dplyr::case_match(
      AnxiousDepressed,
      1 ~ "Yes",
      2 ~ "No",
      3 ~ "Not sure",
      .default = NA_character_
    )
  )

# NeedChangeTreatment
control_followup %<>%
  dplyr::mutate(
    NeedChangeTreatment = dplyr::case_match(
      NeedChangeTreatment,
      1 ~ "Yes",
      2 ~ "No",
      3 ~ "Not sure",
      .default = NA_character_
    )
  )

# Add control score components and final score
control_followup %<>%
  dplyr::mutate(
    DiseaseControlled_score = dplyr::case_match(
      DiseaseControlled,
      "Yes" ~ 2,
      "No" ~ 0,
      "Not sure" ~ 1,
      .default = NA_real_
    ),
    TreatmentUseful_score = dplyr::case_match(
      TreatmentUseful,
      "Yes" ~ 2,
      "No" ~ 0,
      "Not sure" ~ 1,
      "Not on any treatment" ~ 2,
      .default = NA_real_
    ),
    MissPlannedActivities_score = dplyr::case_match(
      MissPlannedActivities,
      "Yes" ~ 0,
      "No" ~ 2,
      "Not sure" ~ 1,
      .default = NA_real_
    ),
    WakeUpAtNight_score = dplyr::case_match(
      WakeUpAtNight,
      "Yes" ~ 0,
      "No" ~ 2,
      "Not sure" ~ 1,
      .default = NA_real_
    ),
    SignificantPain_score = dplyr::case_match(
      SignificantPain,
      "Yes" ~ 0,
      "No" ~ 2,
      "Not sure" ~ 1,
      .default = NA_real_
    ),
    OftenLackEnergy_score = dplyr::case_match(
      OftenLackEnergy,
      "Yes" ~ 0,
      "No" ~ 2,
      "Not sure" ~ 1,
      .default = NA_real_
    ),
    AnxiousDepressed_score = dplyr::case_match(
      AnxiousDepressed,
      "Yes" ~ 0,
      "No" ~ 2,
      "Not sure" ~ 1,
      .default = NA_real_
    ),
    NeedChangeTreatment_score = dplyr::case_match(
      NeedChangeTreatment,
      "Yes" ~ 0,
      "No" ~ 2,
      "Not sure" ~ 1,
      .default = NA_real_
    ),
    control_8 = rowSums(
      dplyr::across(
        c(
          DiseaseControlled_score,
          TreatmentUseful_score,
          MissPlannedActivities_score,
          WakeUpAtNight_score,
          SignificantPain_score,
          OftenLackEnergy_score,
          AnxiousDepressed_score,
          NeedChangeTreatment_score
        )
      ),
      na.rm = FALSE
    )
  )

# Remove individual item scores from the final data frames
control %<>%
  dplyr::select(-tidyselect::ends_with("_score"))

control_followup %<>%
  dplyr::select(-tidyselect::ends_with("_score"))

# Save dataframes
# readr::write_rds(
#   x = control,
#   file = file.path(alex_data, "control.rds")
# )
# 
# readr::write_rds(
#   x = control_followup,
#   file = file.path(alex_data, "control_followup.rds")
# )


