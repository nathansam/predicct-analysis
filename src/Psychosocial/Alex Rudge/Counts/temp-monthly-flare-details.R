library(tidyverse)
library(magrittr)
library(glue)
library(openxlsx)
library(gtsummary)


monthly_raw <- openxlsx::read.xlsx("/Volumes/igmm/cvallejo-predicct/predicct/final/20221004/Followup/monthlyQ.xlsx")

# Psychosocial
participants <- readr::read_rds("/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/participants.rds")

# Monthly questionnaire - stools

monthly <- monthly_raw %>%
  dplyr::select(
    ParticipantNo, 
    TimepointId, 
    HowCompletedId, 
    DiseaseControlled, 
    GeneralWellbeing, 
    AnyAbdominalPain, 
    LiquidStoolsPerDay,
    MovementsToNormal,
    BloodInStool
  )

# Join
monthly <- participants %>%
  dplyr::inner_join(monthly) %>%
  # Remove incomplete
  dplyr::filter(!is.na(DiseaseControlled))

# Convert liquid stools per day to numeric
monthly <- monthly %>%
  dplyr::mutate(
    LiquidStoolsPerDay = dplyr::if_else(
      LiquidStoolsPerDay == ">20",
      "20",
      LiquidStoolsPerDay
    )
  )

monthly <- monthly %>%
  dplyr::mutate(
    LiquidStoolsPerDay = as.numeric(LiquidStoolsPerDay)
  )

# Plot counts of liquid stools per day
monthly %<>%
  dplyr::mutate(
    LiquidStoolsPerDay = dplyr::case_when(
      LiquidStoolsPerDay == 1 ~ "1",
      LiquidStoolsPerDay == 2 ~ "2",
      LiquidStoolsPerDay == 3 ~ "3",
      LiquidStoolsPerDay == 4 ~ "4",
      LiquidStoolsPerDay == 5 ~ "5",
      LiquidStoolsPerDay >= 6 ~ "6+",
      TRUE ~ NA_character_
    )
  )

# Recode monthly questionnaire responses
monthly <- monthly %>%
  dplyr::mutate(
    DiseaseControlled = factor(
      as.character(DiseaseControlled),
      levels = c("1", "2"),
      labels = c("Yes", "No")
    )
  )

monthly <- monthly %>%
  dplyr::mutate(
    GeneralWellbeing = factor(
      as.character(GeneralWellbeing),
      levels = c("1", "2", "3", "4", "5"),
      labels = c(
        "Very well",
        "Slightly below average",
        "Poor",
        "Very poor",
        "Terrible"
      )
    )
  )

monthly <- monthly %>%
  dplyr::mutate(
    AnyAbdominalPain = factor(
      as.character(AnyAbdominalPain),
      levels = c("1", "2", "3", "4"),
      labels = c("None", "Mild", "Moderate", "Severe")
    )
  )

monthly <- monthly %>%
  dplyr::mutate(
    MovementsToNormal = factor(
      as.character(MovementsToNormal),
      levels = c("1", "2", "3", "4"),
      labels = c(
        "Normal",
        "1-2 more than normal",
        "3-4 more than normal",
        ">4 more than normal"
      )
    )
  )

monthly <- monthly %>%
  dplyr::mutate(
    BloodInStool = factor(
      as.character(BloodInStool),
      levels = c("1", "2", "3", "4"),
      labels = c(
        "None",
        "Less than half the time",
        "More than half the time",
        "Passing blood alone"
      )
    )
  )

# Different questions by UC and CD
monthly_cd <- monthly %>%
  dplyr::filter(diagnosis2 == 'CD') %>%
  dplyr::select(-MovementsToNormal, -BloodInStool)

monthly_uc <- monthly %>%
  dplyr::filter(diagnosis2 == 'UC/IBDU') %>%
  dplyr::select(-GeneralWellbeing, AnyAbdominalPain, LiquidStoolsPerDay)

# Table of monthly flare-related questionnaire responses
cd_variables <- c(
  "GeneralWellbeing",
  "AnyAbdominalPain",
  "LiquidStoolsPerDay"
)

uc_variables <- c(
  "MovementsToNormal",
  "BloodInStool"
)

tbl_monthly_flare_details <- gtsummary::tbl_stack(
  tbls = list(
    monthly_cd %>%
      gtsummary::tbl_summary(
        by = DiseaseControlled,
        include = cd_variables,
        type = list(all_of(cd_variables) ~ "categorical"),
        statistic = all_categorical() ~ "{n} ({p}%)",
        missing = "ifany",
        missing_text = "Missing data",
        label = list(
          GeneralWellbeing ~ "General wellbeing",
          AnyAbdominalPain ~ "Any abdominal pain",
          LiquidStoolsPerDay ~ "Liquid stools per day"
        )
      ) %>%
      gtsummary::modify_header(all_stat_cols() ~ "**{level}**"),
    monthly_uc %>%
      gtsummary::tbl_summary(
        by = DiseaseControlled,
        include = uc_variables,
        type = list(all_of(uc_variables) ~ "categorical"),
        statistic = all_categorical() ~ "{n} ({p}%)",
        missing = "ifany",
        missing_text = "Missing data",
        label = list(
          MovementsToNormal ~ "Bowel movements returned to normal",
          BloodInStool ~ "Blood in stool"
        )
      ) %>%
      gtsummary::modify_header(all_stat_cols() ~ "**{level}**")
  ),
  group_header = c("Crohn's disease", "Ulcerative colitis/IBDU")
) %>%
  gtsummary::modify_header(
    label ~ "**Question**",
    all_stat_cols() ~ "**{level}**"
  ) %>%
  gtsummary::modify_spanning_header(
    all_stat_cols() ~ "**Disease controlled**"
  ) %>%
  gtsummary::modify_caption("**Monthly flare-related questionnaire responses**") %>%
  gtsummary::as_gt() %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_body(
      rows = label %in% c("Crohn's disease", "Ulcerative colitis/IBDU")
    )
  )

tbl_monthly_flare_details
