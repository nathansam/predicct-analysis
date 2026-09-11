library(tidyverse)
library(magrittr)
library(gtsummary)


# Comparing the psychosocial cohort to the entire Predicct cohort

alex_data <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/"
chiara <- "/Volumes/igmm/cvallejo-predicct/people/chiara/"

# Load common participant-level variables
data_cohort <- readr::read_rds(
  file = glue::glue("{alex_data}common_variables.rds")
)

# Psychosocial cohort
participants <- readr::read_rds(
  file = glue::glue("{alex_data}participants.rds")
)

# Flares
flares_soft <- readr::read_rds(
  glue::glue("{chiara}flares_soft.RDS")
)
flares_hard <- readr::read_rds(
  glue::glue("{chiara}flares_hard.RDS")
)


# Data cleaning
# Select psychosocial cohort
data_cohort %<>%
  dplyr::filter(ParticipantNo %in% participants$ParticipantNo)

# Clean IMD labels to match Table 1
data_cohort %<>%
  dplyr::mutate(
    IMD = dplyr::case_match(
      as.character(IMD),
      '1' ~ '1 (most deprived)',
      '2' ~ '2',
      '3' ~ '3',
      '4' ~ '4',
      '5' ~ '5 (least deprived)'
    ),
    IMD = factor(
      IMD,
      levels = c(
        '1 (most deprived)',
        '2',
        '3',
        '4',
        '5 (least deprived)'
      )
    )
  )

# New column - flag if FC is missing
data_cohort %<>%
  dplyr::mutate(
    missing_fc_flag = dplyr::if_else(is.na(FC), "Yes", "No"),
    missing_fc_flag = factor(missing_fc_flag),
    missing_fc_flag = forcats::fct_relevel(missing_fc_flag, "No")
  )


# Number of patients with missing FC
data_cohort %>%
  dplyr::count(missing_fc_flag) %>%
  dplyr::mutate(p = n/sum(n))

# 209 patients (11.4%) with missing FC

# Associations between missing FC and other variables

variables = c(
  'age',
  'Sex',
  'IMD',
  'Smoke',
  'IBD_duration',
  'flare_group',
  'control_8',
  'OverallControl',
  'CReactiveProtein',
  'Biologic'
)

# Using tbl_summary

tbl <- data_cohort %>%
  gtsummary::tbl_strata(
    strata = diagnosis2,
    .tbl_fun = ~
      .x %>%
      gtsummary::tbl_summary(
        by = missing_fc_flag,
        include = variables,
        missing_text = 'Missing data',
        label = list(
          age ~ 'Age (years)',
          Sex ~ 'Sex',
          IMD ~ 'Index of multiple deprivation',
          Smoke ~ 'Smoking status',
          IBD_duration ~ 'IBD duration (years)',
          flare_group ~ 'Flares in past year',
          control_8 ~ 'IBD-Control-8',
          OverallControl ~ 'IBD-Control-VAS',
          CReactiveProtein ~ 'C-reactive protein (mg/L)',
          Biologic ~ 'Biologic use'
        )
      ) %>%
      gtsummary::add_p() %>%
      gtsummary::add_q() %>%
      gtsummary::bold_p(q = TRUE),
    .header = "**{strata}**, N = {n}"
  ) %>%
  gtsummary::as_gt() %>%
  gt::tab_spanner(
    label = gt::md("**Baseline FC missing**"),
    columns = c(stat_1_1, stat_2_1, stat_1_2, stat_2_2),
    level = 2,
    gather = FALSE
  ) %>%
  {tbl <- .
  
  # Swap spanner hierarchy
  tbl$`_spanners` <- tbl$`_spanners` %>%
    dplyr::mutate(spanner_level =
                    dplyr::case_match(
                      spanner_level,
                      1 ~ 2,
                      2 ~ 1))
  
  tbl
  }; tbl

# Save table as Word and HTML
filepath <- "/Users/arudge/Library/CloudStorage/OneDrive-UniversityofEdinburgh/Predicct/Tables/"

tbl %>%
  gt::gtsave(
    filename = paste0(filepath, "Baseline associations missing FC.docx")
  )

tbl %>%
  gt::gtsave(
    filename = paste0(filepath, "Baseline associations missing FC.html")
  )



# Is missingness informative of the outcome?

data_survival_soft <- data_cohort %>%
  dplyr::inner_join(
    flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(DiseaseFlareYN = softflare, time = softflare_time)

data_survival_hard <- data_cohort %>%
  dplyr::inner_join(
    flares_hard %>% dplyr::select(ParticipantNo, hardflare, hardflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(DiseaseFlareYN = hardflare, time = hardflare_time)


# Plotting Kaplan-Meier curves
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# HADS Anxiety ####
legend.title = 'Missing FC'
legend.labs = c('No', 'Yes')
palette = okabe_ito
dependent = 'missing_fc_flag'

# Soft
summon_km_curves(
  data = data_survival_soft,
  dependent = dependent,
  title = "Time to Patient Reported Flare",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

# Hard
summon_km_curves(
  data = data_survival_hard,
  dependent = dependent,
  title = "Time to Objective Flare",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)


# Patients with missing FC have better survival probability

# Split UC and CD
data_survival_soft_uc <- data_survival_soft %>%
  dplyr::filter(diagnosis2 == 'UC/IBDU')

data_survival_soft_cd <- data_survival_soft %>%
  dplyr::filter(diagnosis2 == 'CD')

data_survival_hard_uc <- data_survival_hard %>%
  dplyr::filter(diagnosis2 == 'UC/IBDU')

data_survival_hard_cd <- data_survival_hard %>%
  dplyr::filter(diagnosis2 == 'CD')

# Plot KM
# Soft UC
summon_km_curves(
  data = data_survival_soft_uc,
  dependent = dependent,
  title = "Time to Patient Reported Flare in UC",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

# Soft CD
summon_km_curves(
  data = data_survival_soft_cd,
  dependent = dependent,
  title = "Time to Patient Reported Flare in CD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

# Hard UC
summon_km_curves(
  data = data_survival_hard_uc,
  dependent = dependent,
  title = "Time to Objective Flare in UC",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)

# Hard CD
summon_km_curves(
  data = data_survival_hard_cd,
  dependent = dependent,
  title = "Time to Objective Flare in CD",
  legend.title = legend.title,
  legend.labs = legend.labs,
  palette = palette
)
