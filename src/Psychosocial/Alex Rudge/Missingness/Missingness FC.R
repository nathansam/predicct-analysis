library(tidyverse)
library(magrittr)
library(survival)

# Load psychosocial cohort
cohort <- readr::read_rds("/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/participants.rds")

# Load other variables
data_cohort_raw <- readr::read_rds(
  file = "/Volumes/igmm/cvallejo-predicct/predicct/processed/demo-full.rds"
)

# Flares
flares_soft <- readRDS("/Volumes/igmm/cvallejo-predicct/people/chiara/flares_soft.RDS")
flares_hard <- readRDS("/Volumes/igmm/cvallejo-predicct/people/chiara/flares_hard.RDS")

# Select psychosocial cohort
data_cohort <- data_cohort_raw %>%
  dplyr::filter(ParticipantNo %in% cohort$ParticipantNo)


# Select columns
data_cohort %<>%
  dplyr::select(
    ParticipantNo,
    SiteNo,
    diagnosis,
    diagnosis2,
    Sex,
    Age,
    Ethnicity,
    BMIcat,
    IMD,
    `IBD Duration`,
    Treatment,
    Biologic,
    Smoke,
    FC,
    CReactiveProtein,
    control_8,
    vas_control
  )

# Rename cols
data_cohort %<>%
  dplyr::rename(
    IBD_duration = `IBD Duration`,
    OverallControl = vas_control
  )


# Tidy up variables
# IBD
data_cohort %<>%
  dplyr::mutate(IMD = as.character(IMD)) %>%
  dplyr::mutate(
    IMD = dplyr::case_match(
      IMD,
      '1' ~ '1 (most deprived)',
      '2' ~ '2',
      '3' ~ '3',
      '4' ~ '4',
      '5' ~ '5 (least deprived)'
    )
  )


# New column - flag if FC is missing
data_cohort %<>%
  dplyr::mutate(missing_fc_flag = is.na(FC)) %>%
  dplyr::mutate(missing_fc_flag = factor(missing_fc_flag)) %>%
  dplyr::mutate(missing_fc_flag = forcats::fct_relevel(missing_fc_flag, "FALSE"))


# Number of patients with missing FC
data_cohort %>%
  dplyr::count(missing_fc_flag) %>%
  dplyr::mutate(p = n/sum(n))

# 209 patients (11.4%) with missing FC

# Associations between missing FC and other variables

variables = c(
  'Age',
  'Sex',
  'BMIcat',
  'Smoke',
  'IMD',
  'Ethnicity',
  'IBD_duration',
  'control_8',
  'OverallControl',
  'CReactiveProtein',
  'Biologic'
)

# Using tbl_summary

data_cohort %>%
  gtsummary::tbl_strata(
    strata = diagnosis2,
    .tbl_fun = ~
      .x %>%
      gtsummary::tbl_summary(
        by = missing_fc_flag,
        include = variables,
        missing_text = 'Missing data',
        label = list(
          Age ~ "Age",
          Smoke ~ "Smoking",
          OverallControl ~ "VAS Control Score"
        )
      ) %>%
      gtsummary::add_p() %>%
      gtsummary::bold_p() %>%
      gtsummary::add_q(),
    .header = "**{strata}**, N = {n}"
  ) %>%
  gtsummary::as_gt() %>%
  gt::tab_spanner(
    label = gt::md("**Missing FC**"),
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
  }



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
