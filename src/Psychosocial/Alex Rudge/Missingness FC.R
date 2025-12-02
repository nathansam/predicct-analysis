library(tidyverse)
library(magrittr)
library(survival)


# Run HADS data to get a dataset with missing FC

data <- hads %>%
  # Select anxiety hads
  dplyr::filter(hads_type == 'anxiety_hads') %>%
  # Select relevant variables
  dplyr::select(
    ParticipantNo,
    diagnosis2,
    age,
    Sex,
    IMD,
    FC,
    flare_group,
    OverallControl,
    Smoke,
    SiteNo,
    score_group
  )

# New column - flag if FC is missing
data %<>%
  dplyr::mutate(missing_fc_flag = is.na(FC)) %>%
  dplyr::mutate(missing_fc_flag = factor(missing_fc_flag)) %>%
  dplyr::mutate(missing_fc_flag = forcats::fct_relevel(missing_fc_flag, "FALSE"))


# Number of patients with missing FC
data %>%
  dplyr::count(missing_fc_flag) %>%
  dplyr::mutate(p = n/sum(n))

# 209 patients (11.4%) with missing FC

# Associations between missing FC and other variables

variables = c('age',
              'Sex',
              'flare_group',
              'Smoke',
              'OverallControl')

# Using tbl_summary

data %>%
  gtsummary::tbl_strata(
    strata = diagnosis2,
    .tbl_fun = ~
      .x %>%
      gtsummary::tbl_summary(
        by = missing_fc_flag,
        include = variables,
        missing_text = 'Missing data',
        label = list(
          age ~ "Age",
          flare_group ~ "Flares in previous year",
          Smoke ~ "Smoking",
          OverallControl ~ "VAS Control Score"
        )
      ) %>%
      gtsummary::add_p() %>%
      gtsummary::bold_p(),
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

data_survival_soft <- data %>%
  dplyr::inner_join(
    flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(DiseaseFlareYN = softflare, time = softflare_time)

data_survival_hard <- data %>%
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
