library(tidyverse)
library(magrittr)


# Run HADS data to get a dataset with missing FC

data <- hads %>%
  # Select anxiety hads
  dplyr::filter(hads_type == 'anxiety_hads') %>%
  # Select relevant variables
  dplyr::select(
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
  dplyr::mutate(missing_fc_flag = is.na(FC))


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



