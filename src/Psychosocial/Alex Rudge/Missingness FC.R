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



# Is missingness informative of the outcome?

data_survival <- data %>%
  dplyr::inner_join(
    flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(status = softflare, time = softflare_time)


data_survival %>%
  survfit(Surv(time, status) ~ missing_fc_flag, data = .) %>%
  ggsurvplot(
    ., 
    data = data_survival, 
    conf.int = TRUE, 
    risk.table = TRUE,
    pval = TRUE,
    pval.method = TRUE,
    legend.title = 'Missing FC',
    xlab = "Time from study recruitment (days)",
    ggtheme = theme_minimal())

# Patients with missing FC have better survival probability



