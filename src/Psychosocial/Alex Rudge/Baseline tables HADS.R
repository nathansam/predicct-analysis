library(gt)
library(gtsummary)

# Variable to include
variables = c('age',
              'Sex',
              'flare_group',
              'FC',
              'Smoke',
              'OverallControl')

# Tidy up the data ready for the table

data_baseline_anxiety_table <- data_baseline_anxiety %>%
  dplyr::mutate(
    diagnosis2 = dplyr::case_match(
      diagnosis2,
      "CD" ~ "Crohn's Disease",
      "UC/IBDU" ~ 'Ulcerative Colitis'
    ) 
  ) %>%
  # Unlog FC
  dplyr::mutate(FC = exp(FC)) %>%
  # Reword Flares
  dplyr::mutate(
    flare_group = forcats::fct_recode(
      flare_group,
      "None" = "No flares",
      "At least one" = "1 or More Flares"
  ))


data_baseline_anxiety_table %>%
  gtsummary::tbl_strata(
    strata = diagnosis2,
    .tbl_fun = ~
      .x %>%
      gtsummary::tbl_summary(
        by = 'score_group',
        include = independent,
        missing_text = 'Missing data',
        label = list(
          age ~ "Age",
          flare_group ~ "Flares in previous year",
          FC ~ 'Fecal Calprotectin',
          Smoke ~ "Smoker",
          OverallControl ~ "VAS Control Score"
        )
      ) %>%
      gtsummary::add_p() %>%
      gtsummary::add_q(method = 'holm') %>%
      gtsummary::modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**HADS Anxiety Score**"),
    .header = "**{strata}**, N = {n}"
  )
