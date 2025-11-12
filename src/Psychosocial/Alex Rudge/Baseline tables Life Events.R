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
data_baseline_table <- data_baseline %>%
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
  )) %>%
  # Reword life events
  dplyr::mutate(
    AnyLifeEvents = forcats::fct_recode(
      AnyLifeEvents,
      "None" = "No",
      "At least one" = "Yes"
    )
  )
  

data_baseline_table %>%
  gtsummary::tbl_strata(
    strata = diagnosis2,
    .tbl_fun = ~
      .x %>%
      gtsummary::tbl_summary(
        by = AnyLifeEvents,
        include = variables,
        missing_text = 'Missing data',
        label = list(
          age ~ "Age",
          flare_group ~ "Flares in previous year",
          FC ~ 'Fecal Calprotectin',
          Smoke ~ "Smoking",
          OverallControl ~ "VAS Control Score"
        )
      ) %>%
      gtsummary::add_p() %>%
      gtsummary::add_q(method = 'holm') %>%
      gtsummary::bold_p(q = TRUE),
    .header = "**{strata}**, N = {n}"
  ) %>%
  gtsummary::as_gt() %>%
  gt::tab_spanner(
    label = gt::md("**Life events in the past month**"),
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
