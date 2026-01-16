library(gt)
library(gtsummary)

# Variable to include
variables = c('age',
              'Sex',
              'Smoke',
              'IMD',
              'FC',
              'flare_group',
              'OverallControl',
              'control_8')

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
      "None" = "No Flares",
      "At least one" = "1 or More Flares"
  )) %>%
  # Reword life events
  dplyr::mutate(
    AnyLifeEvents = forcats::fct_recode(
      AnyLifeEvents,
      "None" = "No",
      "At least one" = "Yes"
    )
  ) %>%
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
  

tbl <- data_baseline_table %>%
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
          Sex ~ 'Sex',
          Smoke ~ "Smoking",
          IMD ~ 'Index of Multiple Deprivation',
          FC ~ 'Fecal Calprotectin',
          flare_group ~ "Flares in previous year",
          OverallControl ~ "IBD-Control-VAS score",
          control_8 ~ "IBD-Control-8 score"
        )
      ) %>%
      gtsummary::add_p(
        test.args = all_tests("fisher.test") ~ list(simulate.p.value = TRUE, B = 1e5)
      ) %>%
      gtsummary::add_q(method = 'fdr') %>%
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


tbl



# Save to word
filepath <- "/Users/arudge/Library/CloudStorage/OneDrive-UniversityofEdinburgh/Predicct/Tables/"

# Anxiety
tbl %>%
  gt::gtsave(
    filename = paste0(filepath, "Baseline associations life events.docx")
  )
