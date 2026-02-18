
# Extracting the results from the Cox models

# Run HADS

variable = 'score_group'

# Anxiety
cox_results_hads_anxiety_cc <- extract_cox_results(
  data = data_survival_anxiety_soft,
  cox_model = cox_anxiety_soft,
  flare_type = 'soft',
  diagnosis2 = NULL,
  variable = variable
) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_anxiety_hard,
      cox_model = cox_anxiety_hard,
      flare_type = 'hard',
      diagnosis2 = NULL,
      variable = variable
    )
  )

# Depression
cox_results_hads_depression_cc <- extract_cox_results(
  data = data_survival_depression_soft,
  cox_model = cox_depression_soft,
  flare_type = 'soft',
  diagnosis2 = NULL,
  variable = variable
) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_depression_hard,
      cox_model = cox_depression_hard,
      flare_type = 'hard',
      diagnosis2 = NULL,
      variable = variable
    )
  )

# MICE
cox_results_hads_anxiety_mice <- extract_cox_results(
  data = data_survival_anxiety_soft,
  cox_model = cox_anxiety_soft_pool,
  flare_type = 'soft',
  diagnosis2 = NULL,
  variable = variable
) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_anxiety_hard,
      cox_model = cox_anxiety_hard_pool,
      flare_type = 'hard',
      diagnosis2 = NULL,
      variable = variable
    )
  )

cox_results_hads_depression_mice <- extract_cox_results(
  data = data_survival_depression_soft,
  cox_model = cox_depression_soft_pool,
  flare_type = 'soft',
  diagnosis2 = NULL,
  variable = variable
) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_depression_hard,
      cox_model = cox_depression_hard_pool,
      flare_type = 'hard',
      diagnosis2 = NULL,
      variable = variable
    )
  )

# Save
filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Full cohort/"

readr::write_rds(
  x = cox_results_hads_anxiety_cc,
  file = paste0(filepath, "cox_results_hads_anxiety_cc.rds")
)

readr::write_rds(
  x = cox_results_hads_depression_cc,
  file = paste0(filepath, "cox_results_hads_depression_cc.rds")
)

readr::write_rds(
  x = cox_results_hads_anxiety_mice,
  file = paste0(filepath, "cox_results_hads_anxiety_mice.rds")
)

readr::write_rds(
  x = cox_results_hads_depression_mice,
  file = paste0(filepath, "cox_results_hads_depression_mice.rds")
)
