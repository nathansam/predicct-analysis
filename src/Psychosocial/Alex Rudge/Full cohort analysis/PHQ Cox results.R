
# Extracting the results from the Cox models

# Run phq

variable = "somatisation"

# Extract Cox results
cox_results_phq_cc <- extract_cox_results(
  data = data_survival_soft,
  cox_model = cox_soft,
  flare_type = 'soft',
  diagnosis2 = NULL,
  variable = variable
) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_hard,
      cox_model = cox_hard,
      flare_type = 'hard',
      diagnosis2 = NULL,
      variable = variable
    )
  )


cox_results_phq_mice <- extract_cox_results(
  data = data_survival_soft,
  cox_model = cox_soft_pool,
  flare_type = 'soft',
  diagnosis2 = NULL,
  variable = variable
) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_hard,
      cox_model = cox_hard_pool,
      flare_type = 'hard',
      diagnosis2 = NULL,
      variable = variable
    )
  )

# Save
filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Full cohort/"

readr::write_rds(
  x = cox_results_phq_cc,
  file = paste0(filepath, "cox_results_phq_cc.rds")
)

readr::write_rds(
  x = cox_results_phq_mice,
  file = paste0(filepath, "cox_results_phq_mice.rds")
)
