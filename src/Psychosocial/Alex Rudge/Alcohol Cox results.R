
# Extracting the results from the Cox models

# Run Exercise

variable = "weekly_units"
filename = "cox_results_alcohol.rds"

cox_results <- extract_cox_results(
  data = data_survival_soft_uc,
  cox_model = cox_soft_uc,
  flare_type = 'soft',
  diagnosis2 = 'UC/IBDU',
  variable = variable
) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_soft_cd,
      cox_model = cox_soft_cd,
      flare_type = 'soft',
      diagnosis2 = 'CD',
      variable = variable
    )
  ) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_hard_uc,
      cox_model = cox_hard_uc,
      flare_type = 'hard',
      diagnosis2 = 'UC/IBDU',
      variable = variable
    ) %>%
      dplyr::bind_rows(
        extract_cox_results(
          data = data_survival_hard_cd,
          cox_model = cox_hard_cd,
          flare_type = 'hard',
          diagnosis2 = 'CD',
          variable = variable
        )
      )
  )

# Save
filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/"

readr::write_rds(
  x = cox_results,
  file = paste0(filepath, filename)
)
