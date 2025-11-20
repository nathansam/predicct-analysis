
# Extracting the results from the Cox models

# Run phq

source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

variable = "somatisation"

# Extract Cox results
cox_results_phq_cc <- extract_cox_results(
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

cox_results_phq_mice <- extract_cox_results(
  data = data_survival_soft_uc,
  cox_model = cox_soft_uc_pool,
  flare_type = 'soft',
  diagnosis2 = 'UC/IBDU',
  variable = variable
) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_soft_cd,
      cox_model = cox_soft_cd_pool,
      flare_type = 'soft',
      diagnosis2 = 'CD',
      variable = variable
    )
  ) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_hard_uc,
      cox_model = cox_hard_uc_pool,
      flare_type = 'hard',
      diagnosis2 = 'UC/IBDU',
      variable = variable
    ) %>%
      dplyr::bind_rows(
        extract_cox_results(
          data = data_survival_hard_cd,
          cox_model = cox_hard_cd_pool,
          flare_type = 'hard',
          diagnosis2 = 'CD',
          variable = variable
        )
      )
  )

# Save
filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Cox results grouped/"

readr::write_rds(
  x = cox_results_phq_cc,
  file = paste0(filepath, "cox_results_phq_cc.rds")
)

readr::write_rds(
  x = cox_results_phq_mice,
  file = paste0(filepath, "cox_results_phq_mice.rds")
)
