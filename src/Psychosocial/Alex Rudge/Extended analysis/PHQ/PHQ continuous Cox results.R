source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

# Extracting the results from the continuous PHQ Cox models

variable <- "TotalPHQ"

# Complete case
cox_results_phq_cts_cc <- extract_cox_results_cts(
  data = data_survival_soft_uc,
  cox_model = cox_phq_cts_soft_uc,
  flare_type = 'soft',
  diagnosis2 = 'UC/IBDU',
  variable = variable
) %>%
  dplyr::bind_rows(
    extract_cox_results_cts(
      data = data_survival_soft_cd,
      cox_model = cox_phq_cts_soft_cd,
      flare_type = 'soft',
      diagnosis2 = 'CD',
      variable = variable
    )
  ) %>%
  dplyr::bind_rows(
    extract_cox_results_cts(
      data = data_survival_hard_uc,
      cox_model = cox_phq_cts_hard_uc,
      flare_type = 'hard',
      diagnosis2 = 'UC/IBDU',
      variable = variable
    ) %>%
      dplyr::bind_rows(
        extract_cox_results_cts(
          data = data_survival_hard_cd,
          cox_model = cox_phq_cts_hard_cd,
          flare_type = 'hard',
          diagnosis2 = 'CD',
          variable = variable
        )
      )
  )

# MICE
cox_results_phq_cts_mice <- extract_cox_results_cts(
  data = data_survival_soft_uc,
  cox_model = cox_phq_cts_soft_uc_pool,
  flare_type = 'soft',
  diagnosis2 = 'UC/IBDU',
  variable = variable
) %>%
  dplyr::bind_rows(
    extract_cox_results_cts(
      data = data_survival_soft_cd,
      cox_model = cox_phq_cts_soft_cd_pool,
      flare_type = 'soft',
      diagnosis2 = 'CD',
      variable = variable
    )
  ) %>%
  dplyr::bind_rows(
    extract_cox_results_cts(
      data = data_survival_hard_uc,
      cox_model = cox_phq_cts_hard_uc_pool,
      flare_type = 'hard',
      diagnosis2 = 'UC/IBDU',
      variable = variable
    ) %>%
      dplyr::bind_rows(
        extract_cox_results_cts(
          data = data_survival_hard_cd,
          cox_model = cox_phq_cts_hard_cd_pool,
          flare_type = 'hard',
          diagnosis2 = 'CD',
          variable = variable
        )
      )
  )

# Save
filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Extended analysis/"

readr::write_rds(
  x = cox_results_phq_cts_cc,
  file = paste0(filepath, "cox_results_phq_cts_cc.rds")
)

readr::write_rds(
  x = cox_results_phq_cts_mice,
  file = paste0(filepath, "cox_results_phq_cts_mice.rds")
)
