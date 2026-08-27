source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

# Extracting the results from the continuous HADS Cox models

# Complete case
cox_results_hads_anxiety_cts_cc <- extract_cox_results_cts(
  data = data_survival_anxiety_soft_uc,
  cox_model = cox_anxiety_cts_soft_uc,
  flare_type = 'soft',
  diagnosis2 = 'UC/IBDU',
  variable = 'hads_score'
) %>%
  dplyr::bind_rows(
    extract_cox_results_cts(
      data = data_survival_anxiety_soft_cd,
      cox_model = cox_anxiety_cts_soft_cd,
      flare_type = 'soft',
      diagnosis2 = 'CD',
      variable = 'hads_score'
    )
  ) %>%
  dplyr::bind_rows(
    extract_cox_results_cts(
      data = data_survival_anxiety_hard_uc,
      cox_model = cox_anxiety_cts_hard_uc,
      flare_type = 'hard',
      diagnosis2 = 'UC/IBDU',
      variable = 'hads_score'
    ) %>%
      dplyr::bind_rows(
        extract_cox_results_cts(
          data = data_survival_anxiety_hard_cd,
          cox_model = cox_anxiety_cts_hard_cd,
          flare_type = 'hard',
          diagnosis2 = 'CD',
          variable = 'hads_score'
        )
      )
  )

cox_results_hads_depression_cts_cc <- extract_cox_results_cts(
  data = data_survival_depression_soft_uc,
  cox_model = cox_depression_cts_soft_uc,
  flare_type = 'soft',
  diagnosis2 = 'UC/IBDU',
  variable = 'hads_score'
) %>%
  dplyr::bind_rows(
    extract_cox_results_cts(
      data = data_survival_depression_soft_cd,
      cox_model = cox_depression_cts_soft_cd,
      flare_type = 'soft',
      diagnosis2 = 'CD',
      variable = 'hads_score'
    )
  ) %>%
  dplyr::bind_rows(
    extract_cox_results_cts(
      data = data_survival_depression_hard_uc,
      cox_model = cox_depression_cts_hard_uc,
      flare_type = 'hard',
      diagnosis2 = 'UC/IBDU',
      variable = 'hads_score'
    ) %>%
      dplyr::bind_rows(
        extract_cox_results_cts(
          data = data_survival_depression_hard_cd,
          cox_model = cox_depression_cts_hard_cd,
          flare_type = 'hard',
          diagnosis2 = 'CD',
          variable = 'hads_score'
        )
      )
  )

# MICE
cox_results_hads_anxiety_cts_mice <- extract_cox_results_cts(
  data = data_survival_anxiety_soft_uc,
  cox_model = cox_anxiety_cts_soft_uc_pool,
  flare_type = 'soft',
  diagnosis2 = 'UC/IBDU',
  variable = 'hads_score'
) %>%
  dplyr::bind_rows(
    extract_cox_results_cts(
      data = data_survival_anxiety_soft_cd,
      cox_model = cox_anxiety_cts_soft_cd_pool,
      flare_type = 'soft',
      diagnosis2 = 'CD',
      variable = 'hads_score'
    )
  ) %>%
  dplyr::bind_rows(
    extract_cox_results_cts(
      data = data_survival_anxiety_hard_uc,
      cox_model = cox_anxiety_cts_hard_uc_pool,
      flare_type = 'hard',
      diagnosis2 = 'UC/IBDU',
      variable = 'hads_score'
    ) %>%
      dplyr::bind_rows(
        extract_cox_results_cts(
          data = data_survival_anxiety_hard_cd,
          cox_model = cox_anxiety_cts_hard_cd_pool,
          flare_type = 'hard',
          diagnosis2 = 'CD',
          variable = 'hads_score'
        )
      )
  )

cox_results_hads_depression_cts_mice <- extract_cox_results_cts(
  data = data_survival_depression_soft_uc,
  cox_model = cox_depression_cts_soft_uc_pool,
  flare_type = 'soft',
  diagnosis2 = 'UC/IBDU',
  variable = 'hads_score'
) %>%
  dplyr::bind_rows(
    extract_cox_results_cts(
      data = data_survival_depression_soft_cd,
      cox_model = cox_depression_cts_soft_cd_pool,
      flare_type = 'soft',
      diagnosis2 = 'CD',
      variable = 'hads_score'
    )
  ) %>%
  dplyr::bind_rows(
    extract_cox_results_cts(
      data = data_survival_depression_hard_uc,
      cox_model = cox_depression_cts_hard_uc_pool,
      flare_type = 'hard',
      diagnosis2 = 'UC/IBDU',
      variable = 'hads_score'
    ) %>%
      dplyr::bind_rows(
        extract_cox_results_cts(
          data = data_survival_depression_hard_cd,
          cox_model = cox_depression_cts_hard_cd_pool,
          flare_type = 'hard',
          diagnosis2 = 'CD',
          variable = 'hads_score'
        )
      )
  )

# Save
filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Extended analysis/"

readr::write_rds(
  x = cox_results_hads_anxiety_cts_cc,
  file = paste0(filepath, "cox_results_hads_anxiety_cts_cc.rds")
)

readr::write_rds(
  x = cox_results_hads_depression_cts_cc,
  file = paste0(filepath, "cox_results_hads_depression_cts_cc.rds")
)

readr::write_rds(
  x = cox_results_hads_anxiety_cts_mice,
  file = paste0(filepath, "cox_results_hads_anxiety_cts_mice.rds")
)

readr::write_rds(
  x = cox_results_hads_depression_cts_mice,
  file = paste0(filepath, "cox_results_hads_depression_cts_mice.rds")
)
