
# Extracting the results from the Cox models

# Run HADS

# Anxiety
# Extract Cox results
cox_results_hads_anxiety_cc <- extract_cox_results(
  data = data_survival_anxiety_soft_uc,
  cox_model = cox_anxiety_soft_uc,
  flare_type = 'soft',
  diagnosis2 = 'UC/IBDU',
  variable = 'score_group'
) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_anxiety_soft_cd,
      cox_model = cox_anxiety_soft_cd,
      flare_type = 'soft',
      diagnosis2 = 'CD',
      variable = 'score_group'
    )
  ) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_anxiety_hard_uc,
      cox_model = cox_anxiety_hard_uc,
      flare_type = 'hard',
      diagnosis2 = 'UC/IBDU',
      variable = 'score_group'
    ) %>%
      dplyr::bind_rows(
        extract_cox_results(
          data = data_survival_anxiety_hard_cd,
          cox_model = cox_anxiety_hard_cd,
          flare_type = 'hard',
          diagnosis2 = 'CD',
          variable = 'score_group'
        )
      )
  )


# Depression
cox_results_hads_depression_cc <- extract_cox_results(
  data = data_survival_depression_soft_uc,
  cox_model = cox_depression_soft_uc,
  flare_type = 'soft',
  diagnosis2 = 'UC/IBDU',
  variable = 'score_group'
) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_depression_soft_cd,
      cox_model = cox_depression_soft_cd,
      flare_type = 'soft',
      diagnosis2 = 'CD',
      variable = 'score_group'
    )
  ) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_depression_hard_uc,
      cox_model = cox_depression_hard_uc,
      flare_type = 'hard',
      diagnosis2 = 'UC/IBDU',
      variable = 'score_group'
    ) %>%
      dplyr::bind_rows(
        extract_cox_results(
          data = data_survival_depression_hard_cd,
          cox_model = cox_depression_hard_cd,
          flare_type = 'hard',
          diagnosis2 = 'CD',
          variable = 'score_group'
        )
      )
  )

# MICE

# Anxiety
# Extract Cox results
cox_results_hads_anxiety_mice <- extract_cox_results(
  data = data_survival_anxiety_soft_uc,
  cox_model = cox_anxiety_soft_uc_pool,
  flare_type = 'soft',
  diagnosis2 = 'UC/IBDU',
  variable = 'score_group'
) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_anxiety_soft_cd,
      cox_model = cox_anxiety_soft_cd_pool,
      flare_type = 'soft',
      diagnosis2 = 'CD',
      variable = 'score_group'
    )
  ) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_anxiety_hard_uc,
      cox_model = cox_anxiety_hard_uc_pool,
      flare_type = 'hard',
      diagnosis2 = 'UC/IBDU',
      variable = 'score_group'
    ) %>%
      dplyr::bind_rows(
        extract_cox_results(
          data = data_survival_anxiety_hard_cd,
          cox_model = cox_anxiety_hard_cd_pool,
          flare_type = 'hard',
          diagnosis2 = 'CD',
          variable = 'score_group'
        )
      )
  )

# Depression
cox_results_hads_depression_mice <- extract_cox_results(
  data = data_survival_depression_soft_uc,
  cox_model = cox_depression_soft_uc_pool,
  flare_type = 'soft',
  diagnosis2 = 'UC/IBDU',
  variable = 'score_group'
) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_depression_soft_cd,
      cox_model = cox_depression_soft_cd_pool,
      flare_type = 'soft',
      diagnosis2 = 'CD',
      variable = 'score_group'
    )
  ) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_depression_hard_uc,
      cox_model = cox_depression_hard_uc_pool,
      flare_type = 'hard',
      diagnosis2 = 'UC/IBDU',
      variable = 'score_group'
    ) %>%
      dplyr::bind_rows(
        extract_cox_results(
          data = data_survival_depression_hard_cd,
          cox_model = cox_depression_hard_cd_pool,
          flare_type = 'hard',
          diagnosis2 = 'CD',
          variable = 'score_group'
        )
      )
  )

# Save
filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Fully adjusted/"

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
