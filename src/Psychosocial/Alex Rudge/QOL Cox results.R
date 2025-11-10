
# Extracting the results from the Cox models

# Run QOL

# PCS12_binary
# Extract Cox results
cox_results_physical <- extract_cox_results(
  data = data_survival_soft_uc,
  cox_model = cox_physical_soft_uc,
  flare_type = 'soft',
  diagnosis2 = 'UC/IBDU',
  variable = 'PCS12_binary'
) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_soft_cd,
      cox_model = cox_physical_soft_cd,
      flare_type = 'soft',
      diagnosis2 = 'CD',
      variable = 'PCS12_binary'
    )
  ) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_hard_uc,
      cox_model = cox_physical_hard_uc,
      flare_type = 'hard',
      diagnosis2 = 'UC/IBDU',
      variable = 'PCS12_binary'
    ) %>%
      dplyr::bind_rows(
        extract_cox_results(
          data = data_survival_hard_cd,
          cox_model = cox_physical_hard_cd,
          flare_type = 'hard',
          diagnosis2 = 'CD',
          variable = 'PCS12_binary'
        )
      )
  )

# Save
filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/"

# readr::write_rds(
#   x = cox_results_physical,
#   file = paste0(filepath, "cox_results_pcs12binary_cc.rds")
# )

# readr::write_rds(
#   x = cox_results_physical,
#   file = paste0(filepath, "cox_results_pcs12binary_mice.rds")
# )

# MCS12_binary
cox_results_mental <- extract_cox_results(
  data = data_survival_soft_uc,
  cox_model = cox_mental_soft_uc,
  flare_type = 'soft',
  diagnosis2 = 'UC/IBDU',
  variable = 'MCS12_binary'
) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_soft_cd,
      cox_model = cox_mental_soft_cd,
      flare_type = 'soft',
      diagnosis2 = 'CD',
      variable = 'MCS12_binary'
    )
  ) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_hard_uc,
      cox_model = cox_mental_hard_uc,
      flare_type = 'hard',
      diagnosis2 = 'UC/IBDU',
      variable = 'MCS12_binary'
    ) %>%
      dplyr::bind_rows(
        extract_cox_results(
          data = data_survival_hard_cd,
          cox_model = cox_mental_hard_cd,
          flare_type = 'hard',
          diagnosis2 = 'CD',
          variable = 'MCS12_binary'
        )
      )
  )

# Save
filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/"

# readr::write_rds(
#   x = cox_results_mental,
#   file = paste0(filepath, "cox_results_mcs12binary_cc.rds")
# )

# readr::write_rds(
#   x = cox_results_mental,
#   file = paste0(filepath, "cox_results_mcs12binary_mice.rds")
# )


# Continuous variable

# PCS12_decade
# Extract Cox results
cox_results_physical <- extract_cox_results(
  data = data_survival_soft_uc,
  cox_model = cox_physical_soft_uc,
  flare_type = 'soft',
  diagnosis2 = 'UC/IBDU',
  variable = 'PCS12_decade'
) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_soft_cd,
      cox_model = cox_physical_soft_cd,
      flare_type = 'soft',
      diagnosis2 = 'CD',
      variable = 'PCS12_decade'
    )
  ) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_hard_uc,
      cox_model = cox_physical_hard_uc,
      flare_type = 'hard',
      diagnosis2 = 'UC/IBDU',
      variable = 'PCS12_decade'
    ) %>%
      dplyr::bind_rows(
        extract_cox_results(
          data = data_survival_hard_cd,
          cox_model = cox_physical_hard_cd,
          flare_type = 'hard',
          diagnosis2 = 'CD',
          variable = 'PCS12_decade'
        )
      )
  )

# Save
filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/"

# readr::write_rds(
#   x = cox_results_physical,
#   file = paste0(filepath, "cox_results_pcs12continuous_cc.rds")
# )

# readr::write_rds(
#   x = cox_results_physical,
#   file = paste0(filepath, "cox_results_pcs12continuous_mice.rds")
# )

# MCS12_decade
cox_results_mental <- extract_cox_results(
  data = data_survival_soft_uc,
  cox_model = cox_mental_soft_uc,
  flare_type = 'soft',
  diagnosis2 = 'UC/IBDU',
  variable = 'MCS12_decade'
) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_soft_cd,
      cox_model = cox_mental_soft_cd,
      flare_type = 'soft',
      diagnosis2 = 'CD',
      variable = 'MCS12_decade'
    )
  ) %>%
  dplyr::bind_rows(
    extract_cox_results(
      data = data_survival_hard_uc,
      cox_model = cox_mental_hard_uc,
      flare_type = 'hard',
      diagnosis2 = 'UC/IBDU',
      variable = 'MCS12_decade'
    ) %>%
      dplyr::bind_rows(
        extract_cox_results(
          data = data_survival_hard_cd,
          cox_model = cox_mental_hard_cd,
          flare_type = 'hard',
          diagnosis2 = 'CD',
          variable = 'MCS12_decade'
        )
      )
  )

# Save
filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/"

# readr::write_rds(
#   x = cox_results_mental,
#   file = paste0(filepath, "cox_results_mcs12continuous_cc.rds")
# )

# readr::write_rds(
#   x = cox_results_mental,
#   file = paste0(filepath, "cox_results_mcs12continuous_mice.rds")
# )
