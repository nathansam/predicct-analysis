
# Extracting the results from the Cox models

# Run HADS
source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/HADS/tdc model uc.R")
source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/HADS/tdc model cd.R")


# Anxiety
# Extract Cox results
cox_results_hads_anxiety_cc <- extract_tdc_results(
  data = data_anxiety_soft_merged_uc,
  cox_model = cox_anxiety_soft_uc,
  flare_type = 'soft',
  diagnosis2 = 'UC/IBDU',
  variable = 'score_group'
) %>%
  dplyr::bind_rows(
    extract_tdc_results(
      data = data_anxiety_soft_merged_cd,
      cox_model = cox_anxiety_soft_cd,
      flare_type = 'soft',
      diagnosis2 = 'CD',
      variable = 'score_group'
    )
  ) %>%
  dplyr::bind_rows(
    extract_tdc_results(
      data = data_anxiety_hard_merged_uc,
      cox_model = cox_anxiety_hard_uc,
      flare_type = 'hard',
      diagnosis2 = 'UC/IBDU',
      variable = 'score_group'
    ) %>%
      dplyr::bind_rows(
        extract_tdc_results(
          data = data_anxiety_hard_merged_cd,
          cox_model = cox_anxiety_hard_cd,
          flare_type = 'hard',
          diagnosis2 = 'CD',
          variable = 'score_group'
        )
      )
  )


# Depression
cox_results_hads_depression_cc <- extract_tdc_results(
  data = data_depression_soft_merged_uc,
  cox_model = cox_depression_soft_uc,
  flare_type = 'soft',
  diagnosis2 = 'UC/IBDU',
  variable = 'score_group'
) %>%
  dplyr::bind_rows(
    extract_tdc_results(
      data = data_depression_soft_merged_cd,
      cox_model = cox_depression_soft_cd,
      flare_type = 'soft',
      diagnosis2 = 'CD',
      variable = 'score_group'
    )
  ) %>%
  dplyr::bind_rows(
    extract_tdc_results(
      data = data_depression_hard_merged_uc,
      cox_model = cox_depression_hard_uc,
      flare_type = 'hard',
      diagnosis2 = 'UC/IBDU',
      variable = 'score_group'
    ) %>%
      dplyr::bind_rows(
        extract_tdc_results(
          data = data_depression_hard_merged_cd,
          cox_model = cox_depression_hard_cd,
          flare_type = 'hard',
          diagnosis2 = 'CD',
          variable = 'score_group'
        )
      )
  )


# Save
filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Longitudinal analysis/"

readr::write_rds(
  x = cox_results_hads_anxiety_cc,
  file = paste0(filepath, "cox_results_hads_anxiety_cc.rds")
)

readr::write_rds(
  x = cox_results_hads_depression_cc,
  file = paste0(filepath, "cox_results_hads_depression_cc.rds")
)

