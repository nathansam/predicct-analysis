
# Extracting the results from the Cox models

# Run Exercise
source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")
source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/Exercise/tdc model uc.R")
source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/Exercise/tdc model cd.R")


# Anxiety
# Extract Cox results
cox_results_exercise_cc <- extract_tdc_results(
  data = data_soft_merged_uc,
  cox_model = cox_soft_uc,
  flare_type = 'soft',
  diagnosis2 = 'UC/IBDU',
  variable = 'MinimumExercise'
) %>%
  dplyr::bind_rows(
    extract_tdc_results(
      data = data_soft_merged_cd,
      cox_model = cox_soft_cd,
      flare_type = 'soft',
      diagnosis2 = 'CD',
      variable = 'MinimumExercise'
    )
  ) %>%
  dplyr::bind_rows(
    extract_tdc_results(
      data = data_hard_merged_uc,
      cox_model = cox_hard_uc,
      flare_type = 'hard',
      diagnosis2 = 'UC/IBDU',
      variable = 'MinimumExercise'
    ) %>%
      dplyr::bind_rows(
        extract_tdc_results(
          data = data_hard_merged_cd,
          cox_model = cox_hard_cd,
          flare_type = 'hard',
          diagnosis2 = 'CD',
          variable = 'MinimumExercise'
        )
      )
  )



# Save
filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Longitudinal analysis/"

readr::write_rds(
  x = cox_results_exercise_cc,
  file = paste0(filepath, "cox_results_exercise_cc.rds")
)


