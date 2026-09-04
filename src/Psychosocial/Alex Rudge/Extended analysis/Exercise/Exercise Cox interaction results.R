qmd_code <- tempfile(fileext = ".R")
knitr::purl("/Users/arudge/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Extended analysis/Exercise/Exercise interactions.qmd", output = qmd_code, documentation = 0)
source(qmd_code, local = .GlobalEnv)
unlink(qmd_code)

source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

# Extract MinimumExercise x diagnosis2 interaction terms from the extended exercise Cox models.
cox_results_exercise_interaction_cc <- dplyr::bind_rows(
  extract_cox_interaction_results(cox_soft_interaction, "soft", "MinimumExercise", "diagnosis2"),
  extract_cox_interaction_results(cox_hard_interaction, "hard", "MinimumExercise", "diagnosis2")
)

cox_results_exercise_interaction_mice <- dplyr::bind_rows(
  extract_cox_interaction_results(cox_soft_interaction_pool, "soft", "MinimumExercise", "diagnosis2"),
  extract_cox_interaction_results(cox_hard_interaction_pool, "hard", "MinimumExercise", "diagnosis2")
)

# Extract MinimumExercise x age_decade interaction terms from diagnosis-stratified models.
cox_results_exercise_interaction_age_cc <- dplyr::bind_rows(
  extract_cox_interaction_results(cox_soft_uc_age_interaction, "soft", "MinimumExercise", "age_decade") %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(cox_soft_cd_age_interaction, "soft", "MinimumExercise", "age_decade") %>% dplyr::mutate(diagnosis2 = "CD"),
  extract_cox_interaction_results(cox_hard_uc_age_interaction, "hard", "MinimumExercise", "age_decade") %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(cox_hard_cd_age_interaction, "hard", "MinimumExercise", "age_decade") %>% dplyr::mutate(diagnosis2 = "CD")
)

cox_results_exercise_interaction_age_mice <- dplyr::bind_rows(
  extract_cox_interaction_results(cox_soft_uc_age_interaction_pool, "soft", "MinimumExercise", "age_decade") %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(cox_soft_cd_age_interaction_pool, "soft", "MinimumExercise", "age_decade") %>% dplyr::mutate(diagnosis2 = "CD"),
  extract_cox_interaction_results(cox_hard_uc_age_interaction_pool, "hard", "MinimumExercise", "age_decade") %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(cox_hard_cd_age_interaction_pool, "hard", "MinimumExercise", "age_decade") %>% dplyr::mutate(diagnosis2 = "CD")
)
