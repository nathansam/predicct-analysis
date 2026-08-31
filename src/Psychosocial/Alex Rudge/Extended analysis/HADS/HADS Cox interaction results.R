source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

# Extract the score_group x diagnosis2 interaction terms from the HADS Cox models.
# The HADS type is represented by the object/file name; flare_type is added by the
# extraction function.

# Complete case
cox_results_hads_anxiety_interaction_cc <- dplyr::bind_rows(
  extract_cox_interaction_results(
    cox_model = cox_anxiety_soft_interaction,
    flare_type = "soft",
    variable = "score_group",
    diagnosis = "diagnosis2"
  ),
  extract_cox_interaction_results(
    cox_model = cox_anxiety_hard_interaction,
    flare_type = "hard",
    variable = "score_group",
    diagnosis = "diagnosis2"
  )
)

cox_results_hads_depression_interaction_cc <- dplyr::bind_rows(
  extract_cox_interaction_results(
    cox_model = cox_depression_soft_interaction,
    flare_type = "soft",
    variable = "score_group",
    diagnosis = "diagnosis2"
  ),
  extract_cox_interaction_results(
    cox_model = cox_depression_hard_interaction,
    flare_type = "hard",
    variable = "score_group",
    diagnosis = "diagnosis2"
  )
)

# MICE
cox_results_hads_anxiety_interaction_mice <- dplyr::bind_rows(
  extract_cox_interaction_results(
    cox_model = cox_anxiety_soft_interaction_pool,
    flare_type = "soft",
    variable = "score_group",
    diagnosis = "diagnosis2"
  ),
  extract_cox_interaction_results(
    cox_model = cox_anxiety_hard_interaction_pool,
    flare_type = "hard",
    variable = "score_group",
    diagnosis = "diagnosis2"
  )
)

cox_results_hads_depression_interaction_mice <- dplyr::bind_rows(
  extract_cox_interaction_results(
    cox_model = cox_depression_soft_interaction_pool,
    flare_type = "soft",
    variable = "score_group",
    diagnosis = "diagnosis2"
  ),
  extract_cox_interaction_results(
    cox_model = cox_depression_hard_interaction_pool,
    flare_type = "hard",
    variable = "score_group",
    diagnosis = "diagnosis2"
  )
)

# Save
filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Extended analysis/"

readr::write_rds(
  x = cox_results_hads_anxiety_interaction_cc,
  file = paste0(filepath, "cox_results_hads_anxiety_interaction_cc.rds")
)

readr::write_rds(
  x = cox_results_hads_depression_interaction_cc,
  file = paste0(filepath, "cox_results_hads_depression_interaction_cc.rds")
)

readr::write_rds(
  x = cox_results_hads_anxiety_interaction_mice,
  file = paste0(filepath, "cox_results_hads_anxiety_interaction_mice.rds")
)

readr::write_rds(
  x = cox_results_hads_depression_interaction_mice,
  file = paste0(filepath, "cox_results_hads_depression_interaction_mice.rds")
)
