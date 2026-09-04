qmd_code <- tempfile(fileext = ".R")
knitr::purl("/Users/arudge/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Primary analysis/HADS/HADS interactions.qmd", output = qmd_code, documentation = 0)
source(qmd_code, local = .GlobalEnv)
unlink(qmd_code)

source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

# Extract the score_group x age_decade interaction terms from the stratified
# HADS Cox models. The diagnosis label is added because UC and CD are modelled
# separately.

# Complete case
cox_results_hads_anxiety_interaction_age_cc <- dplyr::bind_rows(
  extract_cox_interaction_results(
    cox_model = cox_anxiety_soft_uc_age_interaction,
    flare_type = "soft",
    variable = "score_group",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(
    cox_model = cox_anxiety_soft_cd_age_interaction,
    flare_type = "soft",
    variable = "score_group",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "CD"),
  extract_cox_interaction_results(
    cox_model = cox_anxiety_hard_uc_age_interaction,
    flare_type = "hard",
    variable = "score_group",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(
    cox_model = cox_anxiety_hard_cd_age_interaction,
    flare_type = "hard",
    variable = "score_group",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "CD")
)

cox_results_hads_depression_interaction_age_cc <- dplyr::bind_rows(
  extract_cox_interaction_results(
    cox_model = cox_depression_soft_uc_age_interaction,
    flare_type = "soft",
    variable = "score_group",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(
    cox_model = cox_depression_soft_cd_age_interaction,
    flare_type = "soft",
    variable = "score_group",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "CD"),
  extract_cox_interaction_results(
    cox_model = cox_depression_hard_uc_age_interaction,
    flare_type = "hard",
    variable = "score_group",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(
    cox_model = cox_depression_hard_cd_age_interaction,
    flare_type = "hard",
    variable = "score_group",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "CD")
)

# MICE
cox_results_hads_anxiety_interaction_age_mice <- dplyr::bind_rows(
  extract_cox_interaction_results(
    cox_model = cox_anxiety_soft_uc_age_interaction_pool,
    flare_type = "soft",
    variable = "score_group",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(
    cox_model = cox_anxiety_soft_cd_age_interaction_pool,
    flare_type = "soft",
    variable = "score_group",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "CD"),
  extract_cox_interaction_results(
    cox_model = cox_anxiety_hard_uc_age_interaction_pool,
    flare_type = "hard",
    variable = "score_group",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(
    cox_model = cox_anxiety_hard_cd_age_interaction_pool,
    flare_type = "hard",
    variable = "score_group",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "CD")
)

cox_results_hads_depression_interaction_age_mice <- dplyr::bind_rows(
  extract_cox_interaction_results(
    cox_model = cox_depression_soft_uc_age_interaction_pool,
    flare_type = "soft",
    variable = "score_group",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(
    cox_model = cox_depression_soft_cd_age_interaction_pool,
    flare_type = "soft",
    variable = "score_group",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "CD"),
  extract_cox_interaction_results(
    cox_model = cox_depression_hard_uc_age_interaction_pool,
    flare_type = "hard",
    variable = "score_group",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(
    cox_model = cox_depression_hard_cd_age_interaction_pool,
    flare_type = "hard",
    variable = "score_group",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "CD")
)
