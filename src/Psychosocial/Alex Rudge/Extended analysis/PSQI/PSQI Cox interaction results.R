qmd_code <- tempfile(fileext = ".R")
knitr::purl("/Users/arudge/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Extended analysis/PSQI/PSQI interactions.qmd", output = qmd_code, documentation = 0)
source(qmd_code, local = .GlobalEnv)
unlink(qmd_code)

source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

# Extract SleepDisturbance x diagnosis2 interaction terms from the extended PSQI Cox models.
cox_results_psqi_interaction_cc <- dplyr::bind_rows(
  extract_cox_interaction_results(cox_soft_interaction, "soft", "SleepDisturbance", "diagnosis2"),
  extract_cox_interaction_results(cox_hard_interaction, "hard", "SleepDisturbance", "diagnosis2")
)

cox_results_psqi_interaction_mice <- dplyr::bind_rows(
  extract_cox_interaction_results(cox_soft_interaction_pool, "soft", "SleepDisturbance", "diagnosis2"),
  extract_cox_interaction_results(cox_hard_interaction_pool, "hard", "SleepDisturbance", "diagnosis2")
)

# Extract SleepDisturbance x age_decade interaction terms from diagnosis-stratified models.
cox_results_psqi_interaction_age_cc <- dplyr::bind_rows(
  extract_cox_interaction_results(cox_soft_uc_age_interaction, "soft", "SleepDisturbance", "age_decade") %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(cox_soft_cd_age_interaction, "soft", "SleepDisturbance", "age_decade") %>% dplyr::mutate(diagnosis2 = "CD"),
  extract_cox_interaction_results(cox_hard_uc_age_interaction, "hard", "SleepDisturbance", "age_decade") %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(cox_hard_cd_age_interaction, "hard", "SleepDisturbance", "age_decade") %>% dplyr::mutate(diagnosis2 = "CD")
)

cox_results_psqi_interaction_age_mice <- dplyr::bind_rows(
  extract_cox_interaction_results(cox_soft_uc_age_interaction_pool, "soft", "SleepDisturbance", "age_decade") %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(cox_soft_cd_age_interaction_pool, "soft", "SleepDisturbance", "age_decade") %>% dplyr::mutate(diagnosis2 = "CD"),
  extract_cox_interaction_results(cox_hard_uc_age_interaction_pool, "hard", "SleepDisturbance", "age_decade") %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(cox_hard_cd_age_interaction_pool, "hard", "SleepDisturbance", "age_decade") %>% dplyr::mutate(diagnosis2 = "CD")
)
