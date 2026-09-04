qmd_code <- tempfile(fileext = ".R")
knitr::purl("/Users/arudge/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Extended analysis/Life Events/Life Events interactions.qmd", output = qmd_code, documentation = 0)
source(qmd_code, local = .GlobalEnv)
unlink(qmd_code)

source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

# Extract AnyLifeEvents x diagnosis2 interaction terms from the extended life-events Cox models.
cox_results_lifeevents_interaction_cc <- dplyr::bind_rows(
  extract_cox_interaction_results(cox_soft_interaction, "soft", "AnyLifeEvents", "diagnosis2"),
  extract_cox_interaction_results(cox_hard_interaction, "hard", "AnyLifeEvents", "diagnosis2")
)

cox_results_lifeevents_interaction_mice <- dplyr::bind_rows(
  extract_cox_interaction_results(cox_soft_interaction_pool, "soft", "AnyLifeEvents", "diagnosis2"),
  extract_cox_interaction_results(cox_hard_interaction_pool, "hard", "AnyLifeEvents", "diagnosis2")
)

# Extract AnyLifeEvents x age_decade interaction terms from diagnosis-stratified models.
cox_results_lifeevents_interaction_age_cc <- dplyr::bind_rows(
  extract_cox_interaction_results(cox_soft_uc_age_interaction, "soft", "AnyLifeEvents", "age_decade") %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(cox_soft_cd_age_interaction, "soft", "AnyLifeEvents", "age_decade") %>% dplyr::mutate(diagnosis2 = "CD"),
  extract_cox_interaction_results(cox_hard_uc_age_interaction, "hard", "AnyLifeEvents", "age_decade") %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(cox_hard_cd_age_interaction, "hard", "AnyLifeEvents", "age_decade") %>% dplyr::mutate(diagnosis2 = "CD")
)

cox_results_lifeevents_interaction_age_mice <- dplyr::bind_rows(
  extract_cox_interaction_results(cox_soft_uc_age_interaction_pool, "soft", "AnyLifeEvents", "age_decade") %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(cox_soft_cd_age_interaction_pool, "soft", "AnyLifeEvents", "age_decade") %>% dplyr::mutate(diagnosis2 = "CD"),
  extract_cox_interaction_results(cox_hard_uc_age_interaction_pool, "hard", "AnyLifeEvents", "age_decade") %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(cox_hard_cd_age_interaction_pool, "hard", "AnyLifeEvents", "age_decade") %>% dplyr::mutate(diagnosis2 = "CD")
)
