qmd_code <- tempfile(fileext = ".R")
knitr::purl("/Users/arudge/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Primary analysis/PHQ/PHQ interactions.qmd", output = qmd_code, documentation = 0)
source(qmd_code, local = .GlobalEnv)
unlink(qmd_code)

source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

# Extract somatisation x diagnosis2 interaction terms from the PHQ Cox models.
cox_results_phq_interaction_cc <- dplyr::bind_rows(
  extract_cox_interaction_results(
    cox_model = cox_soft_interaction,
    flare_type = "soft",
    variable = "somatisation",
    diagnosis = "diagnosis2"
  ),
  extract_cox_interaction_results(
    cox_model = cox_hard_interaction,
    flare_type = "hard",
    variable = "somatisation",
    diagnosis = "diagnosis2"
  )
)

cox_results_phq_interaction_mice <- dplyr::bind_rows(
  extract_cox_interaction_results(
    cox_model = cox_soft_interaction_pool,
    flare_type = "soft",
    variable = "somatisation",
    diagnosis = "diagnosis2"
  ),
  extract_cox_interaction_results(
    cox_model = cox_hard_interaction_pool,
    flare_type = "hard",
    variable = "somatisation",
    diagnosis = "diagnosis2"
  )
)

# Extract somatisation x age_decade interaction terms from diagnosis-stratified models.
cox_results_phq_interaction_age_cc <- dplyr::bind_rows(
  extract_cox_interaction_results(
    cox_model = cox_soft_uc_age_interaction,
    flare_type = "soft",
    variable = "somatisation",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(
    cox_model = cox_soft_cd_age_interaction,
    flare_type = "soft",
    variable = "somatisation",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "CD"),
  extract_cox_interaction_results(
    cox_model = cox_hard_uc_age_interaction,
    flare_type = "hard",
    variable = "somatisation",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(
    cox_model = cox_hard_cd_age_interaction,
    flare_type = "hard",
    variable = "somatisation",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "CD")
)

cox_results_phq_interaction_age_mice <- dplyr::bind_rows(
  extract_cox_interaction_results(
    cox_model = cox_soft_uc_age_interaction_pool,
    flare_type = "soft",
    variable = "somatisation",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(
    cox_model = cox_soft_cd_age_interaction_pool,
    flare_type = "soft",
    variable = "somatisation",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "CD"),
  extract_cox_interaction_results(
    cox_model = cox_hard_uc_age_interaction_pool,
    flare_type = "hard",
    variable = "somatisation",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(
    cox_model = cox_hard_cd_age_interaction_pool,
    flare_type = "hard",
    variable = "somatisation",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "CD")
)
