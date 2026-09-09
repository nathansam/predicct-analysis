qmd_code <- tempfile(fileext = ".R")
knitr::purl("/Users/arudge/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Primary analysis/Fatigue/Fatigue interactions.qmd", output = qmd_code, documentation = 0)
source(qmd_code, local = .GlobalEnv)
unlink(qmd_code)

source("~/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/functions.R")

# Extract OftenLackEnergy x diagnosis2 interaction terms from the fatigue Cox models.
cox_results_fatigue_interaction_cc <- dplyr::bind_rows(
  extract_cox_interaction_results(
    cox_model = cox_soft_interaction,
    flare_type = "soft",
    variable = "OftenLackEnergy",
    diagnosis = "diagnosis2"
  ),
  extract_cox_interaction_results(
    cox_model = cox_hard_interaction,
    flare_type = "hard",
    variable = "OftenLackEnergy",
    diagnosis = "diagnosis2"
  )
)

cox_results_fatigue_interaction_mice <- dplyr::bind_rows(
  extract_cox_interaction_results(
    cox_model = cox_soft_interaction_pool,
    flare_type = "soft",
    variable = "OftenLackEnergy",
    diagnosis = "diagnosis2"
  ),
  extract_cox_interaction_results(
    cox_model = cox_hard_interaction_pool,
    flare_type = "hard",
    variable = "OftenLackEnergy",
    diagnosis = "diagnosis2"
  )
)

# Extract OftenLackEnergy x age_decade interaction terms from diagnosis-stratified models.
cox_results_fatigue_interaction_age_cc <- dplyr::bind_rows(
  extract_cox_interaction_results(
    cox_model = cox_soft_uc_age_interaction,
    flare_type = "soft",
    variable = "OftenLackEnergy",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(
    cox_model = cox_soft_cd_age_interaction,
    flare_type = "soft",
    variable = "OftenLackEnergy",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "CD"),
  extract_cox_interaction_results(
    cox_model = cox_hard_uc_age_interaction,
    flare_type = "hard",
    variable = "OftenLackEnergy",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(
    cox_model = cox_hard_cd_age_interaction,
    flare_type = "hard",
    variable = "OftenLackEnergy",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "CD")
)

cox_results_fatigue_interaction_age_mice <- dplyr::bind_rows(
  extract_cox_interaction_results(
    cox_model = cox_soft_uc_age_interaction_pool,
    flare_type = "soft",
    variable = "OftenLackEnergy",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(
    cox_model = cox_soft_cd_age_interaction_pool,
    flare_type = "soft",
    variable = "OftenLackEnergy",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "CD"),
  extract_cox_interaction_results(
    cox_model = cox_hard_uc_age_interaction_pool,
    flare_type = "hard",
    variable = "OftenLackEnergy",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "UC/IBDU"),
  extract_cox_interaction_results(
    cox_model = cox_hard_cd_age_interaction_pool,
    flare_type = "hard",
    variable = "OftenLackEnergy",
    diagnosis = "age_decade"
  ) %>% dplyr::mutate(diagnosis2 = "CD")
)

# Save
filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Primary analysis/Interactions/"

readr::write_rds(
  x = cox_results_fatigue_interaction_age_mice,
  file = paste0(filepath, "cox_results_fatigue_interaction_age_mice.rds")
)
