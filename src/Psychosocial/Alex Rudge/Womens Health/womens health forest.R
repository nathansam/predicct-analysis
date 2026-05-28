library(tidyverse)
library(magrittr)
library(glue)
library(survival)
library(broom)



# Load data
filepath_data <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct4/cox-objects"

results <- readr::read_rds(glue("{filepath_data}/cox_all_results.rds"))


# NA estimates as the reference level of 1
results %<>%
  tidyr::replace_na(
    list(estimate = 1)
  )

# Significance as a factor
results %<>%
  dplyr::mutate(
    significance = forcats::as_factor(significance)
  )

variables = c(
  'meno2',
  'GlobalAUB',
  'PeriodTypePain',
  'SufferPMS',
  'IBDsymptoms_menses',
  'hormonal2'
)

# CD
# Patient reported
plot_cd_soft <- summon_complete_forest_plot(
  data = results,
  variables = variables,
  ibd_type = 'CD',
  flare_type = 'soft'
)

# Objective
plot_cd_hard <- summon_complete_forest_plot(
  data = results,
  variables = variables,
  ibd_type = 'CD',
  flare_type = 'hard'
)


# UC
# Patient reported
plot_uc_soft <- summon_complete_forest_plot(
  data = results,
  variables = variables,
  ibd_type = 'UC',
  flare_type = 'soft'
)

# Objective
plot_uc_hard <- summon_complete_forest_plot(
  data = results,
  variables = variables,
  ibd_type = 'UC',
  flare_type = 'hard'
)


# Save
# ggsave(
#   filename = glue("{filepath_data}/Forest_plot_cd_soft.pdf"),
#   plot = plot_cd_soft,
#   width = 11,
#   height = 6,
#   units = 'in'
# )
# 
# ggsave(
#   filename = glue("{filepath_data}/Forest_plot_cd_hard.pdf"),
#   plot = plot_cd_hard,
#   width = 11,
#   height = 6,
#   units = 'in'
# )
# 
# ggsave(
#   filename = glue("{filepath_data}/Forest_plot_uc_soft.pdf"),
#   plot = plot_uc_soft,
#   width = 11,
#   height = 6,
#   units = 'in'
# )
# 
# ggsave(
#   filename = glue("{filepath_data}/Forest_plot_uc_hard.pdf"),
#   plot = plot_uc_hard,
#   width = 11,
#   height = 6,
#   units = 'in'
# )
