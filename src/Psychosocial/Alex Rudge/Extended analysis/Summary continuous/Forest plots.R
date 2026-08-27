library(tidyverse)
library(magrittr)
library(patchwork)

# Load in data
filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Extended analysis/"

# Suffix
#suffix <- "cc"
suffix <- "mice"

suffix_load <- paste0("_", suffix, ".rds")
suffix_save <- paste0(" ", suffix, '.pdf')

cox_results <- readr::read_rds(
  paste0(filepath, "cox_results_all_variables_cts", suffix_load)
)

# NA estimates as the reference level of 1
cox_results %<>%
  tidyr::replace_na(
    list(estimate = 1)
  )

# Significance as a factor
cox_results %<>%
  dplyr::mutate(
    significance = forcats::as_factor(significance)
  )

# Flare type
cox_results_soft <- cox_results %>%
  dplyr::filter(flare_type == 'soft')

cox_results_hard <- cox_results %>%
  dplyr::filter(flare_type == 'hard')

# Forest plot
base_size = 10

# Custom theme
custom_theme <-
  theme_minimal() +
  theme(
    plot.title = element_text(size = base_size + 2),
    plot.subtitle = element_text(size = base_size - 2),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = base_size, colour = 'black'),
    axis.title.x = element_text(size = base_size, colour = 'black'),
    axis.text.x = element_text(size = base_size - 2, colour = 'black')
  )

# Maximum x-limit
x_max <- cox_results %>%
  dplyr::pull(conf.high) %>%
  max(na.rm = TRUE) %>%
  ceiling()

# Function to create forest plot for a variable and its HR and p-value
summon_forest_plot <- function(data, variable, diagnosis2) {

  variable_value <- variable
  diagnosis2_value <- diagnosis2

  data_plot <- data %>%
    dplyr::filter(
      variable == variable_value,
      diagnosis2 == diagnosis2_value
    ) %>%
    dplyr::arrange(desc(ordering))

  plot <- data_plot %>%
    ggplot(aes(
      x = estimate,
      y = forcats::as_factor(term_tidy),
      xmin = conf.low,
      xmax = conf.high,
      colour = significance
    )) +
    geom_point() +
    geom_errorbarh() +
    geom_vline(xintercept = 1, linetype = "dotted") +
    coord_cartesian(xlim = c(0, x_max)) +
    scale_colour_manual(
      limits = c("Reference level", "Not Significant", "Significant"),
      values = c("black", "black", "red"),
      drop = FALSE
    ) +
    xlab("Adjusted Hazard Ratio (aHR)") +
    scale_x_continuous(breaks = seq(0, 6, 1)) +
    custom_theme

  n <- data_plot %>%
    ggplot() +
    geom_text(aes(
      x = 0,
      y = forcats::as_factor(term_tidy),
      label = n
    ),
    size = base_size,
    size.unit = "pt",
    color = 'black'
    ) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))

  hr <- data_plot %>%
    ggplot() +
    geom_text(aes(
      x = 0,
      y = forcats::as_factor(term_tidy),
      label = conf.interval.tidy
    ),
    size = base_size,
    size.unit = "pt",
    color = 'black'
    ) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))

  p <- data_plot %>%
    ggplot() +
    geom_text(aes(
      x = 0,
      y = forcats::as_factor(term_tidy),
      label = p.value.tidy
    ),
    size = base_size,
    size.unit = "pt",
    color = 'black'
    ) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))

  q <- data_plot %>%
    ggplot() +
    geom_text(aes(
      x = 0,
      y = forcats::as_factor(term_tidy),
      label = q.value.tidy
    ),
    size = base_size,
    size.unit = "pt",
    color = 'black'
    ) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))

  list(plot = plot, n = n, hr = hr, p = p, q = q)
}

# Create the plot per flare type per diagnosis
summon_complete_forest <- function(
    data,
    diagnosis2,
    title,
    subtitle = NULL) {

  plot_anxiety <- summon_forest_plot(
    data,
    variable = 'hads_score_anxiety',
    diagnosis2 = diagnosis2
  )
  plot_depression <- summon_forest_plot(
    data,
    variable = 'hads_score_depression',
    diagnosis2 = diagnosis2
  )
  plot_somatisation <- summon_forest_plot(
    data,
    variable = 'TotalPHQ',
    diagnosis2 = diagnosis2
  )
  plot_fatigue <- summon_forest_plot(
    data,
    variable = 'OftenLackEnergy',
    diagnosis2 = diagnosis2
  )
  plot_sleep <- summon_forest_plot(
    data,
    variable = 'SleepDisturbance',
    diagnosis2 = diagnosis2
  )
  plot_exercise <- summon_forest_plot(
    data,
    variable = 'MinimumExercise',
    diagnosis2 = diagnosis2
  )
  plot_lifeevents <- summon_forest_plot(
    data,
    variable = 'AnyLifeEvents',
    diagnosis2 = diagnosis2
  )

  plot_anxiety$plot +
    (plot_anxiety$n +
       labs(title = 'N') +
       theme(plot.title = element_text(size = base_size))) +
    (plot_anxiety$hr +
       labs(title = 'aHR (95% CI)') +
       theme(plot.title = element_text(size = base_size))) +
    (plot_anxiety$p +
       labs(title = 'P-value') +
       theme(plot.title = element_text(size = base_size))) +
    (plot_anxiety$q +
       labs(title = 'Q-value') +
       theme(plot.title = element_text(size = base_size))) +
    plot_depression$plot + plot_depression$n + plot_depression$hr +
    plot_depression$p + plot_depression$q +
    plot_somatisation$plot + plot_somatisation$n + plot_somatisation$hr +
    plot_somatisation$p + plot_somatisation$q +
    plot_fatigue$plot + plot_fatigue$n + plot_fatigue$hr +
    plot_fatigue$p + plot_fatigue$q +
    plot_sleep$plot + plot_sleep$n + plot_sleep$hr +
    plot_sleep$p + plot_sleep$q +
    plot_exercise$plot + plot_exercise$n + plot_exercise$hr +
    plot_exercise$p + plot_exercise$q +
    plot_lifeevents$plot + plot_lifeevents$n + plot_lifeevents$hr +
    plot_lifeevents$p + plot_lifeevents$q +
    patchwork::plot_layout(
      ncol = 5,
      guides = 'collect',
      axes = 'collect',
      width = c(2.5, 0.4, 1.2, 0.5, 0.5),
      height = c(1, 1, 1, 2, 2, 2, 2)
    ) +
    patchwork::plot_annotation(
      title = title,
      subtitle = subtitle
    ) &
    theme(
      plot.title = element_text(size = base_size, hjust = 0.5, face = 'bold'),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "none",
      plot.margin = margin(0, 0, 3, 0)
    )
}

# Patient-reported flare in UC/IBDU
plot_hr_soft_uc <- summon_complete_forest(
  data = cox_results_soft,
  diagnosis2 = 'UC/IBDU',
  title = "Patient-reported flare in UC/IBDU"
)

plot_hr_soft_uc

# Patient-reported flare in CD
plot_hr_soft_cd <- summon_complete_forest(
  data = cox_results_soft,
  diagnosis2 = 'CD',
  title = "Patient-reported flare in CD"
)

plot_hr_soft_cd

# Objective flare in UC/IBDU
plot_hr_hard_uc <- summon_complete_forest(
  data = cox_results_hard,
  diagnosis2 = 'UC/IBDU',
  title = "Objective flare in UC/IBDU"
)

plot_hr_hard_uc

# Objective flare in CD
plot_hr_hard_cd <- summon_complete_forest(
  data = cox_results_hard,
  diagnosis2 = 'CD',
  title = "Objective flare in CD"
)

plot_hr_hard_cd

# Save
filepath_save <- paste0(
  "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Plots/Extended analysis/Continuous/",
  toupper(suffix),
  "/"
)

width = 7
height = 4

ggsave(
  filename = paste0(filepath_save, "HR forest plot soft uc", suffix_save),
  plot = plot_hr_soft_uc,
  width = width,
  height = height,
  units = 'in'
)

ggsave(
  filename = paste0(filepath_save, "HR forest plot soft cd", suffix_save),
  plot = plot_hr_soft_cd,
  width = width,
  height = height,
  units = 'in'
)

ggsave(
  filename = paste0(filepath_save, "HR forest plot hard uc", suffix_save),
  plot = plot_hr_hard_uc,
  width = width,
  height = height,
  units = 'in'
)

ggsave(
  filename = paste0(filepath_save, "HR forest plot hard cd", suffix_save),
  plot = plot_hr_hard_cd,
  width = width,
  height = height,
  units = 'in'
)

# Raw data as a separate table for publication
cox_results_table <- cox_results %>%
  dplyr::arrange(
    flare_type,
    diagnosis2,
    factor(variable, levels = c(
      'hads_score_anxiety',
      'hads_score_depression',
      'TotalPHQ',
      'OftenLackEnergy',
      'SleepDisturbance',
      'MinimumExercise',
      'AnyLifeEvents'
    ))
  ) %>%
  dplyr::select(
    diagnosis2,
    flare_type,
    term_tidy,
    n,
    conf.interval.tidy,
    p.value.tidy
  )

# 4 separate tables
table_hr_soft_uc <- cox_results_table %>%
  dplyr::filter(diagnosis2 == 'UC/IBDU', flare_type == 'soft') %>%
  dplyr::select(-diagnosis2, -flare_type) %>%
  gt::gt() %>%
  gt::cols_label(
    term_tidy = '',
    n = 'N',
    conf.interval.tidy = 'aHR (95% CI)',
    p.value.tidy = 'P-value'
  )

table_hr_soft_cd <- cox_results_table %>%
  dplyr::filter(diagnosis2 == 'CD', flare_type == 'soft') %>%
  dplyr::select(-diagnosis2, -flare_type) %>%
  gt::gt() %>%
  gt::cols_label(
    term_tidy = '',
    n = 'N',
    conf.interval.tidy = 'aHR (95% CI)',
    p.value.tidy = 'P-value'
  )

table_hr_hard_uc <- cox_results_table %>%
  dplyr::filter(diagnosis2 == 'UC/IBDU', flare_type == 'hard') %>%
  dplyr::select(-diagnosis2, -flare_type) %>%
  gt::gt() %>%
  gt::cols_label(
    term_tidy = '',
    n = 'N',
    conf.interval.tidy = 'aHR (95% CI)',
    p.value.tidy = 'P-value'
  )

table_hr_hard_cd <- cox_results_table %>%
  dplyr::filter(diagnosis2 == 'CD', flare_type == 'hard') %>%
  dplyr::select(-diagnosis2, -flare_type) %>%
  gt::gt() %>%
  gt::cols_label(
    term_tidy = '',
    n = 'N',
    conf.interval.tidy = 'aHR (95% CI)',
    p.value.tidy = 'P-value'
  )

suffix_word <- paste0(" ", suffix, '.docx')

gt::gtsave(
  data = table_hr_soft_uc,
  filename = paste0(filepath_save, "Data HR forest plot soft uc", suffix_word)
)

gt::gtsave(
  data = table_hr_soft_cd,
  filename = paste0(filepath_save, "Data HR forest plot soft cd", suffix_word)
)

gt::gtsave(
  data = table_hr_hard_uc,
  filename = paste0(filepath_save, "Data HR forest plot hard uc", suffix_word)
)

gt::gtsave(
  data = table_hr_hard_cd,
  filename = paste0(filepath_save, "Data HR forest plot hard cd", suffix_word)
)
