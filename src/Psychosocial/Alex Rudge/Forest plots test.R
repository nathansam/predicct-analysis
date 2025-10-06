library(tidyverse)
library(magrittr)
library(patchwork)

# Load in data

filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/"

cox_results <- readr::read_rds(paste0(filepath, "cox_results_all_variables.rds"))

# NA estimates as the reference level of 1
cox_results %<>%
  tidyr::replace_na(
    list(estimate = 1)
  )

# Remove reference group for binary variables (as the reference is obvious)
cox_results %<>%
  dplyr::filter(
    !term == 'MinimumExerciseYes',
    !term == 'AnyLifeEventsNo',
    !term == 'SleepDisturbanceNo'
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

# Custom theme
custom_theme <- 
  theme_minimal() +
  theme(
  # Title
  plot.title = element_text(size = 10),
  plot.subtitle = element_text(size = 8),
  plot.title.position = "plot", 
  plot.caption.position = "plot",
  # Axes
  axis.title.y = element_blank()
)

# Do separate plots of UC and CD

summon_forest_plot <- function(data, variable, diagnosis2){
  
  # Variable and diagnosis2 alias
  variable_value <- variable
  diagnosis2_value <- diagnosis2
  
  # Filter data
  data_plot <- data %>%
    dplyr::filter(
      variable == variable_value,
      diagnosis2 == diagnosis2_value) %>%
    dplyr::arrange(desc(ordering))
  
  plot <- data_plot  %>%
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
    coord_cartesian(xlim = c(0, 7)) +
    # Legend colours
    scale_colour_manual(
      limits = c("Reference level", "Not Significant", "Significant"),
      values = c("black", "black", "forestgreen"),
      drop = FALSE) +
    # Axes labels
    xlab("Hazard Ratio (HR)") +
    # Axes ticks
    scale_x_continuous(
      breaks = seq(0, 6, 1)
    ) +
    custom_theme
  
  # HR
  hr <- data_plot %>%
    ggplot() +
    geom_text(aes(
      x = 0,
      y = forcats::as_factor(term_tidy),
      label = conf.interval)) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # P-value
  p <- data_plot %>%
    ggplot() +
    geom_text(aes(
      x = 0,
      y = forcats::as_factor(term_tidy),
      label = p.value.tidy)) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Return
  list(plot = plot, hr = hr, p = p)
  
}

plot_anxiety_uc <- summon_forest_plot(cox_results_soft, variable = 'score_group_anxiety', diagnosis2 = 'UC/IBDU')
plot_depression_uc <- summon_forest_plot(cox_results_soft, variable = 'score_group_depression', diagnosis2 = 'UC/IBDU')
plot_exercise_uc <- summon_forest_plot(cox_results_soft, variable = 'MinimumExercise', diagnosis2 = 'UC/IBDU')
plot_lifeevents_uc <- summon_forest_plot(cox_results_soft, variable = 'AnyLifeEvents', diagnosis2 = 'UC/IBDU')
plot_sleep_uc <- summon_forest_plot(cox_results_soft, variable = 'SleepDisturbance', diagnosis2 = 'UC/IBDU')
plot_somatisation_uc <- summon_forest_plot(cox_results_soft, variable = 'somatisation', diagnosis2 = 'UC/IBDU')

plot_anxiety_uc$plot + (plot_anxiety_uc$hr + labs(title = 'HR (95% CI)')) + (plot_anxiety_uc$p + labs(title = 'P-value')) +
  plot_depression_uc$plot + plot_depression_uc$hr +  plot_depression_uc$p +
  plot_exercise_uc$plot + plot_exercise_uc$hr + plot_exercise_uc$p +
  plot_lifeevents_uc$plot + plot_lifeevents_uc$hr + plot_lifeevents_uc$p +
  plot_sleep_uc$plot + plot_sleep_uc$hr + plot_sleep_uc$p +
  plot_somatisation_uc$plot + plot_somatisation_uc$hr + plot_somatisation_uc$p +
  patchwork::plot_layout(
    ncol = 3,
    guides = 'collect',
    axes = 'collect',
    width = c(3, 1, 0.5),
    height = c(3,3,1,1,1,4)
  ) +
  patchwork::plot_annotation(
    title = "Psychosocial Variables",
    subtitle = "UC/IBDU"
  ) &
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",
    plot.margin = margin(0, 0, 1, 0))


# May have wasted my time - looks fine in a single plot

plot <- cox_results_soft %>%
  dplyr::filter(diagnosis2 == 'UC/IBDU') %>%
  dplyr::mutate(term_tidy = forcats::as_factor(term_tidy)) %>%
  dplyr::mutate(term_tidy = forcats::fct_rev(term_tidy)) %>%
  ggplot(aes(
    x = estimate,
    y = term_tidy,
    xmin = conf.low,
    xmax = conf.high,
    colour = significance
  )) +
  geom_point() +
  geom_errorbarh() +
  geom_vline(xintercept = 1, linetype = "dotted") +
  coord_cartesian(xlim = c(0, 7)) +
  # Legend colours
  scale_colour_manual(
    limits = c("Reference level", "Not Significant", "Significant"),
    values = c("black", "black", "forestgreen"),
    drop = FALSE) +
  # Axes labels
  xlab("Hazard Ratio (HR)") +
  # Axes ticks
  scale_x_continuous(
    breaks = seq(0, 6, 1)
  ) +
  custom_theme

# Hazard ratios
hr <- cox_results_soft %>%
  dplyr::filter(diagnosis2 == 'UC/IBDU') %>%
  dplyr::mutate(term_tidy = forcats::as_factor(term_tidy)) %>%
  dplyr::mutate(term_tidy = forcats::fct_rev(term_tidy)) %>%
  ggplot() +
  geom_text(aes(
    x = 0,
    y = term_tidy,
    label = conf.interval)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

# P values
p <- cox_results_soft %>%
  dplyr::filter(diagnosis2 == 'UC/IBDU') %>%
  dplyr::mutate(term_tidy = forcats::as_factor(term_tidy)) %>%
  dplyr::mutate(term_tidy = forcats::fct_rev(term_tidy)) %>%
  ggplot() +
  geom_text(aes(
    x = 0,
    y = term_tidy,
    label = p.value.tidy)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

# Together
plot + 
  (hr + labs(title = 'HR (95% CI)')) + 
  (p + labs(title = 'P-values')) +
  patchwork::plot_layout(
  ncol = 3,
  width = c(3,1,1),
  guides = 'collect',
  axes = 'collect'
) +
  patchwork::plot_annotation(
    title = "Psychosocial Variables",
    subtitle = "UC/IBDU"
  ) &
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom")
