library(tidyverse)
library(magrittr)
library(patchwork)

# Load in data

filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Full cohort/"

# Suffix
suffix <- "cc"
#suffix <- "mice"

suffix_load <- paste0("_", suffix, ".rds")
suffix_save <- paste0(" ", suffix, '.pdf')

cox_results <- readr::read_rds(paste0(filepath, "cox_results_all_variables", suffix_load))

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

# p.value.tidy bmj or lancet
cox_results %<>%
  dplyr::rename(p.value.tidy = p.value.tidy.lancet)


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
  plot.title = element_text(size = 12),
  plot.subtitle = element_text(size = 10),
  # Axes
  axis.title.y = element_blank(),
  axis.text.y = element_text(size = 12, colour = 'black'),
  axis.title.x = element_text(size = 12, colour = 'black'),
  axis.text.x = element_text(size = 10, colour = 'black')
)

# Maximum xlimit
x_max <- 5

# Function to create forest plot for a variable as well as their HR and pvalue

summon_forest_plot <- function(data, variable){
  
  # Variable and diagnosis2 alias
  variable_value <- variable
  
  # Filter data
  data_plot <- data %>%
    dplyr::filter(
      variable == variable_value) %>%
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
    coord_cartesian(xlim = c(0, x_max)) +
    # Legend colours
    scale_colour_manual(
      limits = c("Reference level", "Not Significant", "Significant"),
      values = c("black", "black", "red"),
      drop = FALSE) +
    # Axes labels
    xlab("Adjusted Hazard Ratio (aHR)") +
    # Axes ticks
    scale_x_continuous(
      breaks = seq(0, 6, 1)
    ) +
    custom_theme
  
  # n, sample size
  n <- data_plot %>%
    ggplot() +
    geom_text(aes(
      x = 0,
      y = forcats::as_factor(term_tidy),
      label = n),
      size = 12,
      size.unit = "pt",
      color = 'black'
    ) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # HR
  hr <- data_plot %>%
    ggplot() +
    geom_text(aes(
      x = 0,
      y = forcats::as_factor(term_tidy),
      label = conf.interval.tidy),
      size = 12,
      size.unit = "pt",
      color = 'black'
      ) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # P-value
  p <- data_plot %>%
    ggplot() +
    geom_text(aes(
      x = 0,
      y = forcats::as_factor(term_tidy),
      label = p.value.tidy),
      size = 12,
      size.unit = "pt",
      color = 'black'
      ) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Return
  list(plot = plot, n = n, hr = hr, p = p)
  
}


# Create the plot per flare type per diagnosis

summon_complete_forest <- function(
    data,
    title,
    subtitle = NULL) {

  # Patient reported flares in UC
  plot_anxiety <- summon_forest_plot(data, variable = 'score_group_anxiety')
  plot_depression <- summon_forest_plot(data, variable = 'score_group_depression')
  plot_somatisation <- summon_forest_plot(data, variable = 'somatisation')
  plot_fatigue <- summon_forest_plot(data, variable = 'OftenLackEnergy')
  plot_sleep <- summon_forest_plot(data, variable = 'SleepDisturbance')
  plot_exercise <- summon_forest_plot(data, variable = 'MinimumExercise')
  plot_lifeevents <- summon_forest_plot(data, variable = 'AnyLifeEvents')
  


  plot_anxiety$plot + 
    (plot_anxiety$n + 
       labs(title = 'N') + 
       theme(plot.title = element_text(size = 12))) +
    (plot_anxiety$hr + 
       labs(title = 'aHR (95% CI)') + 
       theme(plot.title = element_text(size = 12))) + 
    (plot_anxiety$p + 
       labs(title = 'P-value') +
       theme(plot.title = element_text(size = 12))) +
   plot_depression$plot + plot_depression$n + plot_depression$hr +  plot_depression$p +
   plot_somatisation$plot + plot_somatisation$n + plot_somatisation$hr + plot_somatisation$p +
   plot_fatigue$plot + plot_fatigue$n + plot_fatigue$hr + plot_fatigue$p +
   plot_sleep$plot + plot_sleep$n + plot_sleep$hr + plot_sleep$p +
   plot_exercise$plot + plot_exercise$n + plot_exercise$hr + plot_exercise$p +
   plot_lifeevents$plot + plot_lifeevents$n + plot_lifeevents$hr + plot_lifeevents$p +
    patchwork::plot_layout(
      ncol = 4,
     guides = 'collect',
     axes = 'collect',
     width = c(2.5, 0.4, 1.2, 0.5),
     height = c(2,2,3,2,2,2,2)
   ) +
   patchwork::plot_annotation(
      title = title,
      subtitle = subtitle
   ) &
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
     plot.subtitle = element_text(hjust = 0.5),
      legend.position = "none",
     plot.margin = margin(0, 0, 3, 0))
}

# Soft
plot_hr_soft <- summon_complete_forest(
  data = cox_results_soft,
  title = "Patient-reported flare in IBD"
)

plot_hr_soft

# Hard 
plot_hr_hard <- summon_complete_forest(
  data = cox_results_hard,
  title = "Objective flare in IBD"
)

plot_hr_hard




# Save
filepath_save <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Plots/Full cohort/"

# soft
ggsave(
  filename = paste0(filepath_save, "HR forest plot soft", suffix_save),
  plot = plot_hr_soft,
  width = 9.5,
  height = 7,
  units = 'in'
)

# hard 
ggsave(
  filename = paste0(filepath_save, "HR forest plot hard", suffix_save),
  plot = plot_hr_hard,
  width = 9.5,
  height = 7,
  units = 'in'
)


# Raw data as a separate table for publication

cox_results_table <- cox_results %>%
  dplyr::arrange(
    flare_type, 
    # Ordering to match the forest plot ordering
    factor(variable, levels = c(
      'score_group_anxiety', 
      'score_group_depression', 
      'somatisation',
      'OftenLackEnergy', 
      'SleepDisturbance',
      'MinimumExercise', 
      'AnyLifeEvents'))
  ) %>%
  dplyr::select(flare_type, term_tidy, n, conf.interval.tidy, p.value.tidy)

# 4 separate tables
# Soft uc
table_hr_soft <- cox_results_table %>%
  dplyr::filter(
    flare_type == 'soft'
  ) %>%
  dplyr::select(-flare_type) %>%
  gt::gt() %>%
  gt::cols_label(
    term_tidy = '',
    n = 'N',
    conf.interval.tidy = 'aHR (95% CI)',
    p.value.tidy = 'P-value'
  )

# hard
table_hr_hard <- cox_results_table %>%
  dplyr::filter(
    flare_type == 'hard'
  ) %>%
  dplyr::select(-flare_type) %>%
  gt::gt() %>%
  gt::cols_label(
    term_tidy = '',
    n = 'N',
    conf.interval.tidy = 'aHR (95% CI)',
    p.value.tidy = 'P-value'
  )

suffix_word <- paste0(" ", suffix, '.docx')

# Save as word
# soft
gt::gtsave(
  data = table_hr_soft,
  filename = paste0(filepath_save, "Data HR forest plot soft", suffix_word)
)


# hard
gt::gtsave(
  data = table_hr_hard,
  filename = paste0(filepath_save, "Data HR forest plot hard", suffix_word)
)

