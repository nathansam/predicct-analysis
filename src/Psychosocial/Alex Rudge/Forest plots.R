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
  # Axes
  axis.title.y = element_blank()
)

# Do separate plots of UC and CD

# Function to create forest plot for a variable as well as their HR and pvalue

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
      values = c("black", "black", "red"),
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
      label = conf.interval.tidy)) +
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


# Create the plot per flare type per diagnosis

summon_complete_forest <- function(
    data,
    diagnosis2,
    title) {

  # Patient reported flares in UC
  plot_anxiety <- summon_forest_plot(data, variable = 'score_group_anxiety', diagnosis2 = diagnosis2)
  plot_depression <- summon_forest_plot(data, variable = 'score_group_depression', diagnosis2 = diagnosis2)
  plot_exercise <- summon_forest_plot(data, variable = 'MinimumExercise', diagnosis2 = diagnosis2)
  plot_alcohol <- summon_forest_plot(data, variable = 'weekly_units', diagnosis2 = diagnosis2)
  plot_lifeevents <- summon_forest_plot(data, variable = 'AnyLifeEvents', diagnosis2 = diagnosis2)
  plot_sleep <- summon_forest_plot(data, variable = 'SleepDisturbance', diagnosis2 = diagnosis2)
  plot_somatisation <- summon_forest_plot(data, variable = 'somatisation', diagnosis2 = diagnosis2)

  plot_anxiety$plot + 
    (plot_anxiety$hr + 
       labs(title = 'HR (95% CI)') + 
       theme(plot.title = element_text(size = 12))) + 
    (plot_anxiety$p + 
       labs(title = 'P-value') +
       theme(plot.title = element_text(size = 12))) +
   plot_depression$plot + plot_depression$hr +  plot_depression$p +
   plot_exercise$plot + plot_exercise$hr + plot_exercise$p +
    plot_alcohol$plot + plot_alcohol$hr + plot_alcohol$p +
   plot_lifeevents$plot + plot_lifeevents$hr + plot_lifeevents$p +
   plot_sleep$plot + plot_sleep$hr + plot_sleep$p +
   plot_somatisation$plot + plot_somatisation$hr + plot_somatisation$p +
    patchwork::plot_layout(
      ncol = 3,
     guides = 'collect',
     axes = 'collect',
     width = c(3, 1, 0.5),
     height = c(3,3,1,3,1,1,4)
   ) +
   patchwork::plot_annotation(
      title = title
   ) &
    theme(
      plot.title = element_text(hjust = 0.5),
     plot.subtitle = element_text(hjust = 0.5),
      legend.position = "none",
     plot.margin = margin(0, 0, 3, 0))
}

# Soft UC
summon_complete_forest(
  data = cox_results_soft,
  diagnosis2 = 'UC/IBDU',
  title = "Patient reported flare in UC/IBDU"
)

# Soft CD
summon_complete_forest(
  data = cox_results_soft,
  diagnosis2 = 'CD',
  title = "Patient reported flare in CD"
)

# Hard UC
summon_complete_forest(
  data = cox_results_hard,
  diagnosis2 = 'UC/IBDU',
  title = "Hard flare in UC/IBDU"
)

# Hard CD
summon_complete_forest(
  data = cox_results_hard,
  diagnosis2 = 'CD',
  title = "Hard flare in CD"
)
