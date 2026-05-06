library(tidyverse)
library(magrittr)
library(glue)
library(broom)

tidy_cox_results <- function(cox_model, variable, ibd_type, flare_type) {
  
  cox_model %>%
    purrr::pluck('xlevels') %>%
    purrr::pluck(variable) %>%
    # Create tibble to store Cox results including reference level
    {
      tibble(term = paste0(variable, .), variable = variable, level = .)
    } %>%
    dplyr::left_join(
      cox_model %>%
        broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%
        dplyr::filter(stringr::str_starts(term, variable)),
      by = "term"
    ) %>%
    # Diagnosis
    dplyr::mutate(ibd_type = ibd_type, flare_type = flare_type) %>%
    dplyr::select(
      tidyselect::any_of(
        c('term', 'variable', 'level', 'estimate', 'std.error', 'statistic', 'df', 'p.value', 'conf.low', 'conf.high', 'ibd_type', 'flare_type')))
}

read_and_tidy_cox <- function(subdir, file_prefix, variable) {
  tibble(
    ibd_type = c("CD", "CD", "UC", "UC"),
    flare_type = c("soft", "hard", "soft", "hard")
  ) %>%
    dplyr::mutate(
      path = glue(
        "{filepath_data}/{subdir}/{file_prefix}-{tolower(ibd_type)}-{flare_type}.rds"
      )
    ) %>%
    purrr::pmap(
      .f = function(path, ibd_type, flare_type) {
        tidy_cox_results(
          cox_model = readr::read_rds(path),
          variable = variable,
          ibd_type = ibd_type,
          flare_type = flare_type
        )
      }
    ) %>%
    purrr::list_rbind()
}

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


summon_forest_plot <- function(data, variable, ibd_type, flare_type){
  
  # Maximum value for the x axis
  x_max <- data %>% dplyr::pull(conf.high) %>% max(na.rm = TRUE) %>% ceiling()
  
  # Variable and ibd_type alias
  variable_value <- variable
  ibd_type_value <- ibd_type
  flare_type_value <- flare_type
  
  # Filter data
  data_plot <- data %>%
    dplyr::filter(
      variable == variable_value,
      ibd_type == ibd_type_value,
      flare_type == flare_type_value) %>%
    dplyr::arrange(desc(ordering))
  
  plot <- data_plot %>%
    ggplot(aes(
      x = estimate,
      y = forcats::as_factor(term.tidy),
      xmin = conf.low,
      xmax = conf.high,
      colour = significance
    )) +
    geom_point() +
    geom_errorbarh() +
    geom_vline(xintercept = 1, linetype = "dotted") +
    # Legend colours
    scale_colour_manual(
      limits = c("Reference level", "Not Significant", "Significant"),
      values = c("black", "black", "red"),
      drop = FALSE) +
    # Axes labels
    xlab("Adjusted Hazard Ratio (aHR)") +
    # Axes ticks
    scale_x_continuous(
      breaks = seq(0, x_max, 1),
      limits = c(0, x_max)
    ) +
    custom_theme
  
  # HR
  hr <- data_plot %>%
    ggplot() +
    geom_text(aes(
      x = 0,
      y = forcats::as_factor(term.tidy),
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
      y = forcats::as_factor(term.tidy),
      label = p.value.tidy),
      size = 12,
      size.unit = "pt",
      color = 'black'
    ) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Return
  list(plot = plot, hr = hr, p = p)
  
}

summon_complete_forest_plot <- function(data,
                                        variables,
                                        ibd_type,
                                        flare_type,
                                        title = NULL,
                                        subtitle = NULL) {
  # Patient reported flares in UC
  plots <- purrr::map(
    .x = variables,
    .f = function(x)
      summon_forest_plot(data, x, ibd_type, flare_type)
  )
  
  # Add titles to header row
  
  plots[[1]]$hr <- plots[[1]]$hr + labs(title = "aHR (95% CI)")
  plots[[1]]$p <- plots[[1]]$p  + labs(title = "P-value")
  
  patchwork_plots <- plots %>%
    purrr::list_flatten() %>%
    Reduce('+', .)
  
  # Extract plot heights
  heights = data %>%
    dplyr::distinct(variable, level) %>%
    # Factor to preserve order of variables
    dplyr::mutate(variable = forcats::as_factor(variable)) %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(h = n()) %>%
    dplyr::pull(h)
  
  patchwork_plots <- patchwork_plots +
    patchwork::plot_layout(
      ncol = 3,
      guides = 'collect',
      axes = 'collect',
      width = c(2.5, 1.2, 0.5),
      heights = heights
    )
  
  # Title and theme
  if (is.null(title)) {
    flare_type_title <- ifelse(flare_type == 'soft',
                               "Patient-reported flare",
                               "Objective flare")
    ibd_type_title <- ifelse(ibd_type == 'CD', "CD", "UC/IBDU")
    
    title <- glue("{flare_type_title} in {ibd_type_title}")
  }
  
  patchwork_plots +
    patchwork::plot_annotation(title = title, subtitle = subtitle) &
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "none",
      plot.margin = margin(0, 0, 3, 0)
    )
  
}