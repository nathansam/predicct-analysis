

data = results


# Patient reported flares in UC
plots_soft_cd <- purrr::map(
  .x = variables,
  .f = function(x)
    summon_forest_plot(data, x, 'CD', 'soft')
)

plots_soft_uc <- purrr::map(
  .x = variables,
  .f = function(x)
    summon_forest_plot(data, x, 'UC', 'soft')
)

plots_hard_cd <- purrr::map(
  .x = variables,
  .f = function(x)
    summon_forest_plot(data, x, 'CD', 'hard')
)

plots_hard_uc <- purrr::map(
  .x = variables,
  .f = function(x)
    summon_forest_plot(data, x, 'UC', 'hard')
)

# Add titles to header row
plots_soft_cd[[1]]$plot <- plots_soft_cd[[1]]$plot + 
  labs(title = "Patient-reported flare in CD") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
plots_soft_cd[[1]]$hr <- plots_soft_cd[[1]]$hr + labs(title = "aHR (95% CI)")
plots_soft_cd[[1]]$p <- plots_soft_cd[[1]]$p  + labs(title = "P-value")

plots_soft_uc[[1]]$plot <- plots_soft_uc[[1]]$plot + 
  # Title
  labs(title = "Patient-reported flare in UC/IBDU") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
    )
plots_soft_uc[[1]]$hr <- plots_soft_uc[[1]]$hr + labs(title = "aHR (95% CI)")
plots_soft_uc[[1]]$p <- plots_soft_uc[[1]]$p  + labs(title = "P-value")

plots_hard_cd[[1]]$plot <- plots_hard_cd[[1]]$plot + 
  labs(title = "Objective flare in CD")  +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
plots_hard_cd[[1]]$hr <- plots_hard_cd[[1]]$hr + labs(title = "aHR (95% CI)")
plots_hard_cd[[1]]$p <- plots_hard_cd[[1]]$p  + labs(title = "P-value")

plots_hard_uc[[1]]$plot <- plots_hard_uc[[1]]$plot + 
  labs(title = "Objective flare in UC/IBDU") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
plots_hard_uc[[1]]$hr <- plots_hard_uc[[1]]$hr + labs(title = "aHR (95% CI)")
plots_hard_uc[[1]]$p <- plots_hard_uc[[1]]$p  + labs(title = "P-value")


# Remove y axis labels for the UC plots

plots_soft_uc <- plots_soft_uc %>%
  purrr::map(
    .f = function(x){
      x$plot <- x$plot +
        theme(axis.text.y = element_blank())
      
      x
    }
  )

plots_hard_uc <- plots_hard_uc %>%
  purrr::map(
    .f = function(x){
      x$plot <- x$plot +
        theme(axis.text.y = element_blank())
      
      x
    }
  )




# Weave plots into one list

plots <- c(c(rbind(plots_soft_cd, plots_soft_uc)), c(rbind(plots_hard_cd, plots_hard_uc)))


# Extract plot heights
heights = data %>%
  dplyr::distinct(variable, level) %>%
  # Factor to preserve order of variables
  dplyr::mutate(variable = forcats::as_factor(variable)) %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(h = n()) %>%
  dplyr::pull(h)

patchwork_plots <- plots %>%
  purrr::list_flatten() %>%
  Reduce('+', .) +
  patchwork::plot_layout(
    ncol = 6,
    guides = 'collect',
    axes = 'collect',
    width = c(2.5, 2, 0.7, 2.5, 2, 0.7),
    heights = c(heights, heights)
  ) &
  theme(
    legend.position = "none"
  )

patchwork_plots
