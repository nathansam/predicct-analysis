
# Functions for the analysis

# Chi squared tests
summon_chisq_test <- function(data, dependent, independent) {
  
  # Chi square tests for the dependent variable vs each independent variable
  
  purrr::map2_df(
    .x = dependent,
    .y = independent,
    .f = function(.x, .y) {
      x <- data %>%
        dplyr::pull(.x)
      
      y <- data %>%
        dplyr::pull(.y)
      
      chisq_test(x = x, y = y) %>%
        dplyr::mutate(dependent = .x, independent = .y)
    }
  ) %>%
    dplyr::mutate(p.adjust = p.adjust(p = p, method = 'holm')) %>%
    dplyr::mutate(p.adjust = signif(p.adjust, 3)) %>%
    dplyr::select(-p.signif) %>%
    dplyr::mutate(
      p.signif = dplyr::case_when(
        p.adjust < 0.0001 ~ "****",
        p.adjust < 0.001 ~ "***",
        p.adjust < 0.01  ~ "**",
        p.adjust < 0.05  ~ "*",
        .default = 'ns'
      )
    )
  
}

# # Test
# dependent = 'score_group'
# independent = c('diagnosis2',
#                 'AgeGroup',
#                 'Sex',
#                 'flare_group',
#                 'cat')
# 
# summon_chisq_test(data = data_baseline, dependent = dependent, independent = independent)


# Creating proportions for plotting baseline data
calc_proportion <- function(data, dependent, independent){
  
  data %>%
    dplyr::group_by(!!rlang::sym(independent), !!rlang::sym(dependent)) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!rlang::sym(independent)) %>%
    dplyr::mutate(p = n/sum(n)) 
  
}

# Function to create all baseline plots


summon_baseline_plots <- function(data, dependent, independent) {
  
  # Chi squared tests
  chisq_results <- summon_chisq_test(
    data = data,
    dependent = dependent,
    independent = independent
  ) %>%
    dplyr::transmute(independent, p.adjust) %>%
    {setNames(as.list(.$p.adjust), .$independent)}

  
  # Creating plots for baseline variables with the dependent variable
  
  plot_list <- list()
  
  for (y in independent) {
    
    # Create a plot for each independent variable
    
    plot <- data %>%
      calc_proportion(., dependent = dependent, independent = y) %>%
      ggplot(aes(x = .data[[dependent]], y = p, fill = .data[[y]])) +
      geom_col(position = 'dodge') +
      annotate(
        "text",
        label = paste0("Adjusted p-value: ", chisq_results[[y]]),
        x = Inf,
        y = Inf,
        vjust = 2,
        hjust = 1.3
      )
    
    plot_list[[y]] <- plot
    
  }
  
  # Return the plots
  plot_list
  
}
    



# Test with HADS data
# State dependent and independent variables
# dependent = 'score_group'
# independent = c('diagnosis2',
#                 'AgeGroup',
#                 'Sex',
#                 'flare_group',
#                 'cat')
# 
# 
# data = data_baseline_anxiety
# 
# baseline_plots <- summon_baseline_plots(data = data, dependent = dependent, independent = independent)
# 
# baseline_plots$diagnosis2
# baseline_plots$AgeGroup
# baseline_plots$Sex
# baseline_plots$flare_group
# baseline_plots$cat





