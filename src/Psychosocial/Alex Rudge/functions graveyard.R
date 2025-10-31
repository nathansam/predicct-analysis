# Chi squared tests
summon_chisq_test <- function(data, dependent, independent) {
  
  # Chi square tests/Fisher's exact test for the dependent variable vs each independent variable
  
  purrr::map2_df(
    .x = independent,
    .y = dependent,
    .f = function(.x, .y) {
      x <- data %>%
        dplyr::pull(.x)
      
      y <- data %>%
        dplyr::pull(.y)
      
      # Check whether we need a Fisher test
      fisher_flag <- data %>%
        # Count combinations of x and y
        dplyr::count(!!rlang::sym(.x), !!rlang::sym(.y)) %>%
        # Pull the count
        dplyr::pull(n) %>%
        # Check if any counts are < 5
        min() < 5
      
      if (fisher_flag){
        # If one of the counts is < 5 use Fisher exact test
        fisher_test(table(x, y), simulate.p.value = TRUE, B = 1e5) %>%
          dplyr::mutate(
            independent = .x, 
            dependent = .y,
            method = 'Fisher\'s exact test')
        
      } else {    
        # Otherwise use chi square
        chisq_test(x = x, y = y) %>%
          dplyr::mutate(independent = .x, dependent = .y)
      }
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


summon_baseline_plots2 <- function(data, dependent, independent) {
  # Allowing the flexibility for independent to be continuous also
  
  # Chi squared tests
  statistical_test_results <- summon_statistical_test(
    data = data,
    dependent = dependent,
    independent = independent
  )
  
  
  # Creating plots for baseline variables with the dependent variable
  
  plot_list <- list()
  
  for (y in independent) {
    
    # Check if continuous variable
    continuous_flag <- data %>% 
      dplyr::pull(y) %>%
      is.numeric()
    
    if (continuous_flag){
      
      # Label for the plot needs multiple tests on it
      test_result <- statistical_test_results %>%
        dplyr::filter(independent == y) %>%
        dplyr::mutate(label = paste0(group1, " vs ", group2, ": ", p.adjust))
      
      label <- test_result %>%
        dplyr::pull(label) %>%
        paste0(., collapse="\n") %>%
        {paste0("Adjusted p-values:\n", .)}
      
      # Plot
      plot <- data %>%
        dplyr::filter(
          !is.na(!!sym(dependent)),
          !is.na(!!sym(y))
        ) %>%
        ggplot(aes(x = .data[[y]], colour = .data[[dependent]])) +
        geom_density() +
        annotate(
          "text",
          label = label,
          x = Inf,
          y = Inf,
          vjust = 1.1,
          hjust = 1.3
        ) +
        ylab("") +
        theme_minimal()
      
    } else {
      # Discrete variables
      
      label <- statistical_test_results %>%
        dplyr::filter(independent == y) %>%
        dplyr::pull(p.adjust) %>%
        { p.adjust <- .
        paste0("Adjusted p-value: ", p.adjust)}
      
      plot <- data %>%
        dplyr::filter(
          !is.na(!!sym(dependent)),
          !is.na(!!sym(y))
        ) %>%
        calc_proportion(., dependent = dependent, independent = y) %>%
        ggplot(aes(x = .data[[dependent]], y = p, fill = .data[[y]])) +
        geom_col(position = 'dodge') +
        annotate(
          "text",
          label = label,
          x = Inf,
          y = Inf,
          vjust = 2,
          hjust = 1.3
        ) + 
        theme_minimal()
    }
    
    plot_list[[y]] <- plot
    
  }
  
  # Return the plots
  plot_list
  
}