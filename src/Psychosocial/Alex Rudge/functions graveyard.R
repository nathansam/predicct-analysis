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


summon_risk_difference_factor <- function(data, model, time, variable, ref_level = NULL) {
  # Function that calculates (population average) risk difference of a variable
  # at a given time point with respect to specified value of the variable from a
  # Cox model
  
  # Values of the variable
  values <- data %>%
    dplyr::pull(variable) %>%
    levels()
  
  # If no reference level then use the first factor level
  if (is.null(ref_level)) {
    ref_level <- values %>%
      dplyr::first()
  }
  
  # Identify the time variable - to differentiate between soft and hard flare models
  time_variable <- all.vars(terms(model))[1]
  
  results <- purrr::map(
    .x = values,
    .f = function(x) {
      t <- time 
      
      newdata <- data %>%
        # Set values for time and the variable
        dplyr::mutate(!!sym(time_variable) := t, !!sym(variable) := x)
      
      # Estimate expected for entire population
      expected <- predict(model, newdata = newdata, type = "expected")
      
      # Calculate mean cumulative incidence (1 - survival)
      cum_incidence <- (1 - exp(-expected)) %>% mean(na.rm = TRUE)
      
      # Return as a tibble row
      tibble(
        time = time,
        variable = variable,
        level = x,
        cum_incidence = cum_incidence
      )
    }
  ) %>%
    purrr::list_rbind()
  
  # Calculate difference relative to the reference
  cum_incidence_ref <- results %>%
    dplyr::filter(level == ref_level) %>%
    dplyr::pull(cum_incidence)
  
  results %>%
    dplyr::mutate(rd = (cum_incidence - cum_incidence_ref) * 100)
  
}


summon_risk_difference_factor_boot <- function(data,
                                               model,
                                               time = 365,
                                               variable,
                                               ref_level = NULL,
                                               nboot = 99,
                                               seed = 1) {
  
  
  # Function to calculate bootstrapped risk difference
  
  # Set seed
  set.seed(seed)
  
  # Values of the variable
  values <- data %>%
    dplyr::pull(variable) %>%
    levels()
  
  # If no reference level then use the first factor level
  if (is.null(ref_level)) {
    ref_level <- values %>%
      dplyr::first()
  }
  
  # Remove NAs from the data
  # Variable used in the model
  all_variables <- all.vars(terms(model))
  
  data %<>%
    # Remove any NAs as Cox doesn't use these
    dplyr::select(tidyselect::all_of(all_variables)) %>%
    dplyr::filter(!dplyr::if_any(.cols = everything(), .fns = is.na))
  
  purrr::map_dfr(
    .x = seq_len(nboot),
    .f = function(b) {
      
      data_boot <- data %>%
        dplyr::group_by(DiseaseFlareYN) %>%
        # Sample the df
        dplyr::slice_sample(prop = 1, replace = TRUE) %>%
        dplyr::ungroup()
      
      # Refit cox model of bootstrapped data
      model_boot <- coxph(formula(model), data = data_boot, model = TRUE)
      
      # Calculate risk differences
      summon_risk_difference_factor(
        data = data_boot,
        model = model_boot,
        time = time,
        variable = variable,
        ref_level = ref_level
      )
    }
  ) %>%
    dplyr::group_by(time, level) %>%
    # Bootstrapped estimate and confidence intervals
    dplyr::summarise(
      mean_rd = mean(rd, na.rm = TRUE),
      conf.low = quantile(rd, prob = 0.025, na.rm = TRUE),
      conf.high = quantile(rd, prob = 0.975, na.rm = TRUE),
      nboot = sum(!is.na(rd))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(estimate = mean_rd) %>%
    # Note variable, reference level flag
    dplyr::mutate(
      variable = variable,
      reference_flag = (level == ref_level)
    ) %>%
    # Ordering for plotting
    dplyr::arrange(time, level) %>%
    dplyr::group_by(time) %>%
    dplyr::mutate(ordering = dplyr::row_number()) %>%
    # Tidy confidence intervals
    # Remove confidence intervals for reference level
    dplyr::mutate(
      conf.low = dplyr::case_when(reference_flag == TRUE ~ NA,
                                  .default = conf.low
      ),
      conf.high = dplyr::case_when(reference_flag == TRUE ~ NA,
                                   .default = conf.high
      )
    ) %>%
    dplyr::mutate(
      conf.interval.tidy = dplyr::case_when(
        (is.na(conf.low) & is.na(conf.high)) ~ "-",
        TRUE ~ paste0(
          sprintf("%.3g", estimate),
          " (",
          sprintf("%.3g", conf.low),
          ", ",
          sprintf("%.3g", conf.high),
          ")"
        )
      )
    ) %>%
    # Significance
    dplyr::mutate(
      significance = dplyr::case_when(
        reference_flag == TRUE ~ "Reference level",
        sign(conf.low) == sign(conf.high) ~ "Significant",
        sign(conf.low) != sign(conf.high) ~ "Not Significant"
      )
    )
}


summon_rd_forest_plot <- function(data, time) {
  # Creating a forest plot of risk difference from the result of
  # summon_population_risk_difference_boot
  
  # Mask time
  time_point <- time
  
  data_plot <- data %>%
    dplyr::filter(time == time_point)
  
  # Forest plot
  plot <- data_plot %>%
    ggplot(aes(
      x = estimate,
      y = forcats::as_factor(term_tidy),
      xmin = conf.low,
      xmax = conf.high
    )) +
    geom_point() +
    geom_errorbarh() +
    geom_vline(xintercept = 0, linetype = "dotted") +
    theme_minimal() +
    xlab("Risk Difference (%) and 95% CI") +
    theme(
      # Title
      plot.title = element_text(size = 10),
      plot.subtitle = element_text(size = 8),
      # Axes
      axis.title.y = element_blank()
    )
  
  # RD values
  rd <- data_plot %>%
    ggplot() +
    geom_text(aes(
      x = 0,
      y = forcats::as_factor(term_tidy),
      label = conf.interval.tidy
    )) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(list(plot = plot, rd = rd))
}