library(tidyverse)
library(magrittr)
library(survival)
library(mice)
library(splines)


# Functions for the analysis

summon_statistical_test <- function(data, dependent, independent) {
  
  # Chi square tests/Fisher's exact test/Wilcox/Kruskal-Wallis
  # for the dependent variable vs each independent variable
  # Dependent variables must be discrete
  
  purrr::map2_df(
    .x = independent,
    .y = dependent,
    .f = function(.x, .y) {
      # Remove NA
      data %<>%
        dplyr::filter(!is.na(!!sym(.x)), !is.na(!!sym(.y)))
      
      x <- data %>%
        dplyr::pull(.x)
      
      y <- data %>%
        dplyr::pull(.y)
      
      # Check if x is continuous (numeric) or not
      continuous_flag <- is.numeric(x)
      
      # If continuous, use a Wilcox/Mann Whitney if y has 2 levels
      # or Kruskal Wallis if >2
      n_levels_y <- levels(y) %>% length()
      
      if (continuous_flag & (n_levels_y <= 2)) {
        # Wilcoxon
        
        # Formula as score ~ group
        test_formula <- as.formula(paste0(.x, " ~ ", .y))
        
        wilcox_test(data, formula = test_formula) %>%
          # Calculate n for consistency with kruskal_test
          dplyr::mutate(n = n1 + n2) %>%
          # Don't need the adjusted p values as we adjust ourselves in a second
          dplyr::select(-tidyselect::any_of(c('.y.', 'group1', 'group2', 'n1', 'n2', 'p.adj', 'p.adj.signif'))) %>%
          dplyr::mutate(x = .x,
                        y = .y,
                        method = 'Wilcox test')
        
      } else if (continuous_flag & (n_levels_y > 2)) {
        
        # Formula as score ~ group
        test_formula <- as.formula(paste0(.x, " ~ ", .y))
        
        kruskal_test(data, formula = test_formula) %>%
          dplyr::select(-.y.) %>%
          dplyr::mutate(x = .x, y = .y)
        
      } else {
        # Otherwise use categorical tests
        
        # Check whether we need a Fisher test
        fisher_flag <- data %>%
          # Count combinations of x and y
          dplyr::count(!!rlang::sym(.x), !!rlang::sym(.y)) %>%
          # Pull the count
          dplyr::pull(n) %>%
          # Check if any counts are < 5
          min() < 5
        
        if (fisher_flag) {
          # If one of the counts is < 5 use Fisher exact test
          fisher_test(table(x, y), simulate.p.value = TRUE, B = 1e5) %>%
            dplyr::mutate(x = .x,
                          y = .y,
                          method = 'Fisher\'s exact test')
          
        } else {
          # Otherwise use chi square
          chisq_test(x = x, y = y) %>%
            dplyr::mutate(x = .x, y = .y)
        }
      }
    }
  ) %>%
    dplyr::mutate(p.adjust = p.adjust(p = p, method = 'holm')) %>%
    dplyr::mutate(p.adjust = signif(p.adjust, 3)) %>%
    dplyr::select(-tidyselect::any_of(c('p.signif'))) %>%
    dplyr::mutate(
      p.adjust.signif = dplyr::case_when(
        p.adjust < 0.0001 ~ "****",
        p.adjust < 0.001 ~ "***",
        p.adjust < 0.01  ~ "**",
        p.adjust < 0.05  ~ "*",
        .default = 'ns'
      )
    ) %>%
    # Reorder columns
    dplyr::select(
      y, x, n, statistic, df, p, p.adjust, p.adjust.signif, method
    )
  
}

# Function to create plots for baseline associations

summon_baseline_plot_discrete <- function(data, stat_tests, dependent, independent) {
  # Creating plot for baseline variables with the dependent variable
  # Discrete variables
  
  # Stat_tests is a dataframe from summon_statistical_test to get p-value info
  
  label <- stat_tests %>%
    dplyr::filter(x == independent) %>%
    {df <- .
    method <- df %>% dplyr::pull(method)
    p <- df %>% dplyr::pull(p.adjust)
    
    paste0(stringr::str_pad(method, 25, 'right'), "\n", "Adjusted p-value: ", p)
    
    }

  
  data %>%
    dplyr::filter(!is.na(!!sym(dependent)), !is.na(!!sym(independent))) %>%
    dplyr::group_by(!!rlang::sym(independent), !!rlang::sym(dependent)) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!rlang::sym(independent)) %>%
    dplyr::mutate(p = n/sum(n)) %>%
    dplyr::ungroup() %>%
    ggplot(aes(x = !!sym(dependent), y = p, fill = !!sym(independent))) +
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


summon_baseline_plot_continuous <- function(data, stat_tests, dependent, independent) {
  # Creating plot for baseline variables with the dependent variable
  # Continuous independent variable
  
  # Stat_tests is a dataframe from summon_statistical_test to get p-value info
  label <- stat_tests %>%
    dplyr::filter(x == independent) %>%
    {df <- .
    method <- df %>% dplyr::pull(method)
    p <- df %>% dplyr::pull(p.adjust)
    
    paste0(stringr::str_pad(method, 25, 'right'), "\n", "Adjusted p-value: ", p)
    
    }
  
  # Plot
  data %>%
    dplyr::filter(
      !is.na(!!sym(dependent)),
      !is.na(!!sym(independent))
    ) %>%
    ggplot(aes(x = !!sym(independent), colour = !!sym(dependent))) +
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
  
}


# Function to perform coxph with mice for missing data
coxph_mice <- function(formula, data, ...){
  
  # Variables
  variables <- all.vars(formula)
  
  # Extract time and status variable names
  timevar <- variables[1]
  statusvar <- variables[2]
  
  # Select variables in the data
  data %<>%
    dplyr::select(tidyselect::all_of(variables)) %>%
    # Calculate the cumulative hazard
    dplyr::mutate(cumhaz = mice::nelsonaalen(
      data = .,
      timevar = !!sym(timevar),
      statusvar = !!sym(statusvar)
    ))
  
  # Predictor matrix - need to exclude time from the model
  pred_matrix <- mice::make.predictorMatrix(data)
  
  pred_matrix[, timevar] <- 0
  
  data_imp <- mice::mice(
    data = data,
    predictorMatrix = pred_matrix, ...)
  
  # Return the imputation
  return(data_imp)
  
}


# Extract levels from the data
extract_cox_results <- function(data,
                                cox_model,
                                variable,
                                flare_type,
                                diagnosis2) {
  data %>%
    dplyr::pull(variable) %>%
    levels() %>%
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
    dplyr::mutate(diagnosis2 = diagnosis2, flare_type = flare_type) %>%
    dplyr::select(
      tidyselect::any_of(
        c('term', 'variable', 'level', 'estimate', 'std.error', 'statistic', 'df', 'p.value', 'conf.low', 'conf.high', 'diagnosis2', 'flare_type')))
}

# Creating KM curves
summon_km_curves <- function(data,
                             dependent = 1,
                             title = NULL,
                             legend.title = NULL,
                             legend.labs = NULL,
                             palette = NULL) {
  
  # ggsurvplot
  plot_surv <- data %>%
    survfit(as.formula(paste0("Surv(time, DiseaseFlareYN) ~ ", dependent)), 
            data = .) %>%
    ggsurvplot(
      .,
      data = data,
      conf.int = TRUE,
      risk.table = TRUE,
      pval = TRUE,
      pval.method = TRUE,
      title = title,
      legend.title = legend.title,
      legend.labs = legend.labs,
      xlab = "Time from study recruitment (days)",
      palette = palette,
      ggtheme = theme_minimal()
    )
  
  # Customise the plot and table separately
  
  plot_surv$plot <- plot_surv$plot +
    theme(
      plot.title = element_text(hjust = 0.5))
  
  plot_surv$table <- plot_surv$table + ylab("")
  
  # Recombine using patchwork
  plot_surv$plot / (plot_surv$table + ylab("")) +
    patchwork::plot_layout(
      ncol = 1,
      heights = c(3, 1)
      ) &
    theme(plot.margin = margin(0, 0, 0, 0))
  
}


plot_continuous_hr <- function(data, model, variable, splineterm = NULL){
  
  # Plotting continuous Hazard Ratio curves
  # Data is the same data used to fit the Cox model
  # model is a Cox model
  # variable is the variable we are plotting
  # splineterm is the spline used in the model, if there is one
  
  
  # Range of the variable to plot
  variable_range <- data %>%
    dplyr::pull(variable) %>%
    quantile(., probs = seq(0, 0.9, 0.01), na.rm = TRUE) %>%
    unname() %>%
    unique()
  
  # If there is a spline
  if (!is.null(splineterm)){
    
    # Recreate the spline
    spline <- with(data, eval(parse(text = splineterm)))
    
    # Get the spline terms for the variable values we are plotting
    spline_values <- predict(spline, newx = variable_range)
    
    # Get the reference values of the variables that were used to fit the Cox model
    X_ref <- model$means
    
    # New values to calculate the HR at 
    # Take the ref values and vary our variable of interest
    
    # Number of values
    n_values <- variable_range %>% length()
    
    X_ref <- matrix(rep(X_ref, n_values), nrow = n_values, byrow = TRUE)
    # Fix the column names
    colnames(X_ref) <- names(model$means)
    
    # Now add our new variable values
    X_new <- X_ref
    # Identify the correct columns
    column_pos <- stringr::str_detect(colnames(X_new), variable)
    # Add the values - ordering of columns and model term names should be preserved
    X_new[,column_pos] <- spline_values
    
    # Calculate the linear predictor and se
    # Method directly from predict.coxph from the survival package
    lp <- (X_new - X_ref)%*%coef(model) 
    
    lp_se <- rowSums((X_new - X_ref)%*%vcov(model)*(X_new - X_ref)) %>%
      as.numeric() %>%
      sqrt()
    
  } else {
    # If there is no spline then it is easier
    
    # Get the reference values of the variables that were used to fit the Cox model
    X_ref <- model$means
    
    # New values to calculate the HR at 
    # Take the ref values and vary our variable of interest
    
    # Number of values
    n_values <- variable_range %>% length()
    
    X_ref <- matrix(rep(X_ref, n_values), nrow = n_values, byrow = TRUE)
    # Fix the column names
    colnames(X_ref) <- names(model$means)
    
    # Now add our new variable values
    X_new <- X_ref
    
    # Add the values
    X_new[,variable] <- variable_range
    
    # Calculate the linear predictor and se
    # Method directly from predict.coxph from the survival package
    lp <- (X_new - X_ref)%*%coef(model) %>% as.numeric() 
    
    lp_se <- rowSums((X_new - X_ref)%*%vcov(model)*(X_new - X_ref)) %>% 
      as.numeric() %>%
      sqrt()
    
  }
  
  # Store results in a tibble
  data_pred <- tibble(
    !!sym(variable) := variable_range,
    p = lp,
    se = lp_se
  )
  
  # Plot
  data_pred %>%
    ggplot(aes(
      x = !!rlang::sym(variable),
      y = exp(p),
      ymin = exp(p - 1.96 * se),
      ymax = exp(p + 1.96 * se)
    )) +
    geom_point() +
    geom_line() +
    geom_ribbon(alpha = 0.2) +
    ylab("HR") + 
    theme_minimal()
  
}

# Function to perform an LRT
summon_lrt <- function(model, remove = NULL, add = NULL) {
  # Function that removes a term and performs a
  # Likelihood ratio test to determine its significance
  
  new_formula <- "~ ."
  
  if (!is.null(remove)) {
    new_formula <- paste0(new_formula, " - ", remove)
  } else {
    remove <- NA
  }
  
  if (!is.null(add)) {
    new_formula <- paste0(new_formula, " + ", add)
  } else {
    add <- NA
  }
  
  update(model, new_formula) %>%
    anova(model, ., test = "LRT") %>%
    broom::tidy() %>%
    dplyr::filter(!is.na(p.value)) %>%
    dplyr::select(-term) %>%
    dplyr::mutate(
      removed = remove,
      added = add
    ) %>%
    dplyr::mutate(test = "LRT") %>%
    dplyr::select(test, removed, added, tidyselect::everything())
}


summon_population_risk_difference <- function(data,
                                              model,
                                              times,
                                              variable,
                                              values,
                                              ref_value = NULL) {
  # Function that calculates (population average) risk difference of a variable
  # at given time points with respect to specified value of the variable from a
  # Cox model
  
  # If no reference value then use one in the middle
  if (is.null(ref_value)) {
    pos <- (length(values) / 2) %>% ceiling()
    ref_value <- values %>% purrr::pluck(pos)
  }
  
  # Identify the time variable - to differentiate between soft and hard flare models
  time_variable <- all.vars(terms(model))[1]
  
  df <- tidyr::expand_grid(times, values)
  
  purrr::map2(
    .x = df$times,
    .y = df$values,
    .f = function(x, y) {
      data %>%
        # Set values for time and the variable
        dplyr::mutate(!!sym(time_variable) := x, !!sym(variable) := y) %>%
        # Estimate expected for entire population
        predict(model, newdata = ., type = "expected") %>%
        tibble(expected = .) %>%
        # Remove NA
        dplyr::filter(!is.na(expected)) %>%
        # Calculate survival and cumulative incidence (1 - survival)
        dplyr::mutate(
          survival = exp(-expected),
          cum_incidence = 1 - survival
        ) %>%
        dplyr::select(cum_incidence) %>%
        dplyr::summarise(cum_incidence = mean(cum_incidence)) %>%
        # Note time, value and variable
        dplyr::mutate(time = x, value = y, variable = variable)
    }
  ) %>%
    purrr::list_rbind() %>%
    # Calculate risk difference relative to a reference
    {
      df <- .
      
      # Subtract chosen reference value to set linear predictor to 0 at that value
      cum_incidence_ref <- df %>%
        dplyr::filter(value == ref_value) %>%
        dplyr::rename(cum_incidence_ref = cum_incidence) %>%
        dplyr::select(time, cum_incidence_ref)
      
      df %>%
        dplyr::left_join(cum_incidence_ref, by = "time") %>%
        dplyr::mutate(rd = (cum_incidence - cum_incidence_ref)*100, .keep = "unused")
    }
}


summon_population_risk_difference_boot <- function(data,
                                                   model,
                                                   times,
                                                   variable,
                                                   values,
                                                   ref_value = NULL,
                                                   nboot = 99,
                                                   seed = 1) {
  # Function to calculate population risk difference for a given variable at given time points
  # relative to a specified reference value
  # Confidence intervals calculated using bootstrapping.
  
  # Set seed
  set.seed(seed)
  
  # If no reference value then use one in the middle
  if (is.null(ref_value)) {
    pos <- (length(values) / 2) %>% ceiling()
    ref_value <- values %>% purrr::pluck(pos)
  }
  
  purrr::map_dfr(
    .x = seq_len(nboot),
    .f = function(b) {
      # Variable used in the model
      all_variables <- all.vars(terms(model))
      
      # Bootstrap sample of the data
      data_boot <- data %>%
        # Remove any NAs as Cox doesn't use these
        dplyr::select(tidyselect::all_of(all_variables)) %>%
        dplyr::filter(!dplyr::if_any(.cols = everything(), .fns = is.na)) %>%
        # Sample the df
        dplyr::slice_sample(prop = 1, replace = TRUE)
      
      # Refit cox model of bootstrapped data
      model_boot <- coxph(formula(model), data = data_boot, model = TRUE)
      
      # Calculate risk differences
      summon_population_risk_difference(
        data = data_boot,
        model = model_boot,
        times = times,
        variable = variable,
        values = values,
        ref_value = ref_value
      )
    }
  ) %>%
    dplyr::group_by(time, value) %>%
    # Bootstrapped estimate and confidence intervals
    dplyr::summarise(
      mean_rd = mean(rd),
      conf.low = quantile(rd, prob = 0.025),
      conf.high = quantile(rd, prob = 0.975)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(estimate = mean_rd) %>%
    # Note variable, reference level flag
    dplyr::mutate(
      variable = variable,
      reference_flag = (value == ref_value)
    ) %>%
    # Ordering for plotting
    dplyr::arrange(time, value) %>%
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