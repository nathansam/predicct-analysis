library(tidyverse)
library(magrittr)
library(survival)


# Function to perform the LRT
summon_lrt <- function(model, remove) {
  # Function that removes a term and performs a
  # Likelihood ratio test to determine its significance
  
  update(model, paste0("~ . - ", remove)) %>%
    anova(model, ., test = 'LRT') %>%
    broom::tidy() %>%
    dplyr::filter(!is.na(p.value)) %>%
    dplyr::mutate(term = remove) %>%
    dplyr::mutate(test = 'LRT') %>%
    dplyr::relocate(term) %>%
    dplyr::relocate(test, .after = term)
  
}

summon_reference_values <- function(data, variables){
  # Function that extracts the reference value for each variable in a data frame
  # Reference is either the first factor level or the median
  # Used for producing HR plots
  
  data %>%
    dplyr::select(tidyselect::all_of(variables)) %>%
    dplyr::summarise(across(
      .cols = everything(),
      .fns = function(x) {
        if (is.factor(x)) {
          levels(x) %>% dplyr::first()
        } else if (is.numeric(x)) {
          median(x, na.rm = TRUE)
        } else {
          NULL
        }
      }
    ))
  
}

plot_continuous_hr <- function(data, model, variable){
  # Plotting continuous Hazard Ratio curves
  
  # model is a Cox model
  # variable is the variable we are plotting
  
  # Range of the variable to plot
  variable_range <- data %>%
    dplyr::pull(variable) %>%
    quantile(., probs = seq(0, 0.9, 0.01), na.rm = TRUE) %>%
    unname() %>%
    unique()
  
  # Reference variables
  # Extract variables used in the Cox model
  # Remove the variable we want to vary and the SiteNo
  ref_variables <- all.vars(delete.response(terms(model))) %>%
    tibble(x = .) %>%
    dplyr::filter(
      x != variable,
      x != 'SiteNo'
    ) %>%
    dplyr::pull(x)
  
  # Reference level of the continuous variable
  # Set to the lowest
  ref_value <- variable_range %>% min()
  
  # Create dummy data for plotting
  # Reference levels
  data %>%
    summon_reference_values(
      data = .,
      variables = ref_variables
    ) %>% {
      # Creating dummy data
      tibble(
        .,
        !!sym(variable) := variable_range
      )
    } %>% {
      pred = predict(model, newdata = ., type = "lp", se = TRUE)
      
      tibble(., p = pred$fit, se = pred$se)
    } %>% {
      df <- .
      
      # Subtract chosen reference value to set linear predictor to 0 at that value
      p_ref <- df %>% 
        dplyr::filter(!!sym(variable) == ref_value) %>%
        dplyr::pull(p)
      
      df %>% 
        dplyr::mutate(p = p - p_ref)
      
    } %>%
    ggplot(aes(x = !!rlang::sym(variable), y = exp(p), ymin = exp(p - 1.96*se), ymax = exp(p + 1.96*se))) +
    geom_point() +
    geom_line() +
    geom_ribbon(alpha = 0.2) +
    ylab("HR")
  
}


summon_population_cum_incidence <- function(data, model, times, variable, values) {
  # Function that calculates mean population cumulative incidence at a given time
  # with a specified value of the variable
  # using a Cox model
  
  tidyr::expand_grid(times, values) %>%
    {
      purrr::map2_dfr(
        .x = .$times,
        .y = .$values,
        .f = function(time, value) {
          data %>%
            dplyr::mutate(softflare_time = time, !!sym(variable) := value) %>%
            # Estimate expected for entire population
            predict(model, newdata = ., type = 'expected') %>%
            tibble(expected = .) %>%
            # Remove NA
            dplyr::filter(!is.na(expected)) %>%
            # Calculate survival and cumulative incidence (1 - survival)
            dplyr::mutate(survival = exp(-expected),
                          cum_incidence = 1 - survival) %>%
            dplyr::select(cum_incidence) %>% 
            dplyr::summarise(
              cum_incidence = mean(cum_incidence)
            ) %>%
            dplyr::mutate(time = time, !!sym(variable) := value)
        }
      )
    } %>%
    dplyr::mutate(!!sym(variable) := forcats::as_factor(!!sym(variable)))
  
}



summon_population_risk_difference <- function(data, model, times, variable, values, ref_value = NULL) {
  # Function that calculates mean population cumulative incidence at a given time
  # with a specified value of the variable
  # using a Cox model
  
  # If no reference value then use lowest
  if (is.null(ref_value)) {ref_value <- values %>% min()}
  
  df <- tidyr::expand_grid(times, values) 
      
  purrr::map2(
    .x = df$times,
    .y = df$values,
    .f = function(x, y) {
      
      data %>%
        # Set values for time and the variable
        dplyr::mutate(softflare_time = x, !!sym(variable) := y) %>%
        # Estimate expected for entire population{
        predict(model, newdata = ., type = 'expected') %>%
        tibble(expected = .) %>%
        # Remove NA
        dplyr::filter(!is.na(expected)) %>%
        # Calculate survival and cumulative incidence (1 - survival)
        dplyr::mutate(survival = exp(-expected),
                      cum_incidence = 1 - survival) %>%
        dplyr::select(cum_incidence) %>%
        dplyr::summarise(cum_incidence = mean(cum_incidence)) %>%
        dplyr::mutate(time = x, !!sym(variable) := y)
    }
  ) %>%
    purrr::list_rbind() %>%
    dplyr::mutate(!!sym(variable) := forcats::as_factor(!!sym(variable))) %>%
    # Calculate risk difference relative to a reference
    {
      df <- .
      
      # Subtract chosen reference value to set linear predictor to 0 at that value
      cum_incidence_ref <- df %>% 
        dplyr::filter(!!sym(variable) == ref_value) %>%
        dplyr::rename(cum_incidence_ref = cum_incidence) %>%
        dplyr::select(time, cum_incidence_ref)
      
      df %>% 
        dplyr::left_join(cum_incidence_ref, by = 'time') %>%
        dplyr::mutate(rd = cum_incidence - cum_incidence_ref, .keep = "unused")
      
    } 
}


summon_population_risk_difference_boot <- function(data,
                                                   model,
                                                   times,
                                                   variable,
                                                   values,
                                                   ref_value = NULL,
                                                   nboot = 99) {
  
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
    dplyr::group_by(time, !!sym(variable)) %>%
    # Bootstrapped estimate and confidence intervals
    dplyr::summarise(
      mean_rd = mean(rd),
      conf.low = quantile(rd, prob = 0.025),
      conf.high = quantile(rd, prob = 0.975)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(rd = mean_rd)
  
}
