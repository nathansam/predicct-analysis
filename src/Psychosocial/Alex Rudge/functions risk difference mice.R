
pool_cum_incidence <- function(model, newdata){
  
  # Estimate expected for entire population
  expected <- predict(model, newdata = newdata, type = "expected")
  
  # Calculate mean cumulative incidence (1 - survival)
  cum_incidence <- (1 - exp(-expected)) %>% mean(na.rm = TRUE)
  
  return(cum_incidence)
  
}


summon_risk_difference_factor_mice <- function(data, model, time, variable, ref_level = NULL) {
  # Function that calculates (population average) risk difference of a variable
  # at a given time point with respect to specified value of the variable from a
  # Cox model
  
  # Data - original data
  # model - original model
  
  # Values of the variable
  values <- data %>%
    dplyr::pull(variable) %>%
    levels()
  
  # If no reference level then use the first factor level
  if (is.null(ref_level)) {
    ref_level <- values %>%
      dplyr::first()
  }
  
  # Identify the time variable
  time_variable <- all.vars(terms(model))[1]
  
  # Fit cox model using mice
  model_mice <- coxph_mice(formula(model), data = data)
  
  data_imputed <- mice::complete(model_mice, action = 'all', include = FALSE)
  
  model_imputed <- purrr::map(
    .x = data_imputed,
    .f = function(x){
      coxph(formula(model), data = x, model = TRUE)
    }
  )
  
  results <- purrr::map(
    .x = values,
    .f = function(x) {
      t <- time
      
      # Set the values for time and the variable on the imputed datasets
      newdata <- data_imputed %>%
        purrr::map(
          .f = function(df){
            df %>%
              dplyr::mutate(!!sym(time_variable) := t, !!sym(variable) := x)
            
          }
        )
        
      # Pooled cumulative incidence
      cum_incidence_pooled <- purrr::map2(
        .x = model_imputed, 
        .y = newdata,
        .f = function(model, newdata){
          
          # Estimate expected for entire population
          expected <- predict(model, newdata = newdata, type = "expected")
          
          # Calculate mean cumulative incidence (1 - survival)
          cum_incidence <- (1 - exp(-expected)) %>% mean(na.rm = TRUE)
          
        }
      ) %>% 
        purrr::list_c() %>%
        mean()
      
      # Return as a tibble row
      tibble(
        time = t,
        variable = variable,
        level = x,
        cum_incidence = cum_incidence_pooled
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



summon_risk_difference_factor_boot_mice <- function(data,
                                                    model,
                                               time = 365,
                                               variable,
                                               ref_level = NULL,
                                               nboot = 99,
                                               seed = 1,
                                               ...) {
  
  
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
    # Select variables
    dplyr::select(tidyselect::all_of(all_variables))
  
  # Refit the original cox model with model = TRUE
  model <- coxph(formula(model), data = data, model = TRUE)
  
  purrr::map_dfr(
    .x = seq_len(nboot),
    .f = function(b) {
      
      data_boot <- data %>%
        # Sample the df
        dplyr::slice_sample(prop = 1, replace = TRUE)
      
      # Calculate risk differences
      summon_risk_difference_factor_mice(
        data = data_boot,
        model = model,
        time = time,
        variable = variable,
        ref_level = ref_level
      )
    }
  ) %>%
    dplyr::group_by(time, level) %>%
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
