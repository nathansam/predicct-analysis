library(tidyverse)
library(magrittr)
library(survival)
library(splines)


# Version of Diet with FC as a continuous variable

# Load data
# Run first chunk of Diet.qmd

# FC has been logged twice - reverse
flare.uc.df$FC <- exp(flare.uc.df$FC)
# So now FC is the log of the FC measurement

# UPF

# Survival analysis

# Original
coxph(Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + UPF_perc + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.uc.df
) %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)


# New

# Remove IMD

# Add Age - in decades so coefficients are easier to interpret/scale is better
# Zero at age 40
flare.uc.df %<>%
  dplyr::mutate(
    age_decade = Age/10
  )

# DQI scaled


# Add BMIcat
# Add FC using a spline

cox <- coxph(Surv(softflare_time, softflare) ~
               Sex + 
               IMD +
               ns(age_decade, df = 2) + 
               ns(BMI, df = 2) +
               ns(FC, df = 2) +
               ns(UPF_perc, df = 2) + 
               dqi_tot +
               frailty(SiteNo),
             data = flare.uc.df
)

cox %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)

# Null model without the exposure for calculating overall p-value

cox_null <- coxph(Surv(softflare_time, softflare) ~
               Sex + 
               IMD +
               ns(age_decade, df = 2) + 
               ns(BMI, df = 2) +
               ns(FC, df = 2) +
               dqi_tot +
               frailty(SiteNo),
             data = flare.uc.df
)

cox_null %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)

# Likelihood ratio test
anova(cox, cox_null, test = 'LRT')

# Function to perform the LRT
summon_lrt <- function(model, term) {
  # Function that removes a term and performs a
  # Likelihood ratio test to determine its significance
  
  # Rename term to avoid problems
  term_removed <- term
  
  update(model, paste0("~ . - ", term_removed)) %>%
    anova(model, ., test = 'LRT') %>%
    broom::tidy() %>%
    dplyr::filter(!is.na(p.value)) %>%
    dplyr::mutate(term = term_removed) %>%
    dplyr::mutate(test = 'LRT') %>%
    dplyr::relocate(term) %>%
    dplyr::relocate(test, .after = term)
  
}

summon_lrt(cox, term = "ns(UPF_perc, df = 2)")





# Plotting the shape of age
# Dummy data
# Reference values

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
      denominator <- df %>% 
        dplyr::filter(!!sym(variable) == ref_value) %>%
        dplyr::pull(p)
    
      df %>% 
        dplyr::mutate(p = p - denominator)
    
    } %>%
    ggplot(aes(x = !!rlang::sym(variable), y = exp(p), ymin = exp(p - 1.96*se), ymax = exp(p + 1.96*se))) +
    geom_point() +
    geom_line() +
    geom_ribbon(alpha = 0.2) +
    ylab("HR")
  
}

# Age
plot_continuous_hr(
  data = flare.uc.df,
  model = cox,
  variable = 'age_decade'
)

# FC
plot_continuous_hr(
  data = flare.uc.df,
  model = cox,
  variable = 'FC'
)

# BMI
plot_continuous_hr(
  data = flare.uc.df,
  model = cox,
  variable = 'BMI'
)


