library(tidyverse)
library(magrittr)
library(survival)
library(splines)


# Version of Diet with FC as a continuous variable

# Load data
# Run first chunk of Diet.qmd

# FC has been logged twice - reverse
flare.cd.df$FC <- exp(flare.cd.df$FC)
# So now FC is the log of the FC measurement

# Total meat protein
# Patient reported flare
# Crohn's

# Categorize meat protein by quantiles
flare.cd.df <- categorize_by_quantiles(flare.cd.df, "Meat_sum", reference_data = flare.df)

# Missingness
flare.cd.df %>%
  dplyr::filter(
    !is.na(Meat_sum_cat)) %>%
  dplyr::count(softflare)
# 530 patients with Meat sum data
# 187 events
# 2 with missing events - remove
flare.cd.df %<>%
  dplyr::filter(!is.na(softflare))

# Some exploration
# Relationship between dqi and meat
flare.cd.df %>%
  ggplot(aes(x = dqi_tot, y = Meat_sum)) +
  geom_point(alpha = 0.5) +
  geom_smooth()
# High dqi associated with lower total meat consumption.

# Statistical test
lm(dqi_tot ~ Meat_sum, data = flare.cd.df) %>% 
  broom::tidy()
# Very significant


# Survival analysis

# Original
coxph(Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + Meat_sum_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
) %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)


# New

# Remove IMD

# Add Age - in decades so coefficients are easier to interpret/scale is better
flare.cd.df %<>%
  dplyr::mutate(
    age_decade = Age/10
  )

# Add BMIcat
# Add FC using a spline

cox <- coxph(Surv(softflare_time, softflare) ~
               Sex + 
               ns(age_decade, df = 2) + 
               ns(BMI, df = 2) +
               ns(FC, df = 2) +
               Meat_sum_cat + 
               dqi_tot_scaled +
               frailty(SiteNo),
             data = flare.cd.df
)

cox %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)


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
    quantile(., probs = seq(0, 1, 0.01), na.rm = TRUE) %>%
    unname()
  
  # Reference variables
  # Extract variables used in the Cox model
  # Remove the variable we want to vary and the SiteNo
  ref_variables <- all.vars(delete.response(terms(cox))) %>%
    tibble(x = .) %>%
    dplyr::filter(
      x != variable,
      x != 'SiteNo'
    ) %>%
    dplyr::pull(x)
  
  
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
      pred = predict(cox, newdata = ., type = "lp", se = TRUE)
      
      tibble(., p = pred$fit, se = pred$se)
    } %>%
    ggplot(aes(x = !!rlang::sym(variable), y = exp(p), ymin = exp(p - 1.96*se), ymax = exp(p + 1.96*se))) +
    geom_point() +
    geom_line() +
    geom_ribbon(alpha = 0.2) +
    ylab("HR")
  
}

# Age
plot_continuous_hr(
  data = flare.cd.df,
  model = cox,
  variable = 'age_decade'
)

# FC
plot_continuous_hr(
  data = flare.cd.df,
  model = cox,
  variable = 'FC'
)

# BMI
plot_continuous_hr(
  data = flare.cd.df,
  model = cox,
  variable = 'BMI'
)


