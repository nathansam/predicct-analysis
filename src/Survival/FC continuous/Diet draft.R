library(tidyverse)
library(magrittr)
library(survival)
library(splines)


# Version of Diet with FC as a continuous variable

# Load data
# Run setting up in R in my version of Diet.qmd

# UPF

# Survival analysis

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


summon_lrt(cox, remove = "ns(UPF_perc, df = 2)")


# Plotting the shape of continuous variables
# Dummy data
# Reference values

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


# Absolute risk difference in the population, defined as
# Expected probability of event occurring if entire population was unexposed
# Minus the expected probability of event occurring if entire population was exposed

# For continuous variables, I guess we should choose some values to estimate at
# Ironically, I think I will probably choose quantiles


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
            dplyr::select(cum_incidence) %>% {
              df = .
              
              # Bootstrapping for confidence intervals
              nboot = 100
              
              1:nboot %>%
                purrr::map_dfr(
                  .f = function(x) {
                    df %>%
                      dplyr::slice_sample(prop = 1, replace = TRUE) %>%
                      dplyr::summarise(cum_incidence_mean_boot = mean(cum_incidence))
                  }
                )
            } %>%
            dplyr::summarise(
              cum_incidence_mean = mean(cum_incidence_mean_boot),
              conf.low = quantile(cum_incidence_mean_boot, 0.025),
              conf.high = quantile(cum_incidence_mean_boot, 0.975)
            ) %>%
            dplyr::mutate(time = time, !!sym(variable) := value)
        }
      )
    } %>%
    dplyr::mutate(!!sym(variable) := forcats::as_factor(!!sym(variable)))
  
}

# Use age_decade for illustration

# Set softflare_time
# Set age_decade, which we will vary
times = seq(from = 0, to = 730, by = 182.5)
variable = 'BMI'
values = quantile(flare.uc.df$BMI, probs = seq(0, 0.95, length.out = 5), na.rm = TRUE)

summon_population_cum_incidence(
  flare.uc.df,
  cox,
  times = times,
  variable = variable,
  values = values
) %>%
  ggplot(aes(x = time, y = cum_incidence_mean,
             ymin = conf.low, ymax = conf.high,
             colour = BMI, fill = BMI)) +
  geom_point() + 
  geom_line() +
  geom_ribbon(alpha = 0.2)
