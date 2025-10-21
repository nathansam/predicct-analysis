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


# Use age_decade for illustration

# Set softflare_time
# Set age_decade, which we will vary
data = flare.uc.df
model = cox
times = seq(from = 0, to = 730, by = 182.5)
variable = 'age_decade'
values = c(2.5,4.5,6.5)

summon_population_risk_difference(
  data = flare.uc.df,
  model = cox,
  times = times,
  variable = variable,
  values = values,
  ref_value = NULL
) %>%
  ggplot(aes(x = time, y = rd*100, colour = age_decade)) +
  geom_point() + 
  geom_line()


# Confidence intervals using bootstrapping

summon_population_risk_difference_boot(
  data = flare.uc.df,
  model = cox,
  times = times,
  variable = variable,
  values = values,
  ref_value = NULL,
  nboot = 9
) 



