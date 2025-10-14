library(tidyverse)
library(magrittr)
library(survival)


# Version of Diet with FC as a continuous variable

# Load data
# Run first chunk of Diet.qmd

# FC has already been logged - reverse
demo$FC <- exp(demo$FC)

# Total meat protein
# Patient reported flare
# Crohn's

# Categorize meat protein by quantiles
flare.cd.df <- categorize_by_quantiles(flare.cd.df, "Meat_sum", reference_data = flare.df)

# Missingness
flare.cd.df %>%
  dplyr::filter(
    !is.na(Meat_sum_cat))
# 520 patients with Meat sum data


# Original
coxph(Surv(softflare_time, softflare) ~
    Sex + cat + IMD + dqi_tot + Meat_sum_cat + frailty(SiteNo),
  control = coxph.control(outer.max = 20),
  data = flare.cd.df
) %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)

# New
# Remove IMD
# Add Age - in decades so coefficients interpretable.
flare.cd.df %<>%
  dplyr::mutate(
    age_decade = Age/10
  )

# DQI total - scale
flare.cd.df %<>%
  dplyr::mutate(
    dqi_tot_scaled = scale(dqi_tot, center = TRUE, scale = TRUE)
  )

# BMI category


# Cox with age as a spline
coxph(Surv(softflare_time, softflare) ~
        Sex + 
        pspline(age_decade, 2) + 
        BMIcat +
        cat + 
        #Meat_sum_cat + 
        dqi_tot_scaled +
        frailty(SiteNo),
      data = flare.cd.df
)

