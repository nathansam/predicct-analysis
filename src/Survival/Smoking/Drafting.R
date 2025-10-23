library(tidyverse)
library(survival)
library(mice)

# Smoking included in the model using multiple imputation

# Run Diet to get data
# Using meat_sum as the example

# Reorder Smoke factor levels
flare.cd.df %<>%
  dplyr::mutate(
    Smoke = forcats::fct_relevel(Smoke, "Never", "Previous", "Current")
  )

flare.uc.df %<>%
  dplyr::mutate(
    Smoke = forcats::fct_relevel(Smoke, "Never", "Previous", "Current")
  )


# Missingness in smoking
# CD
flare.cd.df %>%
  dplyr::pull(Smoke) %>%
  forcats::fct_count(prop = TRUE)
# 28.5% missing

# UC
flare.uc.df %>%
  dplyr::pull(Smoke) %>%
  forcats::fct_count(prop = TRUE)
# 25.7%


# Impute smoking using mice
# CD

# Only select relevant columns for the imputation model
# Only imputing smoking so remove missing others
# Calculate cumulative hazard

# Soft flare
data_impute_soft <- flare.cd.df %>%
  dplyr::select(softflare_time,
                softflare,
                Sex,
                cat,
                IMD,
                dqi_tot,
                Meat_sum_cat,
                Smoke,
                SiteNo) %>%
  # Remove missing on all columns except smoke
  dplyr::filter(!dplyr::if_any(
    .cols = -Smoke,
    .fns = is.na
  )) %>%
  # Calculate Cumulative hazard
  dplyr::mutate(
    cumhaz = mice::nelsonaalen(
    data = .,
    timevar = softflare_time,
    statusvar = softflare
  ))

# Predictor matrix - need to exclude time from the model
pred_matrix <- mice::make.predictorMatrix(data_impute_soft)

pred_matrix[, 'softflare_time'] <- 0

# MICE with 5 imputations
mice_meat_cd_soft <- mice::mice(
  data = data_impute_soft,
  predictorMatrix = pred_matrix,
  m = 10,
  maxit = 20,
  seed = 73
)

# Check convergence
mice_meat_cd_soft %>% plot()

# Fit pooled Cox model
with(
  mice_meat_cd_soft,
  coxph(
    Surv(softflare_time, softflare) ~
      Sex +
      cat +
      IMD +
      dqi_tot +
      Meat_sum_cat +
      Smoke +
      frailty(SiteNo),
    data = flare.cd.df
  )
) %>%
  mice::pool() %>%
  summary(conf.int = TRUE,
          conf.level = 0.95,
          exponentiate = TRUE)

# Smoking is significant