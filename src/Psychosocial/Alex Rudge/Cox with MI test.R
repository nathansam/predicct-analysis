library(tidyverse)
library(magrittr)
library(survival)
library(mice)

# Testing the Cox model with MI for FC missing values.

# Run HADS without removing missing FC data.

# Anxiety

# Produce imputed datasets
# Survival data so we want status and cumulative hazard in the imputation model

data_survival_anxiety_soft <- data_survival_anxiety_soft %>%
  dplyr::select(time, DiseaseFlareYN, score_group, Sex, AgeGroup, cat, SiteNo) %>%
  dplyr::mutate(cumhaz = mice::nelsonaalen(data = ., timevar = time, statusvar = DiseaseFlareYN))

# Predictor matrix - need to exclude time from the model
pred_matrix <- mice::make.predictorMatrix(data_survival_anxiety_soft)

pred_matrix[, 'time'] <- 0

data_survival_anxiety_soft_imputed <- data_survival_anxiety_soft %>% 
  mice::mice(m = 5, predictorMatrix = pred_matrix)

# Completed data
data_survival_anxiety_soft_all <- mice::complete(data_survival_anxiety_soft_imputed, action = 'all')


cox_anxiety_soft_results <- with(data_survival_anxiety_soft_imputed, coxph(
  Surv(time, DiseaseFlareYN) ~
    score_group +
    Sex +
    AgeGroup +
    cat +
    frailty(SiteNo))) %>%
  mice::pool() %>%
  summary(
    conf.int = TRUE,
    conf.level = 0.95,
    exponentiate = TRUE)
     
cox_anxiety_soft_results

