library(tidyverse)
library(magrittr)
library(survival)


# Correlation between control-8 score and vas score.

# Run HADS data cleaning

data_baseline_anxiety %>%
  ggplot(aes(x = OverallControl, y = control_8)) +
  geom_point() +
  geom_smooth()

# Pearson's correlation
data_baseline_anxiety %$%
  cor(x = OverallControl, y = control_8, use = "complete.obs", method = 'pearson')

# Correlation with FC
data_baseline_anxiety %>%
  ggplot(aes(x = FC, y = control_8)) +
  geom_point() +
  geom_smooth()

data_baseline_anxiety %$%
  cor(x = FC, y = control_8, use = "pairwise.complete.obs", method = 'pearson')
# Apparently none.

# Correlation with anxiety score (continuous)
data_baseline_anxiety %>%
  ggplot(aes(x = hads_score, y = control_8)) +
  geom_point() +
  geom_smooth()

data_baseline_anxiety %$%
  cor(x = hads_score, y = control_8, use = "pairwise.complete.obs", method = 'pearson')
# -0.4

data_baseline_anxiety %>%
  ggplot(aes(x = hads_score, y = OverallControl)) +
  geom_point() +
  geom_smooth()

data_baseline_anxiety %$%
  cor(x = hads_score, y = OverallControl, use = "pairwise.complete.obs", method = 'pearson')

# Shape of control 8 score
# soft uc

cox_control_soft <- coxph(
  Surv(time, DiseaseFlareYN) ~
    control_8 +
    frailty(SiteNo),
  data = data_survival_anxiety_soft_uc
)

# Spline for control
summon_lrt(cox_control_soft, remove = 'control_8', add = 'ns(control_8, df = 2)')
# Yes

cox_control_soft <- coxph(
  Surv(time, DiseaseFlareYN) ~
    ns(control_8, df = 2) +
    frailty(SiteNo),
  data = data_survival_anxiety_soft_uc
)

summon_lrt(cox_control_soft, remove = 'ns(control_8, df = 2)', add = 'ns(control_8, df = 3)')
# No

plot_continuous_hr(
  data = data_survival_anxiety_soft_uc, 
  model = cox_control_soft, 
  variable = 'control_8',
  splineterm = 'ns(control_8, df = 2)')


