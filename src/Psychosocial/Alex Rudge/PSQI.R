
# PSQI - Sleep Quality Index

# Run psqi.qmd prior to the analysis to get the data
# Called psqi2, rename to data_baseline
data_baseline <- psqi2


# Survival data
# Hard flares
data_survival_hard <- data_baseline %>%
  dplyr::inner_join(
    flares_hard %>% dplyr::select(ParticipantNo, hardflare, hardflare_time),
    by = "ParticipantNo"
  ) %>%
  dplyr::mutate(DiseaseFlareYN = hardflare, time = hardflare_time)

# Soft flares
data_survival_soft <- data_baseline %>%
  dplyr::inner_join(
    flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(DiseaseFlareYN = softflare, time = softflare_time)

# Our dependent variable is a categorical variable representing patients who
# experience sleep disturbance (PSQI score > 5)

# Called SleepDisturbance


# Counts
data_baseline %>%
  dplyr::group_by(SleepDisturbance) %>%
  dplyr::count()

# Baseline plots
dependent = 'SleepDisturbance'
independent = c('diagnosis2', 'AgeGroup', 'Sex', 'flare_group', 'cat')

baseline_plots <- summon_baseline_plots(
  data = data_baseline, 
  dependent = dependent,
  independent = independent)

baseline_plots$diagnosis2
baseline_plots$AgeGroup
baseline_plots$Sex
baseline_plots$flare_group
baseline_plots$cat

# Chi squared tests in a table
summon_chisq_test(data = data_baseline, dependent = dependent, independent = independent)


# Survival analysis
# Soft
# Kaplan Meier
data_survival_soft %>%
  survfit(Surv(time, DiseaseFlareYN) ~ SleepDisturbance, data = .) %>%
  ggsurvplot(., data = data_survival_soft, conf.int = TRUE, ggtheme = theme_minimal())

# Cox model
cox_soft <- coxph(
  Surv(time, DiseaseFlareYN) ~
    SleepDisturbance +
    IMD +
    Sex +
    AgeGroup +
    cat +
    frailty(SiteNo),
  data = data_survival_soft
)

cox_soft %>%
  broom::tidy(exponentiate = TRUE, conf.int = TRUE)


# Hard
# KM
data_survival_hard %>%
  survfit(Surv(time, DiseaseFlareYN) ~ SleepDisturbance, data = .) %>%
  ggsurvplot(., data = data_survival_hard, conf.int = TRUE, ggtheme = theme_minimal())

# Cox
cox_hard <- coxph(
  Surv(time, DiseaseFlareYN) ~
    SleepDisturbance +
    IMD +
    Sex +
    AgeGroup +
    cat +
    frailty(SiteNo),
  data = data_survival_hard
)

cox_hard %>%
  broom::tidy(exponentiate = TRUE, conf.int = TRUE)
