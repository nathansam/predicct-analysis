
# QOL analysis - Scores converted into a binary outcome

# Run QOL.qmd prior to the analysis to get the data
# Also run one of the other files to get the survival data

# Called QOL, rename to data_baseline
data_baseline <- QOL

# Convert scores to binary
data_baseline %<>% 
  mutate(PSC12_binary = ifelse(PSC12 >= 50, "50+", "<50"))

data_baseline %<>% 
  mutate(MCS12_binary = ifelse(MCS12 >= 42, "42+", "<42"))



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

# Our dependent variable is a binary flag indicating whether a patient has
# Above or below average PSC12 score or MCS12 score.

# Called PSC12_binary and MCS12_binary


# Counts
data_baseline %>%
  dplyr::group_by(PSC12_binary) %>%
  dplyr::count()

data_baseline %>%
  dplyr::group_by(MCS12_binary) %>%
  dplyr::count()

# Baseline plots

# PSC12

dependent = 'PSC12_binary'
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

# MCS12
dependent = 'MCS12_binary'
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
  survfit(Surv(time, DiseaseFlareYN) ~ PSC12_binary, data = .) %>%
  ggsurvplot(., data = data_survival_soft, conf.int = TRUE, ggtheme = theme_minimal())

data_survival_soft %>%
  survfit(Surv(time, DiseaseFlareYN) ~ MCS12_binary, data = .) %>%
  ggsurvplot(., data = data_survival_soft, conf.int = TRUE, ggtheme = theme_minimal())

# Cox model
cox_soft <- coxph(
  Surv(time, DiseaseFlareYN) ~
    PSC12_binary +
    MCS12_binary +
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
  survfit(Surv(time, DiseaseFlareYN) ~ PSC12_binary, data = .) %>%
  ggsurvplot(., data = data_survival_hard, conf.int = TRUE, ggtheme = theme_minimal())

data_survival_hard %>%
  survfit(Surv(time, DiseaseFlareYN) ~ MCS12_binary, data = .) %>%
  ggsurvplot(., data = data_survival_hard, conf.int = TRUE, ggtheme = theme_minimal())

# Cox
cox_hard <- coxph(
  Surv(time, DiseaseFlareYN) ~
    PSC12_binary +
    MCS12_binary +
    IMD +
    Sex +
    AgeGroup +
    cat +
    frailty(SiteNo),
  data = data_survival_hard
)

cox_hard %>%
  broom::tidy(exponentiate = TRUE, conf.int = TRUE)


# Counts
data_baseline %>%
  dplyr::group_by(MinimumExercise) %>%
  dplyr::count()

# Baseline plots
dependent = 'MinimumExercise'
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
  survfit(Surv(time, DiseaseFlareYN) ~ MinimumExercise, data = .) %>%
  ggsurvplot(., data = data_survival_soft, conf.int = TRUE, ggtheme = theme_minimal())

# Cox model
cox_soft <- coxph(
  Surv(time, DiseaseFlareYN) ~
    MinimumExercise +
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
  survfit(Surv(time, DiseaseFlareYN) ~ MinimumExercise, data = .) %>%
  ggsurvplot(., data = data_survival_hard, conf.int = TRUE, ggtheme = theme_minimal())

# Cox
cox_hard <- coxph(
  Surv(time, DiseaseFlareYN) ~
    MinimumExercise +
    IMD +
    Sex +
    AgeGroup +
    cat +
    frailty(SiteNo),
  data = data_survival_hard
)

cox_hard %>%
  broom::tidy(exponentiate = TRUE, conf.int = TRUE)