
# HADS baseline analysis

# Run HADS Baseline.qmd prior to the analysis to get the data
# Called hads_for_analysis, rename to data_baseline
data_baseline <- hads_for_analysis

# Add the control scores
# data_baseline %<>%
#   dplyr::left_join(
#     IBD_C %>% dplyr::select(
#       ParticipantNo,
#       control_score,
#       OverallControl,
#       control_8,
#       control_grouped,
#       vas_control
#     ),
#     by = "ParticipantNo"
#   )

# Convert to factor
# data_baseline %<>% dplyr::mutate(
#   control_grouped = factor(control_grouped),
#   vas_control = factor(vas_control)
# )


# Survival data
# Run HADS survival analysis.qmd
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

# Data split into analysis groups
# Anxiety
data_baseline_anxiety <- data_baseline %>%
  dplyr::filter(hads_type == 'anxiety_hads')

# Depression
data_baseline_depression <- data_baseline %>%
  dplyr::filter(hads_type == 'depression_hads')

# Survival, soft, anxiety
data_survival_soft_anxiety <- data_survival_soft %>%
  dplyr::filter(hads_type == 'anxiety_hads')

# Survival, soft, depression
data_survival_soft_depression <- data_survival_soft %>%
  dplyr::filter(hads_type == 'depression_hads')

# Survival, hard, anxiety
data_survival_hard_anxiety <- data_survival_hard %>%
  dplyr::filter(hads_type == 'anxiety_hads')

# Survival, soft, depression
data_survival_hard_depression <- data_survival_hard %>%
  dplyr::filter(hads_type == 'depression_hads')



# Density plot for HADS score
ggplot(data_baseline, aes(x = hads_score, colour = hads_type)) +
  geom_line(stat = "count", aes(y = after_stat(count))) +
  labs(x = "HADS score",
       y = "Number of Participants") +
  scale_color_discrete(name = "HADS type",
                       labels = c("Anxiety", "Depression")) +
  theme_minimal()

# Cumulative sum plot?
data_baseline %>%
  dplyr::group_by(hads_type, hads_score) %>%
  dplyr::count() %>%
  dplyr::arrange(hads_type, hads_score) %>% 
  dplyr::group_by(hads_type) %>%
  dplyr::mutate(cumsum = cumsum(n)) %>% 
  ggplot() +
  geom_line(aes(x = hads_score, y = cumsum, colour = hads_type))


# Counts
data_baseline_anxiety %>%
  dplyr::group_by(score_group) %>%
  dplyr::count()

data_baseline_depression %>%
  dplyr::group_by(score_group) %>%
  dplyr::count()

# Anxiety
# Baseline plots
independent = c('diagnosis2',
                'AgeGroup',
                'Sex',
                'flare_group',
                'cat')

baseline_plots_anxiety <- summon_baseline_plots(
  data = data_baseline_anxiety, 
  dependent = 'score_group',
  independent = independent
  )

baseline_plots_anxiety$diagnosis2
baseline_plots_anxiety$AgeGroup
baseline_plots_anxiety$Sex
baseline_plots_anxiety$flare_group
baseline_plots_anxiety$cat

# Chi squared tests in a table
summon_chisq_test(data = data_baseline_anxiety, dependent = 'score_group', independent = independent)


# Survival analysis
# Soft
# Kaplan Meier
data_survival_soft_anxiety %>%
  survfit(Surv(time, DiseaseFlareYN) ~ score_group, data = .) %>%
  ggsurvplot(., data = data_survival_soft_anxiety, conf.int = TRUE, ggtheme = theme_minimal())

# Cox model
cox_soft_anxiety <- coxph(
  Surv(time, DiseaseFlareYN) ~
    score_group +
    IMD +
    Sex +
    AgeGroup +
    cat +
    frailty(SiteNo),
  data = data_survival_soft_anxiety
)

cox_soft_anxiety %>%
  broom::tidy(exponentiate = TRUE, conf.int = TRUE)


# Hard
# KM
data_survival_hard_anxiety %>%
  survfit(Surv(time, DiseaseFlareYN) ~ score_group, data = .) %>%
  ggsurvplot(., data = data_survival_hard_anxiety, conf.int = TRUE, ggtheme = theme_minimal())

# Cox
cox_hard_anxiety <- coxph(
  Surv(time, DiseaseFlareYN) ~
    score_group +
    IMD +
    Sex +
    AgeGroup +
    cat +
    frailty(SiteNo),
  data = data_survival_hard_anxiety
)

cox_hard_anxiety %>%
  broom::tidy(exponentiate = TRUE, conf.int = TRUE)



# Depression
# Baseline plots
independent = c('diagnosis2',
                'AgeGroup',
                'Sex',
                'flare_group',
                'cat')

baseline_plots_depression <- summon_baseline_plots(
  data = data_baseline_depression, 
  dependent = 'score_group',
  independent = independent
)

baseline_plots_depression$diagnosis2
baseline_plots_depression$AgeGroup
baseline_plots_depression$Sex
baseline_plots_depression$flare_group
baseline_plots_depression$cat

# Chi squared tests in a table
summon_chisq_test(data = data_baseline_depression, dependent = dependent, independent = independent)


# Survival analysis
# Soft
# Kaplan Meier
data_survival_soft_depression %>%
  survfit(Surv(time, DiseaseFlareYN) ~ score_group, data = .) %>%
  ggsurvplot(., data = data_survival_soft_depression, conf.int = TRUE, ggtheme = theme_minimal())


# Cox model
cox_soft_depression <- coxph(
  Surv(time, DiseaseFlareYN) ~
    score_group +
    IMD +
    Sex +
    AgeGroup +
    cat +
    frailty(SiteNo),
  data = data_survival_soft_depression
)

cox_soft_depression %>%
  broom::tidy(exponentiate = TRUE, conf.int = TRUE)

# Hard
# Kaplan Meier
data_survival_hard_depression %>%
  survfit(Surv(time, DiseaseFlareYN) ~ score_group, data = .) %>%
  ggsurvplot(., data = data_survival_hard_depression, conf.int = TRUE, ggtheme = theme_minimal())


# Cox model
cox_hard_depression <- coxph(
  Surv(time, DiseaseFlareYN) ~
    score_group +
    IMD +
    Sex +
    AgeGroup +
    cat +
    frailty(SiteNo),
  data = data_survival_hard_depression
)

cox_hard_depression %>%
  broom::tidy(exponentiate = TRUE, conf.int = TRUE)

