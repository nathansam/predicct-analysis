
# HADS baseline analysis

# Run HADS Baseline.qmd prior to the analysis to get the data

# Add the control scores
hads_for_analysis %<>%
  dplyr::left_join(
    IBD_C %>% dplyr::select(
      ParticipantNo,
      control_score,
      OverallControl,
      control_8,
      control_grouped,
      vas_control
    ),
    by = "ParticipantNo"
  )

# Convert to factor
hads_for_analysis %<>% dplyr::mutate(
  control_grouped = factor(control_grouped),
  vas_control = factor(vas_control)
)

# Data is called hads_for_analysis
# also contains control scores for whom some will have NA, but they will
# automatically get filtered out by survival methods.


# Survival data
# Hard flares
data_survival_hard <- hads_for_analysis %>%
  dplyr::inner_join(
    flares_hard %>% dplyr::select(ParticipantNo, hardflare, hardflare_time),
    by = "ParticipantNo"
  ) %>%
  dplyr::mutate(DiseaseFlareYN = hardflare, time = hardflare_time)

# Soft flares
data_survival_soft <- hads_for_analysis %>%
  dplyr::inner_join(
    flares_soft %>% dplyr::select(ParticipantNo, softflare, softflare_time),
    by = 'ParticipantNo'
  ) %>%
  dplyr::mutate(DiseaseFlareYN = softflare, time = softflare_time)



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


# Data split into analysis groups
# Anxiety, full cohort
data_baseline_anxiety <- hads_for_analysis %>%
  dplyr::filter(hads_type == 'anxiety_hads')

# Depression, full cohort
data_baseline_depression <- hads_for_analysis %>%
  dplyr::filter(hads_type == 'depression_hads')

# Survival, soft anxiety
data_survival_soft_anxiety <- data_survival_soft %>%
  dplyr::filter(hads_type == 'anxiety_hads')

# Survival, soft depression
data_survival_soft_depression <- data_survival_soft %>%
  dplyr::filter(hads_type == 'depression_hads')



# Baseline plots
# Anxiety
baseline_plots_anxiety <- summon_baseline_plots(data = data_baseline_anxiety, dependent = 'score_group')

baseline_plots_anxiety$diagnosis2
baseline_plots_anxiety$AgeGroup
baseline_plots_anxiety$Sex
baseline_plots_anxiety$flare_group
baseline_plots_anxiety$cat

# Depression
baseline_plots_depression <- summon_baseline_plots(data = data_baseline_depression, dependent = 'score_group')

baseline_plots_depression$diagnosis2
baseline_plots_depression$AgeGroup
baseline_plots_depression$Sex
baseline_plots_depression$flare_group
baseline_plots_depression$cat



# Survival analysis ####

# Kaplan Meier
# Soft
data_survival_soft_anxiety %>%
  survfit(Surv(time, DiseaseFlareYN) ~ score_group, data = .) %>%
  ggsurvplot(., data = data_survival_soft, conf.int = TRUE, ggtheme = theme_minimal())

# Hard
data_survival_hard %>%
  dplyr::filter(hads_type == 'anxiety_hads') %>%
  survfit(Surv(time, DiseaseFlareYN) ~ score_group, data = .) %>%
  ggsurvplot(., data = data_survival_hard, conf.int = TRUE, ggtheme = theme_minimal())


# Cox model
# Soft
# Anxiety
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

# Depression
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
# Anxiety
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