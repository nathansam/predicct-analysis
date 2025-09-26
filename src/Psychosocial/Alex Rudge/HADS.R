
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
hads_control %<>% dplyr::mutate(
  control_grouped = factor(control_grouped),
  vas_control = factor(vas_control)
)

# Data is called hads_for_analysis
# Rename as data baseline
data_baseline <- hads_for_analysis

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


# Bar plots for baseline variables and hads score group

calc_proportion <- function(data, dependent, independent){
  
  data %>%
    dplyr::group_by(!!rlang::sym(independent), !!rlang::sym(dependent)) %>%
    dplyr::count() %>%
    # dplyr::group_by(!!rlang::sym(independent)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(p = n/sum(n)) 
  
}

# Diagnosis 
data_baseline %>%
  dplyr::filter(hads_type == 'anxiety_hads') %>%
  calc_proportion(., dependent = 'score_group', independent = 'diagnosis2') %>%
  ggplot(aes(x = score_group, y = p, fill = diagnosis2)) +
  geom_col(position = 'dodge') +
  geom_text(aes(label = diagnosis2), angle = 90, position = position_dodge(width = 0.9), hjust = -0.1)

# Sex
data_baseline %>%
  dplyr::filter(hads_type == 'anxiety_hads') %>%
  calc_proportion(., hads = 'anxiety_hads', dependent = 'score_group', independent = 'Sex') %>%
  ggplot(aes(x = score_group, y = p, fill = Sex)) +
  geom_col(position = 'dodge') +
  geom_text(aes(label = Sex), angle = 90, position = position_dodge(width = 0.9), hjust = -0.1)

# Age Group
data_baseline %>%
  dplyr::filter(hads_type == 'anxiety_hads') %>%
  calc_proportion(., hads = 'anxiety_hads', dependent = 'score_group', independent = 'AgeGroup') %>%
  ggplot(aes(x = score_group, y = p, fill = AgeGroup)) +
  geom_col(position = 'dodge') +
  geom_text(aes(label = AgeGroup), angle = 90, position = position_dodge(width = 0.9), hjust = -0.1)

# Flare Group
data_baseline %>%
  dplyr::filter(hads_type == 'anxiety_hads') %>%
  calc_proportion(., hads = 'anxiety_hads', dependent = 'score_group', independent = 'flare_group') %>%
  ggplot(aes(x = score_group, y = p, fill = flare_group)) +
  geom_col(position = 'dodge') +
  geom_text(aes(label = flare_group), angle = 90, position = position_dodge(width = 0.9), hjust = -0.1)

# FC Cat
data_baseline %>%
  dplyr::filter(hads_type == 'anxiety_hads') %>%
  calc_proportion(., hads = 'anxiety_hads', dependent = 'score_group', independent = 'cat') %>%
  ggplot(aes(x = score_group, y = p, fill = cat)) +
  geom_col( position = 'dodge') +
  geom_text(aes(label = cat), angle = 90, position = position_dodge(width = 0.9), hjust = -0.1)

# Control 8
data_baseline %>%
  dplyr::filter(hads_type == 'anxiety_hads') %>%
  dplyr::filter(!is.na(control_grouped)) %>%
  calc_proportion(., hads = 'anxiety_hads', dependent = 'score_group', independent = 'control_grouped') %>%
  ggplot(aes(x = score_group, y = p, fill = control_grouped)) +
  geom_col(position = 'dodge') +
  geom_text(aes(label = control_grouped), angle = 90, position = position_dodge(width = 0.9), hjust = -0.1)

# Control vas
data_baseline %>%
  dplyr::filter(hads_type == 'anxiety_hads') %>%
  dplyr::filter(!is.na(vas_control)) %>%
  calc_proportion(., hads = 'anxiety_hads', dependent = 'score_group', independent = 'vas_control') %>%
  ggplot(aes(x = score_group, y = p, fill = vas_control)) +
  geom_col(position = 'dodge') +
  geom_text(aes(label = vas_control), angle = 90, position = position_dodge(width = 0.9), hjust = -0.1)


# Need to repeat for depression, then separated into CD and UC.


# Survival analysis ####

# Kaplan Meier
# Soft
data_survival_soft %>%
  dplyr::filter(hads_type == 'anxiety_hads') %>%
  survfit(Surv(time, DiseaseFlareYN) ~ score_group, data = .) %>%
  ggsurvplot(., data = data_survival_soft, conf.int = TRUE, ggtheme = theme_minimal())

# Hard
data_survival_hard %>%
  dplyr::filter(hads_type == 'anxiety_hads') %>%
  survfit(Surv(time, DiseaseFlareYN) ~ score_group, data = .) %>%
  ggsurvplot(., data = data_survival_hard, conf.int = TRUE, ggtheme = theme_minimal())


# Cox model
# Soft
cox_soft <- coxph(
  Surv(time, DiseaseFlareYN) ~
    score_group +
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
cox_hard <- coxph(
  Surv(time, DiseaseFlareYN) ~
    score_group +
    IMD +
    Sex +
    AgeGroup +
    cat +
    frailty(SiteNo),
  data = data_survival_hard
)

cox_hard %>%
  broom::tidy(exponentiate = TRUE, conf.int = TRUE)
