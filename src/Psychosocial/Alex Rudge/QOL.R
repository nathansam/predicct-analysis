
# QOL - Quality of life

# Run QOL.qmd prior to the analysis to get the data
# Called QOL, rename to data_baseline
data_baseline <- QOL


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

# Our dependent variables are 2 continuous variables representing patients' 
# Physical component and Mental component scores

# Called PSC12 (Physical) and MCS12 (Mental)

# Density plots of each score
data_baseline %>%
  ggplot(aes(x = PSC12)) +
  geom_density()

data_baseline %>%
  ggplot(aes(x = MCS12)) +
  geom_density()


# Baseline plots
# PSC12

# Diagnosis
QOL %>% 
  ggplot() +
  geom_density(
    aes(x = PSC12, colour = diagnosis2)
  )

# Sex
QOL %>% 
  ggplot() +
  geom_density(
    aes(x = PSC12, colour = Sex)
  )

# Age Groups
QOL %>% 
  ggplot() +
  geom_density(
    aes(x = PSC12, colour = AgeGroup)
  )

# Flare
QOL %>% 
  ggplot() +
  geom_density(
    aes(x = PSC12, colour = flare_group)
  )

# FC
QOL %>% 
  ggplot() +
  geom_density(
    aes(x = PSC12, colour = cat)
  )

# Hypothesis test
# 2 groups use wilcox (aka Mann Whitney), for 3 or more use kruskal.
# Could also use kruskal for all as it gives the same p value.

dplyr::bind_rows(
  QOL %>%
    wilcox_test(PSC12 ~ diagnosis2) %>%
    dplyr::mutate(group = "diagnosis2"),
  
  QOL %>%
    wilcox_test(PSC12 ~ Sex) %>%
    dplyr::mutate(group = "Sex"),
  
  QOL %>%
    kruskal_test(PSC12 ~ AgeGroup) %>%
    dplyr::mutate(group = "AgeGroup"),
  
  QOL %>%
    wilcox_test(PSC12 ~ flare_group) %>%
    dplyr::mutate(group = "flare_group"),
  
  QOL %>%
    kruskal_test(PSC12 ~ cat) %>%
    dplyr::mutate(group = "cat"),
) %>%
  tidyr::replace_na(list(method = "Mann-Whitney")) %>%
  dplyr::mutate(p.adjust = p.adjust(p = p, method = 'holm')) %>%
  dplyr::mutate(p.adjust = signif(p.adjust, 3)) %>%
  dplyr::mutate(
    p.signif = dplyr::case_when(
      p.adjust < 0.0001 ~ "****",
      p.adjust < 0.001 ~ "***",
      p.adjust < 0.01  ~ "**",
      p.adjust < 0.05  ~ "*",
      .default = 'ns'
    )
  )


# MCS12
# Diagnosis
QOL %>% 
  ggplot() +
  geom_density(
    aes(x = MCS12, colour = diagnosis2)
  )

# Sex
QOL %>% 
  ggplot() +
  geom_density(
    aes(x = MCS12, colour = Sex)
  )

# Age Groups
QOL %>% 
  ggplot() +
  geom_density(
    aes(x = MCS12, colour = AgeGroup)
  )


# Flare
QOL %>% 
  ggplot() +
  geom_density(
    aes(x = MCS12, colour = flare_group)
  )

# FC
QOL %>% 
  ggplot() +
  geom_density(
    aes(x = MCS12, colour = cat)
  )

# Hypothesis test
# 2 groups use wilcox (aka Mann Whitney), for 3 or more use kruskal.
# Could also use kruskal for all as it gives the same p value.

dplyr::bind_rows(
  QOL %>%
    wilcox_test(MCS12 ~ diagnosis2) %>%
    dplyr::mutate(group = "diagnosis2"),
  
  QOL %>%
    wilcox_test(MCS12 ~ Sex) %>%
    dplyr::mutate(group = "Sex"),
  
  QOL %>%
    kruskal_test(MCS12 ~ AgeGroup) %>%
    dplyr::mutate(group = "AgeGroup"),
  
  QOL %>%
    wilcox_test(MCS12 ~ flare_group) %>%
    dplyr::mutate(group = "flare_group"),
  
  QOL %>%
    kruskal_test(MCS12 ~ cat) %>%
    dplyr::mutate(group = "cat"),
) %>%
  tidyr::replace_na(list(method = "Mann-Whitney")) %>%
  dplyr::mutate(p.adjust = p.adjust(p = p, method = 'holm')) %>%
  dplyr::mutate(p.adjust = signif(p.adjust, 3)) %>%
  dplyr::mutate(
    p.signif = dplyr::case_when(
      p.adjust < 0.0001 ~ "****",
      p.adjust < 0.001 ~ "***",
      p.adjust < 0.01  ~ "**",
      p.adjust < 0.05  ~ "*",
      .default = 'ns'
    )
  )

