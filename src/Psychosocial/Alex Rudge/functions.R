
# Functions for the analysis

# Chi squared tests
summon_chisq_test <- function(data, dependent, independent) {
  
  # Chi square tests for the dependent variable vs each dependent variable
  
  purrr::map2_df(
    .x = dependent,
    .y = independent,
    .f = function(.x, .y) {
      x <- data %>%
        dplyr::pull(.x)
      
      y <- data %>%
        dplyr::pull(.y)
      
      chisq_test(x = x, y = y) %>%
        dplyr::mutate(dependent = .x, independent = .y)
    }
  ) %>%
    dplyr::mutate(p.adjust = p.adjust(p = p, method = 'holm')) %>%
    dplyr::mutate(p.adjust = signif(p.adjust, 3)) %>%
    dplyr::select(-p.signif) %>%
    dplyr::mutate(
      p.signif = dplyr::case_when(
        p.adjust < 0.0001 ~ "****",
        p.adjust < 0.001 ~ "***",
        p.adjust < 0.01  ~ "**",
        p.adjust < 0.05  ~ "*",
        .default = 'ns'
      )
    )
  
}

# Test
dependent = 'score_group'
independent = c('diagnosis2',
                'AgeGroup',
                'Sex',
                'flare_group',
                'cat')

summon_chisq_test(data = data_baseline, dependent = dependent, independent = independent)


# Creating proportions for plotting baseline data
calc_proportion <- function(data, dependent, independent){
  
  data %>%
    dplyr::group_by(!!rlang::sym(independent), !!rlang::sym(dependent)) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    # If we want proportions per group uncomment below
    # dplyr::group_by(!!rlang::sym(independent)) %>%
    dplyr::mutate(p = n/sum(n)) 
  
}

# Function to create all baseline plots
# State dependent and independent variables
dependent = 'score_group'

data = data_baseline %>%
  dplyr::filter(hads_type == 'anxiety_hads')

summon_baseline_plots <- function(data, dependent) {
  
  # Independent variables
  independent = c('diagnosis2',
                  'AgeGroup',
                  'Sex',
                  'flare_group',
                  'cat')
  
  # Chi squared tests
  chisq_results <- summon_chisq_test(
    data = data,
    dependent = dependent,
    independent = independent
  ) %>%
    dplyr::transmute(independent, p.adjust) %>%
    {setNames(as.list(.$p.adjust), .$independent)}

  
  # Creating plots for baseline variables with the dependent variable
  
  # Diagnosis
  p_diagnosis <- data %>%
    calc_proportion(., dependent = dependent, independent = 'diagnosis2') %>%
    ggplot(aes(x = score_group, y = p, fill = diagnosis2)) +
    geom_col(position = 'dodge') +
    geom_text(
      aes(label = diagnosis2),
      angle = 90,
      position = position_dodge(width = 0.9),
      hjust = -0.1
    ) +
    annotate(
      "text",
      label = paste0("Adjusted p-value: ", chisq_results$diagnosis2),
      x = Inf, y = Inf, vjust = 1.1, hjust = 1.5
    )
  
  # Sex
  p_sex <- data %>%
    calc_proportion(., dependent = dependent, independent = 'Sex') %>%
    ggplot(aes(x = score_group, y = p, fill = Sex)) +
    geom_col(position = 'dodge') +
    geom_text(
      aes(label = Sex),
      angle = 90,
      position = position_dodge(width = 0.9),
      hjust = -0.1
    ) +
    annotate(
      "text",
      label = paste0("Adjusted p-value: ", chisq_results$Sex),
      x = Inf, y = Inf, vjust = 1.1, hjust = 1.5
    )
  
  # Age Group
  p_age <- data %>%
    calc_proportion(., dependent = dependent, independent = 'AgeGroup') %>%
    ggplot(aes(x = score_group, y = p, fill = AgeGroup)) +
    geom_col(position = 'dodge') +
    geom_text(
      aes(label = AgeGroup),
      angle = 90,
      position = position_dodge(width = 0.9),
      hjust = -0.1
    ) +
    annotate(
      "text",
      label = paste0("Adjusted p-value: ", chisq_results$AgeGroup),
      x = Inf, y = Inf, vjust = 1.1, hjust = 1.5
    )
  
  # Flare Group
  p_flare <- data %>%
    calc_proportion(., dependent = dependent, independent = 'flare_group') %>%
    ggplot(aes(x = score_group, y = p, fill = flare_group)) +
    geom_col(position = 'dodge') +
    geom_text(
      aes(label = flare_group),
      angle = 90,
      position = position_dodge(width = 0.9),
      hjust = -0.1
    ) +
    annotate(
      "text",
      label = paste0("Adjusted p-value: ", chisq_results$flare_group),
      x = Inf, y = Inf, vjust = 1.1, hjust = 1.5
    )
  
  # FC Cat
  p_cat <- data %>%
    calc_proportion(., dependent = dependent, independent = 'cat') %>%
    ggplot(aes(x = score_group, y = p, fill = cat)) +
    geom_col(position = 'dodge') +
    geom_text(
      aes(label = cat),
      angle = 90,
      position = position_dodge(width = 0.9),
      hjust = -0.1
    ) +
    annotate(
      "text",
      label = paste0("Adjusted p-value: ", chisq_results$cat),
      x = Inf, y = Inf, vjust = 1.1, hjust = 1.5
    )
  
  # Combine with patchwork
  # p_diagnosis +
  #   p_age +
  #   p_sex +
  #   p_flare +
  #   p_cat +
  #   patchwork::plot_layout(ncol = 2)
  
  list(
    "diagnosis2" = p_diagnosis,
    "AgeGroup" = p_age,
    "Sex" = p_sex,
    "flare_group" = p_flare,
    "cat" = p_cat
  )
}


# Test
baseline_plots <- summon_baseline_plots(data = data, dependent = 'score_group')

baseline_plots$diagnosis2
baseline_plots$AgeGroup
baseline_plots$Sex
baseline_plots$flare_group
baseline_plots$cat
