library(tidyverse)
library(magrittr)
library(gtsummary)

# Objective: Comparing IBD control at baseline between CD and UC

# Load data
alex_data <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data"


demographics <- readr::read_rds(
  file = file.path(alex_data, "common_variables.rds")
)
  
  

# How many have data?
data_baseline <- demographics %>%
  dplyr::select(diagnosis2, control_8, OverallControl)

data_baseline %>%
  dplyr::group_by(diagnosis2) %>%
  dplyr::summarise(n = n())

# Control 8
data_baseline %>%
  dplyr::filter_out(is.na(control_8)) %>%
  dplyr::group_by(diagnosis2) %>%
  dplyr::summarise(n = n())

# CD 971/1370 = 70.8%
# UC 932/1259 = 74.0%

data_baseline %<>%
  dplyr::filter_out(is.na(control_8)) %>%
  dplyr::filter_out(is.na(OverallControl))

data_baseline %>%
  ggplot(aes(x = control_8, colour = diagnosis2)) +
  geom_density()

data_baseline %>%
  ggplot(aes(x = OverallControl, colour = diagnosis2)) +
  geom_density()



# Discretise
data_baseline %<>%
  dplyr::mutate(
    control_8_binary = dplyr::if_else(control_8 < 13, "<13", ">=13"),
    control_vas_binary = dplyr::if_else(OverallControl < 85, "<85", ">=85"),
  )

chisq_control_8 <- stats::chisq.test(
  x = data_baseline$control_8_binary,
  y = data_baseline$diagnosis2
)

chisq_control_vas <- stats::chisq.test(
  x = data_baseline$control_vas_binary,
  y = data_baseline$diagnosis2
)


# Plot
# Control 8
data_baseline %>%
  dplyr::group_by(diagnosis2) %>%
  dplyr::count(control_8_binary) %>%
  dplyr::mutate(p = n/sum(n)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = control_8_binary, y = p, colour = diagnosis2, fill = diagnosis2)) +
  geom_col(position = 'dodge') +
  geom_text(aes(label = scales::percent(p, accuracy = 1)), position = ggplot2::position_dodge(width = 0.9), vjust = -0.3, show.legend = FALSE) +
  xlab("IBD Control-8 score") +
  ggplot2::scale_x_discrete(labels = c("<13" = "< 13", ">=13" = "≥ 13")) +
  labs(colour = "IBD type", fill = "IBD type") +
  ggtitle(
    "Control-8 score by IBD type",
    subtitle = paste0(
      "Chi-squared test p = ",
      format.pval(chisq_control_8$p.value, digits = 3)
    )
  ) +
  scale_y_continuous(labels = scales::label_percent(), breaks = scales::breaks_width(0.1), expand = ggplot2::expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))


# Control VAS
data_baseline %>%
  dplyr::group_by(diagnosis2) %>%
  dplyr::count(control_vas_binary) %>%
  dplyr::mutate(p = n/sum(n)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = control_vas_binary, y = p, colour = diagnosis2, fill = diagnosis2)) +
  geom_col(position = 'dodge') +
  geom_text(aes(label = scales::percent(p, accuracy = 1)), position = ggplot2::position_dodge(width = 0.9), vjust = -0.3, show.legend = FALSE) +
  xlab("IBD Control-VAS score") +
  ggplot2::scale_x_discrete(labels = c("<85" = "< 85", ">=85" = "≥ 85")) +
  labs(colour = "IBD type", fill = "IBD type") +
  ggtitle(
    "Control-VAS score by IBD type",
    subtitle = paste0(
      "Chi-squared test p = ",
      format.pval(chisq_control_vas$p.value, digits = 3)
    )
  ) +
  scale_y_continuous(labels = scales::label_percent(), breaks = scales::breaks_width(0.1), expand = ggplot2::expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

