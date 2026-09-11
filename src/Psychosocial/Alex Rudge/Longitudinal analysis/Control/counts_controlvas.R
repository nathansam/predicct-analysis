library(tidyverse)
library(magrittr)
library(glue)

setwd("/Users/arudge/GitHub/predicct-analysis/src/Psychosocial/Alex Rudge/Longitudinal analysis/Control")

qmd_code <- knitr::purl(
  "ControlVAS.qmd",
  output = tempfile(fileext = ".R"),
  documentation = 0,
  quiet = TRUE
)

source(qmd_code, local = .GlobalEnv)

palette <- c("#FFA500", "#0072B2", "grey")


data_soft_long %>%
  dplyr::filter_out(is.na(OverallControl)) %>%
  dplyr::count(month, OverallControl_grouped) %>%
  dplyr::group_by(month) %>%
  dplyr::mutate(
    p = n / sum(n),
    mid = rev(cumsum(rev(n))) - n / 2,
    percent = glue::glue("{round(p * 100)}%")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(month = forcats::as_factor(month)) %>%
  ggplot(aes(x = month, y = n, fill = OverallControl_grouped, colour = OverallControl_grouped)) +
  geom_col(alpha = 0.5) +
  geom_text(aes(y = mid, label = percent), size = 3, colour = "black") +
  scale_fill_manual(values = palette) +
  scale_colour_manual(values = palette) +
  scale_y_continuous(breaks = seq(0, 2000, 250)) +
  xlab("Month") +
  ylab("Number of participant responses") +
  ggtitle("Number of questionnaires completed per month") +
  custom_theme


overall_control_percentage <- data_soft_long %>%
  dplyr::filter_out(is.na(OverallControl)) %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(
    n = dplyr::n(),
    n_below_85 = sum(OverallControl_grouped == "<85"),
    percentage = n_below_85 / n,
    confidence_interval = list(
      stats::binom.test(n_below_85, n)$conf.int
    ),
    .groups = "drop"
  ) %>%
  tidyr::unnest_wider(confidence_interval, names_sep = "_") %>%
  dplyr::rename(
    ci_low = confidence_interval_1,
    ci_high = confidence_interval_2
  ) %>%
  dplyr::mutate(month = as.numeric(as.character(month)))


overall_control_percentage %>%
  ggplot(aes(x = month, y = percentage)) +
  geom_ribbon(
    aes(ymin = ci_low, ymax = ci_high),
    fill = "#0072B2",
    alpha = 0.2
  ) +
  geom_line(colour = "#0072B2", linewidth = 1) +
  geom_point(colour = "#0072B2") +
  scale_y_continuous(labels = scales::label_percent()) +
  xlab("Month") +
  ylab("") +
  ggtitle("Percentage of patients with IBD-Control-VAS less than 85") +
  custom_theme


overall_control_percentage_diagnosis <- data_soft_long %>%
  dplyr::filter_out(is.na(OverallControl)) %>%
  dplyr::filter(diagnosis2 %in% c("CD", "UC/IBDU")) %>%
  dplyr::mutate(
    diagnosis = dplyr::case_when(
      diagnosis2 == "CD" ~ "CD",
      diagnosis2 == "UC/IBDU" ~ "UC"
    )
  ) %>%
  dplyr::group_by(diagnosis, month) %>%
  dplyr::summarise(
    n = dplyr::n(),
    n_below_85 = sum(OverallControl_grouped == "<85"),
    percentage = n_below_85 / n,
    confidence_interval = list(
      stats::binom.test(n_below_85, n)$conf.int
    ),
    .groups = "drop"
  ) %>%
  tidyr::unnest_wider(confidence_interval, names_sep = "_") %>%
  dplyr::rename(
    ci_low = confidence_interval_1,
    ci_high = confidence_interval_2
  ) %>%
  dplyr::mutate(month = as.numeric(as.character(month)))


overall_control_percentage_diagnosis %>%
  ggplot(aes(x = month, y = percentage, colour = diagnosis)) +
  geom_ribbon(
    aes(ymin = ci_low, ymax = ci_high, fill = diagnosis),
    alpha = 0.2,
    colour = NA
  ) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_colour_manual(
    values = c("CD" = palette[2], "UC" = palette[1])
  ) +
  scale_fill_manual(
    values = c("CD" = palette[2], "UC" = palette[1])
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    x = "Month",
    y = "Percentage with IBD-Control-VAS less than 85",
    colour = "Diagnosis",
    fill = "Diagnosis",
    title = "Percentage of patients with IBD-Control-VAS less than 85 by diagnosis"
  ) +
  custom_theme
