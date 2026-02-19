library(tidyverse)
library(readxl)
library(patchwork)

monthly <- read_xlsx(
  paste0(
    "/Volumes/igmm/cvallejo-predicct/predicct/",
    "final/20221004/Followup/monthlyQ.xlsx"
  )
)


monthly$TimepointId <- (monthly$TimepointId / 20) - 2

demo <- readRDS(
  paste0(
    "/Volumes/igmm/cvallejo-predicct/predicct/",
    "processed/flares-demographics.RDS"
  )
)

temp <- monthly %>%
  inner_join(demo, by = "ParticipantNo")

temp$softflare <- factor(
  temp$softflare,
  levels = c("0", "1"),
  labels = c("No flare", "Flare")
)
temp$softflare <- relevel(temp$softflare, ref = "Flare")


temp$hardflare <- factor(
  temp$hardflare,
  levels = c("0", "1"),
  labels = c("No flare", "Flare")
)

temp$hardflare <- relevel(temp$hardflare, ref = "Flare")


p1 <- temp %>%
  drop_na(softflare) %>%
  ggplot(
    aes(
      x = TimepointId,
      y = OverallControl,
      group = as.factor(ParticipantNo),
      color = softflare
    )
  ) +
  geom_line(alpha = 0.25) +
  theme_minimal() +
  labs(
    color = "Flare status",
    x = "Month",
    y = "Control VAS",
    title = "VAS Soft flare"
  )


p2 <- temp %>%
  drop_na(hardflare) %>%
  ggplot(
    aes(
      x = TimepointId,
      y = OverallControl,
      group = as.factor(ParticipantNo),
      color = hardflare
    )
  ) +
  geom_line(alpha = 0.25) +
  theme_minimal() +
  labs(
    color = "Flare status",
    x = "Month",
    y = "Control VAS",
    title = "VAS Hard flare"
  )


p3 <- temp %>%
  drop_na(hardflare) %>%
  ggplot(
    aes(
      x = TimepointId,
      y = OverallControl,
      group = as.factor(ParticipantNo),
      color = hardflare
    )
  ) +
  geom_line(alpha = 0.25) +
  theme_minimal() +
  labs(
    color = "Flare status",
    x = "Month",
    y = "Control VAS",
    title = "VAS Hard flare"
  ) +
  facet_grid(rows = vars(hardflare))


p1 <- temp %>%
  drop_na(softflare) %>%
  group_by(TimepointId, softflare) %>%
  summarise(
    mean_val = mean(OverallControl, na.rm = TRUE),
    sd_val = sd(OverallControl, na.rm = TRUE),
    n_obs = sum(!is.na(OverallControl)),
    se = sd_val / sqrt(n_obs),
    lower = mean_val - 1.96 * se,
    upper = mean_val + 1.96 * se,
    .groups = "drop"
  ) %>%
  ggplot(
    aes(
      x = TimepointId,
      y = mean_val,
      color = softflare,
      fill = softflare
    )
  ) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  theme_minimal() +
  ylim(0, 100) +
  xlim(0, 24) +
  labs(
    color = "Flare status",
    fill = "Flare status",
    x = "Month",
    y = "Control VAS",
    title = "VAS Over Time Stratified by Soft Flare"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


p2 <- temp %>%
  drop_na(hardflare) %>%
  group_by(TimepointId, hardflare) %>%
  summarise(
    mean_val = mean(OverallControl, na.rm = TRUE),
    sd_val = sd(OverallControl, na.rm = TRUE),
    n_obs = sum(!is.na(OverallControl)), # Count only non-NA values
    se = sd_val / sqrt(n_obs),
    lower = mean_val - 1.96 * se,
    upper = mean_val + 1.96 * se,
    .groups = "drop"
  ) %>%
  ggplot(
    aes(
      x = TimepointId,
      y = mean_val,
      color = hardflare,
      fill = hardflare
    )
  ) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  theme_minimal() +
  ylim(0, 100) +
  xlim(0, 24) +
  labs(
    color = "Flare status",
    fill = "Flare status",
    x = "Month",
    y = "Control VAS",
    title = "VAS Over Time Stratified by Hard Flare"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

p <- p1 / p2 + patchwork::plot_layout(axis_titles = "collect")
ggsave("VAS-long.png", p, width = 8, height = 8)
