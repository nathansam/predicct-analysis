library(tidyverse)
library(magrittr)
library(survival)
library(patchwork)


# 2x2 plot for the manuscript for the main variables in soft uc

# Load plots
filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Kaplan-Meier/"

# Anxiety
plot_anxiety <- readr::read_rds(
  file = paste0(filepath, "plot_anxiety_soft_uc.rds")
)

plot_anxiety

# Depression
plot_depression <- readr::read_rds(
  file = paste0(filepath, "plot_depression_soft_uc.rds")
)

plot_depression

# Sleep
plot_psqi <- readr::read_rds(
  file = paste0(filepath, "plot_psqi_soft_uc.rds")
)

plot_psqi

# Somatisation
plot_phq <- readr::read_rds(
  file = paste0(filepath, "plot_phq_soft_uc.rds")
)

plot_phq

# Remove titles from individual plots
plot_anxiety$plot <- plot_anxiety$plot + theme(plot.title = element_blank())
plot_depression$plot <- plot_depression$plot + theme(plot.title = element_blank())
plot_psqi$plot <- plot_psqi$plot + theme(plot.title = element_blank())
plot_phq$plot <- plot_phq$plot + theme(plot.title = element_blank())


# 2x2 plot
layout = "
AACC
BBDD
EEGG
FFHH"

plot <- plot_anxiety$plot + plot_anxiety$table +
  plot_depression$plot + plot_depression$table +
  plot_psqi$plot + plot_psqi$table +
  plot_phq$plot + plot_phq$table +
  plot_layout(
    design = layout,
    heights = c(3, 1, 3, 1),
    axes = 'collect'
    ) +
  plot_annotation(
    title = 'Time to patient reported flare in ulcerative colitis',
    theme = theme(
      plot.title = element_text(size = 16, hjust = 0.5))
  ) &
  theme(
    plot.margin = margin(0,0,0,0),
    axis.title.y = element_text(vjust = -7)
  )

plot

# Save
filepath_save <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Plots/"

ggsave(
  filename = paste0(filepath_save, "Kaplan Meier Soft UC.pdf"),
  plot = plot,
  width = 9.5,
  height = 8,
  units = 'in'
)
