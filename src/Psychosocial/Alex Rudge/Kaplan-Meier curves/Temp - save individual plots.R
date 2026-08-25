library(tidyverse)
library(magrittr)
library(glue)
library(patchwork)

# Kaplan-Meier plots for Prof Charles

filepath <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Data/Kaplan-Meier/"


# Anxiety

plot_soft_cd <- readr::read_rds(
  glue("{filepath}plot_anxiety_soft_cd.rds")
)

plot_soft_uc <- readr::read_rds(
  glue("{filepath}plot_anxiety_soft_uc.rds")
)

plot_hard_cd <- readr::read_rds(
  glue("{filepath}plot_anxiety_hard_cd.rds")
)

plot_hard_uc <- readr::read_rds(
  glue("{filepath}plot_anxiety_hard_uc.rds")
)


# Fix plots using patchwork

plot_soft_cd <- plot_soft_cd$plot + plot_soft_cd$table +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(3.2, 1)
  )

plot_soft_uc <- plot_soft_uc$plot + plot_soft_uc$table +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(3.2, 1)
  ) 

plot_hard_cd <- plot_hard_cd$plot + plot_hard_cd$table +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(3.2, 1)
  )

plot_hard_uc <- plot_hard_uc$plot + plot_hard_uc$table +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(3.2, 1)
  ) 


# Save landscape 6.8x5 inch

filepath_save <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Plots/"

ggsave(
  filename = glue("{filepath_save}Kaplan Meier Anxiety soft CD.pdf"),
  plot = plot_soft_cd,
  width = 5.5,
  height = 5,
  units = 'in'
)

ggsave(
  filename = glue("{filepath_save}Kaplan Meier Anxiety soft UC.pdf"),
  plot = plot_soft_uc,
  width = 5.5,
  height = 5,
  units = 'in'
)

ggsave(
  filename = glue("{filepath_save}Kaplan Meier Anxiety hard CD.pdf"),
  plot = plot_hard_cd,
  width = 5.5,
  height = 5,
  units = 'in'
)

ggsave(
  filename = glue("{filepath_save}Kaplan Meier Anxiety hard UC.pdf"),
  plot = plot_hard_uc,
  width = 5.5,
  height = 5,
  units = 'in'
)


# Depression

plot_soft_cd <- readr::read_rds(
  glue("{filepath}plot_depression_soft_cd.rds")
)

plot_soft_uc <- readr::read_rds(
  glue("{filepath}plot_depression_soft_uc.rds")
)

plot_hard_cd <- readr::read_rds(
  glue("{filepath}plot_depression_hard_cd.rds")
)

plot_hard_uc <- readr::read_rds(
  glue("{filepath}plot_depression_hard_uc.rds")
)


# Fix plots using patchwork

plot_soft_cd <- plot_soft_cd$plot + plot_soft_cd$table +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(3.2, 1)
  )

plot_soft_uc <- plot_soft_uc$plot + plot_soft_uc$table +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(3.2, 1)
  ) 

plot_hard_cd <- plot_hard_cd$plot + plot_hard_cd$table +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(3.2, 1)
  )

plot_hard_uc <- plot_hard_uc$plot + plot_hard_uc$table +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(3.2, 1)
  ) 


# Save landscape 6.8x5 inch

filepath_save <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Plots/"

ggsave(
  filename = glue("{filepath_save}Kaplan Meier Depression soft CD.pdf"),
  plot = plot_soft_cd,
  width = 5.5,
  height = 5,
  units = 'in'
)

ggsave(
  filename = glue("{filepath_save}Kaplan Meier Depression soft UC.pdf"),
  plot = plot_soft_uc,
  width = 5.5,
  height = 5,
  units = 'in'
)


ggsave(
  filename = glue("{filepath_save}Kaplan Meier Depression hard CD.pdf"),
  plot = plot_hard_cd,
  width = 5.5,
  height = 5,
  units = 'in'
)

ggsave(
  filename = glue("{filepath_save}Kaplan Meier Depression hard UC.pdf"),
  plot = plot_hard_uc,
  width = 5.5,
  height = 5,
  units = 'in'
)


# PHQ

plot_soft_cd <- readr::read_rds(
  glue("{filepath}plot_phq_soft_cd.rds")
)

plot_soft_uc <- readr::read_rds(
  glue("{filepath}plot_phq_soft_uc.rds")
)

plot_hard_cd <- readr::read_rds(
  glue("{filepath}plot_phq_hard_cd.rds")
)

plot_hard_uc <- readr::read_rds(
  glue("{filepath}plot_phq_hard_uc.rds")
)


# Fix plots using patchwork

plot_soft_cd <- plot_soft_cd$plot + plot_soft_cd$table +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(3.2, 1)
  ) & theme(axis.title.y = element_text(vjust = -20))

plot_soft_uc <- plot_soft_uc$plot + plot_soft_uc$table +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(3.2, 1)
  ) & theme(axis.title.y = element_text(vjust = -20))

plot_hard_cd <- plot_hard_cd$plot + plot_hard_cd$table +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(3.2, 1)
  ) & theme(axis.title.y = element_text(vjust = -20))

plot_hard_uc <- plot_hard_uc$plot + plot_hard_uc$table +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(3.2, 1)
  ) & theme(axis.title.y = element_text(vjust = -20))


# Save landscape 6.8x5 inch

filepath_save <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Plots/"

ggsave(
  filename = glue("{filepath_save}Kaplan Meier PHQ soft CD.pdf"),
  plot = plot_soft_cd,
  width = 6.8,
  height = 5,
  units = 'in'
)

ggsave(
  filename = glue("{filepath_save}Kaplan Meier PHQ soft UC.pdf"),
  plot = plot_soft_uc,
  width = 6.8,
  height = 5,
  units = 'in'
)

ggsave(
  filename = glue("{filepath_save}Kaplan Meier PHQ hard CD.pdf"),
  plot = plot_hard_cd,
  width = 6.8,
  height = 5,
  units = 'in'
)

ggsave(
  filename = glue("{filepath_save}Kaplan Meier PHQ hard UC.pdf"),
  plot = plot_hard_uc,
  width = 6.8,
  height = 5,
  units = 'in'
)



# Fatigue

plot_soft_cd <- readr::read_rds(
  glue("{filepath}plot_fatigue_soft_cd.rds")
)

plot_soft_uc <- readr::read_rds(
  glue("{filepath}plot_fatigue_soft_uc.rds")
)

plot_hard_cd <- readr::read_rds(
  glue("{filepath}plot_fatigue_hard_cd.rds")
)

plot_hard_uc <- readr::read_rds(
  glue("{filepath}plot_fatigue_hard_uc.rds")
)


# Fix plots using patchwork

plot_soft_cd <- plot_soft_cd$plot + plot_soft_cd$table +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(3.2, 1)
  )

plot_soft_uc <- plot_soft_uc$plot + plot_soft_uc$table +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(3.2, 1)
  ) 

plot_hard_cd <- plot_hard_cd$plot + plot_hard_cd$table +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(3.2, 1)
  )

plot_hard_uc <- plot_hard_uc$plot + plot_hard_uc$table +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(3.2, 1)
  ) 


# Save landscape 6.8x5 inch

filepath_save <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Plots/"

ggsave(
  filename = glue("{filepath_save}Kaplan Meier Fatigue soft CD.pdf"),
  plot = plot_soft_cd,
  width = 5.5,
  height = 5,
  units = 'in'
)

ggsave(
  filename = glue("{filepath_save}Kaplan Meier Fatigue soft UC.pdf"),
  plot = plot_soft_uc,
  width = 5.5,
  height = 5,
  units = 'in'
)


ggsave(
  filename = glue("{filepath_save}Kaplan Meier Fatigue hard CD.pdf"),
  plot = plot_hard_cd,
  width = 5.5,
  height = 5,
  units = 'in'
)

ggsave(
  filename = glue("{filepath_save}Kaplan Meier Fatigue hard UC.pdf"),
  plot = plot_hard_uc,
  width = 5.5,
  height = 5,
  units = 'in'
)


# Sleep Disturbance

plot_soft_cd <- readr::read_rds(
  glue("{filepath}plot_psqi_soft_cd.rds")
)

plot_soft_uc <- readr::read_rds(
  glue("{filepath}plot_psqi_soft_uc.rds")
)

plot_hard_cd <- readr::read_rds(
  glue("{filepath}plot_psqi_hard_cd.rds")
)

plot_hard_uc <- readr::read_rds(
  glue("{filepath}plot_psqi_hard_uc.rds")
)


# Fix plots using patchwork

plot_soft_cd <- plot_soft_cd$plot + plot_soft_cd$table +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(3.2, 1)
  )

plot_soft_uc <- plot_soft_uc$plot + plot_soft_uc$table +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(3.2, 1)
  ) 

plot_hard_cd <- plot_hard_cd$plot + plot_hard_cd$table +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(3.2, 1)
  )

plot_hard_uc <- plot_hard_uc$plot + plot_hard_uc$table +
  patchwork::plot_layout(
    ncol = 1,
    heights = c(3.2, 1)
  ) 


# Save landscape 6.8x5 inch

filepath_save <- "/Volumes/igmm/cvallejo-predicct/people/Alex/Predicct2/Plots/"

ggsave(
  filename = glue("{filepath_save}Kaplan Meier PSQI soft CD.pdf"),
  plot = plot_soft_cd,
  width = 5.5,
  height = 5,
  units = 'in'
)

ggsave(
  filename = glue("{filepath_save}Kaplan Meier PSQI soft UC.pdf"),
  plot = plot_soft_uc,
  width = 5.5,
  height = 5,
  units = 'in'
)

ggsave(
  filename = glue("{filepath_save}Kaplan Meier PSQI hard CD.pdf"),
  plot = plot_hard_cd,
  width = 5.5,
  height = 5,
  units = 'in'
)

ggsave(
  filename = glue("{filepath_save}Kaplan Meier PSQI hard UC.pdf"),
  plot = plot_hard_uc,
  width = 5.5,
  height = 5,
  units = 'in'
)
