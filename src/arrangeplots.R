library(ggplot2)
library(patchwork)
library(survminer)

if (file.exists("/docker")) { # If running in docker
  data.path <- "data/final/20221004/"
  redcap.path <- "data/final/20231030/"
  prefix <- "data/end-of-follow-up/"
  outdir <- "data/processed"
} else { # Run on OS directly
  data.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20221004/"
  redcap.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20231030/"
  prefix <- "/Volumes/igmm/cvallejo-predicct/predicct/end-of-follow-up/"
  outdir <- "/Volumes/igmm/cvallejo-predicct/predicct/processed/"
}

##################
#### Figure 2 ####
##################

p1 <- readRDS(paste0(outdir, "flare-comparison.RDS"))$plot
p2 <- readRDS(paste0(outdir, "fc-cd-soft.RDS"))$plot
p3 <- readRDS(paste0(outdir, "fc-cd-hard.RDS"))$plot
p4 <- readRDS(paste0(outdir, "fc-uc-soft.RDS"))$plot
p5 <- readRDS(paste0(outdir, "fc-uc-hard.RDS"))$plot

p <- p1 / (p2 + p3) / (p4 + p5) +
  plot_annotation(tag_levels = "A") +
  patchwork::plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("plots/arranged/Figure2.pdf",
       p,
       width = 11.7 * 3/4,
       height = 16.5 * 3/4,
       units = "in")
ggsave("plots/arranged/Figure2.png",
       p,
       width = 11.7 * 2/3,
       height = 16.5 * 2/3,
       units = "in")


##################
#### Figure 3 ####
##################


p1 <- readRDS(paste0(outdir, "meat-cd-soft.RDS"))$plot
p2 <- readRDS(paste0(outdir, "meat-cd-hard.RDS"))$plot
p3 <- readRDS(paste0(outdir, "meat-uc-soft.RDS"))$plot
p4 <- readRDS(paste0(outdir, "meat-uc-hard.RDS"))$plot

p <- (p1 + p2) / (p3 + p4) +
  plot_annotation(tag_levels = "A") +
  patchwork::plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("plots/arranged/Figure3.pdf",
       p,
       width = 11.7 * 2/3,
       height = 16.5 * 4/9,
       units = "in")
ggsave("plots/arranged/Figure3.png",
       p,
       width = 11.7 * 2/3,
       height = 16.5 * 4/9,
       units = "in")
