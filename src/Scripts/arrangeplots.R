library(ggplot2)
library(patchwork)
library(survminer)

if (file.exists("/docker")) {
  # If running in docker
  data.path <- "data/final/20221004/"
  redcap.path <- "data/final/20231030/"
  prefix <- "data/end-of-follow-up/"
  outdir <- "data/processed/"
} else {
  # Run on OS directly
  data.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20221004/"
  redcap.path <- "/Volumes/igmm/cvallejo-predicct/predicct/final/20231030/"
  prefix <- "/Volumes/igmm/cvallejo-predicct/predicct/end-of-follow-up/"
  outdir <- "/Volumes/igmm/cvallejo-predicct/predicct/processed/"
}

arrange_subplot <- function(p) {
  x <- p$plot / p$table + plot_layout(heights = c(8, 2))
  wrap_elements(x)
}


##################
#### Figure 2 ####
##################

p1 <- readRDS(paste0(outdir, "flare-comparison.RDS"))
p2 <- readRDS(paste0(outdir, "flare-soft.RDS"))
p3 <- readRDS(paste0(outdir, "flare-hard.RDS"))


p1_arranged <-arrange_subplot(p1)
p2_arranged <- arrange_subplot(p2)
p3_arranged <-  arrange_subplot(p3)

p <- (p1_arranged) /
  (p2_arranged + p3_arranged) +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom",
        plot.tag = element_text(size = 18, face = "bold"))

cairo_pdf(
  "plots/arranged/Figure2.pdf",
  width = 16.5 * 6 / 8,
  height = 18.5 * 6 / 8,
  onefile = FALSE
)
p
dev.off()

png(
  "plots/arranged/Figure2.png",
  width = 16.5 * 5 / 8,
  height = 18.5 * 5 / 8,
  units = "in",
  res = 300
)
p
dev.off()

##################
#### Figure 3 ####
##################

p1 <- readRDS(paste0(outdir, "fc-cd-soft.RDS"))
p2 <- readRDS(paste0(outdir, "fc-cd-hard.RDS"))
p3 <- readRDS(paste0(outdir, "fc-uc-soft.RDS"))
p4 <- readRDS(paste0(outdir, "fc-uc-hard.RDS"))

p1_arranged <-arrange_subplot(p1)
p2_arranged <- arrange_subplot(p2)
p3_arranged <-  arrange_subplot(p3)
p4_arranged <-  arrange_subplot(p4)

p <- (p1_arranged + p2_arranged) /
  (p3_arranged + p4_arranged) +
  plot_annotation(tag_levels = "A") &
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        plot.tag = element_text(size = 18, face = "bold"))

ggsave(
  "plots/arranged/Figure3.pdf",
  p,
  width = 12.5 * 7/8 ,
  height = 12.5 * 7/8,
  units = "in"
)
ggsave(
  "plots/arranged/Figure3.png",
  p,
  width = 12.5 * 7/8,
  height = 12.5 * 7/8,
  units = "in"
)


##################
#### Figure 4 ####
##################

p1 <- readRDS(paste0(outdir, "meat-cd-soft.RDS"))
p2 <- readRDS(paste0(outdir, "meat-cd-hard.RDS"))
p3 <- readRDS(paste0(outdir, "meat-uc-soft.RDS"))
p4 <- readRDS(paste0(outdir, "meat-uc-hard.RDS"))


p1_arranged <-arrange_subplot(p1)
p2_arranged <- arrange_subplot(p2)
p3_arranged <-  arrange_subplot(p3)
p4_arranged <-  arrange_subplot(p4)

p <- (p1_arranged + p2_arranged) /
  (p3_arranged + p4_arranged) +
  plot_annotation(tag_levels = "A") &
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        plot.tag = element_text(size = 18, face = "bold"))

ggsave(
  "plots/arranged/Figure4.pdf",
  p,
  width = 12.5 * 7/8,
  height = 12.5 * 7/8,
  units = "in"
)
ggsave(
  "plots/arranged/Figure4.png",
  p,
  width = 12.5 * 7/8,
  height = 12.5 * 7/8,
  units = "in"
)
