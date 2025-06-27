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


##################
#### Figure 2 ####
##################

p1 <- readRDS(paste0(outdir, "flare-comparison.RDS"))
p2 <- readRDS(paste0(outdir, "flare-soft.RDS"))
p3 <- readRDS(paste0(outdir, "flare-hard.RDS"))

lay <- rbind(
  c(1, 1),
  c(2, 3)
)

p <- list(p1, p2, p3)

survs <- lapply(
  p,
  survminer:::.build_ggsurvplot,
  surv.plot.height = NULL,
  risk.table.height = NULL,
  ncensor.plot.height = NULL
)

survs <- do.call(
  gridExtra::marrangeGrob,
  list(grobs = survs, layout_matrix = lay)
)

cairo_pdf(
  "plots/arranged/Figure2.pdf",
  width = 16.5 * 5 / 8,
  height = 18.5 * 5 / 8
)
survs
dev.off()

png(
  "plots/arranged/Figure2.png",
  width = 16.5 * 5 / 8,
  height = 18.5 * 5 / 8,
  units = "in",
  res = 300
)
survs
dev.off()

##################
#### Figure 3 ####
##################

p1 <- readRDS(paste0(outdir, "fc-cd-soft.RDS"))$plot
p2 <- readRDS(paste0(outdir, "fc-cd-hard.RDS"))$plot
p3 <- readRDS(paste0(outdir, "fc-uc-soft.RDS"))$plot
p4 <- readRDS(paste0(outdir, "fc-uc-hard.RDS"))$plot

p <- (p1 + p2) /
  (p3 + p4) +
  plot_annotation(tag_levels = "A") +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(
  "plots/arranged/Figure3.pdf",
  p,
  width = 12.5 * 3 / 4,
  height = 12.5 * 3 / 4,
  units = "in"
)
ggsave(
  "plots/arranged/Figure3.png",
  p,
  width = 16.5 * 2 / 3,
  height = 16.5 * 2 / 3,
  units = "in"
)


##################
#### Figure 4 ####
##################

p1 <- readRDS(paste0(outdir, "meat-cd-soft.RDS"))$plot
p2 <- readRDS(paste0(outdir, "meat-cd-hard.RDS"))$plot
p3 <- readRDS(paste0(outdir, "meat-uc-soft.RDS"))$plot
p4 <- readRDS(paste0(outdir, "meat-uc-hard.RDS"))$plot

p <- (p1 + p2) /
  (p3 + p4) +
  plot_annotation(tag_levels = "A") +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(
  "plots/arranged/Figure4.pdf",
  p,
  width = 11.7 * 2 / 3,
  height = 16.5 * 4 / 9,
  units = "in"
)
ggsave(
  "plots/arranged/Figure4.png",
  p,
  width = 11.7 * 2 / 3,
  height = 16.5 * 4 / 9,
  units = "in"
)
