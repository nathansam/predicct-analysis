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


###################
#### Figure S3 ####
###################

p1 <- readRDS(paste0(outdir, "smoke-cd-soft.RDS"))
p2 <- readRDS(paste0(outdir, "smoke-cd-hard.RDS"))
p3 <- readRDS(paste0(outdir, "smoke-uc-soft.RDS"))
p4 <- readRDS(paste0(outdir, "smoke-uc-hard.RDS"))

p <- list(p1, p2, p3, p4)

lay <- rbind(
  c(1, 2),
  c(3, 4)
)

survs <- lapply(
  p,
  survminer:::.build_ggsurvplot,
  surv.plot.height = NULL,
  risk.table.height = NULL,
  ncensor.plot.height = NULL
)

survs <- do.call(
  gridExtra::marrangeGrob,
  list(grobs = survs, layout_matrix = lay, top = NULL)
)

cairo_pdf(
  "plots/arranged/FigureS3.pdf",
  width = 16.5 * 5 / 8,
  height = 18.5 * 5 / 8
)
survs
dev.off()

png(
  "plots/arranged/FigureS3.png",
  width = 16.5 * 5 / 8,
  height = 18.5 * 5 / 8,
  units = "in",
  res = 300
)
survs
dev.off()



###################
#### Figure S8 ####
###################

p1 <- readRDS(paste0(outdir, "sex-cd-soft.RDS"))
p2 <- readRDS(paste0(outdir, "sex-cd-hard.RDS"))
p3 <- readRDS(paste0(outdir, "sex-uc-soft.RDS"))
p4 <- readRDS(paste0(outdir, "sex-uc-hard.RDS"))

p <- list(p1, p2, p3, p4)

lay <- rbind(
  c(1, 2),
  c(3, 4)
)

survs <- lapply(
  p,
  survminer:::.build_ggsurvplot,
  surv.plot.height = NULL,
  risk.table.height = NULL,
  ncensor.plot.height = NULL
)

survs <- do.call(
  gridExtra::marrangeGrob,
  list(grobs = survs, layout_matrix = lay, top = NULL)
)

cairo_pdf(
  "plots/arranged/FigureS8.pdf",
  width = 16.5 * 5 / 8,
  height = 18.5 * 5 / 8
)
survs
dev.off()

png(
  "plots/arranged/FigureS8.png",
  width = 16.5 * 5 / 8,
  height = 18.5 * 5 / 8,
  units = "in",
  res = 300
)
survs
dev.off()


###################
#### Figure S9 ####
###################

p1 <- readRDS(paste0(outdir, "imd-cd-soft.RDS"))
p2 <- readRDS(paste0(outdir, "imd-cd-hard.RDS"))
p3 <- readRDS(paste0(outdir, "imd-uc-soft.RDS"))
p4 <- readRDS(paste0(outdir, "imd-uc-hard.RDS"))

p <- list(p1, p2, p3, p4)

lay <- rbind(
  c(1, 2),
  c(3, 4)
)

survs <- lapply(
  p,
  survminer:::.build_ggsurvplot,
  surv.plot.height = NULL,
  risk.table.height = NULL,
  ncensor.plot.height = NULL
)

survs <- do.call(
  gridExtra::marrangeGrob,
  list(grobs = survs, layout_matrix = lay, top = NULL)
)

cairo_pdf(
  "plots/arranged/FigureS9.pdf",
  width = 16.5 * 5 / 8,
  height = 18.5 * 5 / 8
)
survs
dev.off()

png(
  "plots/arranged/FigureS9.png",
  width = 16.5 * 5 / 8,
  height = 18.5 * 5 / 8,
  units = "in",
  res = 300
)
survs
dev.off()


####################
#### Figure S10 ####
####################

p1 <- readRDS(paste0(outdir, "meat-overall-uc-hard.RDS"))
p2 <- readRDS(paste0(outdir, "redMeatIntake-uc-hard.RDS"))
p3 <- readRDS(paste0(outdir, "whiteMeatIntake-uc-hard.RDS"))

p <- list(p1, p2, p3)

lay <- rbind(
  c(1, 1),
  c(2, 3)
)

survs <- lapply(
  p,
  survminer:::.build_ggsurvplot,
  surv.plot.height = NULL,
  risk.table.height = NULL,
  ncensor.plot.height = NULL
)

survs <- do.call(
  gridExtra::marrangeGrob,
  list(grobs = survs, layout_matrix = lay, top = NULL)
)

cairo_pdf(
  "plots/arranged/FigureS10.pdf",
  width = 16.5 * 5 / 8,
  height = 18.5 * 5 / 8
)
survs
dev.off()

png(
  "plots/arranged/FigureS10.png",
  width = 16.5 * 5 / 8,
  height = 18.5 * 5 / 8,
  units = "in",
  res = 300
)
survs
dev.off()



####################
#### Figure S11 ####
####################

p1 <- readRDS(paste0(outdir, "fibre-cd-soft.RDS"))
p2 <- readRDS(paste0(outdir, "fibre-cd-hard.RDS"))
p3 <- readRDS(paste0(outdir, "fibre-uc-soft.RDS"))
p4 <- readRDS(paste0(outdir, "fibre-uc-hard.RDS"))

p <- list(p1, p2, p3, p4)

lay <- rbind(
  c(1, 2),
  c(3, 4)
)

survs <- lapply(
  p,
  survminer:::.build_ggsurvplot,
  surv.plot.height = NULL,
  risk.table.height = NULL,
  ncensor.plot.height = NULL
)

survs <- do.call(
  gridExtra::marrangeGrob,
  list(grobs = survs, layout_matrix = lay, top = NULL)
)

cairo_pdf(
  "plots/arranged/FigureS11.pdf",
  width = 16.5 * 5 / 8,
  height = 18.5 * 5 / 8
)
survs
dev.off()

png(
  "plots/arranged/FigureS11.png",
  width = 16.5 * 5 / 8,
  height = 18.5 * 5 / 8,
  units = "in",
  res = 300
)
survs
dev.off()



####################
#### Figure S12 ####
####################

p1 <- readRDS(paste0(outdir, "pufa-cd-soft.RDS"))
p2 <- readRDS(paste0(outdir, "pufa-cd-hard.RDS"))
p3 <- readRDS(paste0(outdir, "pufa-uc-soft.RDS"))
p4 <- readRDS(paste0(outdir, "pufa-uc-hard.RDS"))

p <- list(p1, p2, p3, p4)

lay <- rbind(
  c(1, 2),
  c(3, 4)
)

survs <- lapply(
  p,
  survminer:::.build_ggsurvplot,
  surv.plot.height = NULL,
  risk.table.height = NULL,
  ncensor.plot.height = NULL
)

survs <- do.call(
  gridExtra::marrangeGrob,
  list(grobs = survs, layout_matrix = lay, top = NULL)
)

cairo_pdf(
  "plots/arranged/FigureS12.pdf",
  width = 16.5 * 5 / 8,
  height = 18.5 * 5 / 8
)
survs
dev.off()

png(
  "plots/arranged/FigureS12.png",
  width = 16.5 * 5 / 8,
  height = 18.5 * 5 / 8,
  units = "in",
  res = 300
)
survs
dev.off()

####################
#### Figure S14 ####
####################

p1 <- readRDS(paste0(outdir, "upf-cd-soft.RDS"))
p2 <- readRDS(paste0(outdir, "upf-cd-hard.RDS"))
p3 <- readRDS(paste0(outdir, "upf-uc-soft.RDS"))
p4 <- readRDS(paste0(outdir, "upf-uc-hard.RDS"))

p <- list(p1, p2, p3, p4)

lay <- rbind(
  c(1, 2),
  c(3, 4)
)

survs <- lapply(
  p,
  survminer:::.build_ggsurvplot,
  surv.plot.height = NULL,
  risk.table.height = NULL,
  ncensor.plot.height = NULL
)

survs <- do.call(
  gridExtra::marrangeGrob,
  list(grobs = survs, layout_matrix = lay, top = NULL)
)

cairo_pdf(
  "plots/arranged/FigureS14.pdf",
  width = 16.5 * 5 / 8,
  height = 18.5 * 5 / 8
)
survs
dev.off()

png(
  "plots/arranged/FigureS14.png",
  width = 16.5 * 5 / 8,
  height = 18.5 * 5 / 8,
  units = "in",
  res = 300
)
survs
dev.off()

####################
#### Figure S?? ####
####################
p1 <- readRDS(paste0(outdir, "fc-cont-cd-soft.RDS"))
p2 <- readRDS(paste0(outdir, "fc-cont-cd-hard.RDS"))
p3 <- readRDS(paste0(outdir, "fc-cont-uc-soft.RDS"))
p4 <- readRDS(paste0(outdir, "fc-cont-uc-hard.RDS"))

p <- (p1 + p2) / (p3 + p4) +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom",
        plot.tag = element_text(size = 18, face = "bold")) &
  xlab("Log (FC (µg/g))") & xlim(2.5, 6.5) & ylim(0.4, 2.5)


png(
  "plots/arranged/FigureSFC.png",
  width = 14.5 * 5 / 8,
  height = 14.5 * 5 / 8,
  units = "in",
  res = 300
)
p
dev.off()

cairo_pdf(
  "plots/arranged/FigureSFC.pdf",
  width = 14.5 * 5 / 8,
  height = 14.5 * 5 / 8
)
p
dev.off()
