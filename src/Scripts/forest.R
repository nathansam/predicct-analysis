library(forestplot)
library(tidyverse)

if (file.exists("/docker")) {
  # If running in docker
  outdir <- "data/processed/"
} else {
  # Run on OS directly
  outdir <- "/Volumes/igmm/cvallejo-predicct/predicct/processed/"
}

cd.clin.forest <- readRDS(paste0(outdir, "cd-clin-biochem.RDS"))
cd.hard.forest <- readRDS(paste0(outdir, "cd-hard-biochem.RDS"))
uc.clin.forest <- readRDS(paste0(outdir, "uc-clin-biochem.RDS"))
uc.hard.forest <- readRDS(paste0(outdir, "uc-hard-biochem.RDS"))

cd.clin.forest$Variable <- rownames(cd.clin.forest)
uc.clin.forest$Variable <- rownames(uc.clin.forest)
cd.clin.forest$Diagnosis <- "CD"
uc.clin.forest$Diagnosis <- "UC/IBDU"

clin.forest <- rbind(cd.clin.forest, uc.clin.forest)

clin.forest <- clin.forest %>%
  mutate(color_group = if_else(p < 0.05, "Significant", "Not Significant"))

dodge <- position_dodge(width = 0.8)
color_palette <- c("Significant" = "red", "Not Significant" = "black")


p1 <- clin.forest %>%
  ggplot(aes(
    x = Variable,
    y = HR,
    ymin = Lower95,
    ymax = Upper95,
    shape = Diagnosis,
    color = color_group
  )) +
  # For the point estimate and CI range
  geom_pointrange(position = dodge, size = 0.7) +
  # Adds a reference line at y = 1 (null effect)
  geom_hline(yintercept = 1, linetype = 2) +
  # Flip coordinates for a horizontal forest plot
  coord_flip() +
  labs(
    y = "Hazard Ratio (95% CI)",
    x = "Psychosocial Variables",
    title = "Clinical Flare - Survival Analysis (Cox Regression)"
  ) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    # Adjust the size and position of the title
    plot.title = element_text(size = 8, hjust = 0.5, vjust = -10),
    plot.subtitle = element_text(size = 6, hjust = 0.5, vjust = -15)
  ) +
  scale_color_manual(values = color_palette) +
  scale_shape_manual(values = c("UC/IBDU" = 17, "CD" = 16)) +
  scale_x_discrete(limits = rev(levels(clin.forest$Variable))) +
  guides(
    color = guide_legend(
      title = "Colour Legend",
      override.aes = list(shape = 15, linetype = 0)
    ),
    shape = guide_legend(
      title = "IBD Diagnosis",
      override.aes = list(linetype = 0),
      reverse = TRUE
    )
  ) +
  theme_minimal()

print(p1)

########

new.df <- data.frame(
  Variable = character(),
  HR = numeric(),
  Lower95 = numeric(),
  Upper95 = numeric(),
  p_value = numeric(),
  color_group = character()
)


clin.forest$CI <- paste0(
  round(clin.forest$Lower95, 2),
  "--",
  round(clin.forest$Upper95, 2)
)

new.df <- data.frame(
  Variable = character(),
  HR = numeric(),
  Lower95 = numeric(),
  Upper95 = numeric(),
  p = numeric(),
  Diagnosis = character(),
  color_group = character()
)

for (var in unique(clin.forest$Variable)) {
  temp.df <- data.frame(
    Variable = var,
    HR = NA,
    Lower95 = NA,
    Upper95 = NA,
    p = NA,
    Diagnosis = NA,
    color_group = NA
  )

  temp <- subset(clin.forest, Variable == var & Diagnosis == "CD")[, c(-5, -8)]
  if (nrow(temp) > 0) {
    temp.df <- rbind(
      temp.df,
      cbind(
        data.frame(Variable = "  CD"),
        temp
      )
    )
  }

  temp <- subset(clin.forest, Variable == var & Diagnosis == "UC/IBDU")[, c(
    -5,
    -8
  )]

  if (nrow(temp) > 0) {
    temp.df <- rbind(
      temp.df,
      cbind(
        data.frame(Variable = "  UC/IBDU"),
        temp
      )
    )
  }
  new.df <- rbind(new.df, temp.df)
}


new.df$HR <- round(new.df$HR, 2)
new.df$p <- round(new.df$p, 2)
new.df$CI <- paste0(round(new.df$Lower95, 2), "–", round(new.df$Upper95, 2))

new.df <- new.df %>% select(-Diagnosis)

new.df <- rbind(
  data.frame(
    Variable = c(" Variables", NA),
    HR = c("HR", NA),
    Lower95 = c("Lower95", NA),
    Upper95 = c("Upper95", NA),
    p = c("p-value", NA),
    color_group = c("color_group", NA),
    CI = c("95% CI", NA)
  ),
  new.df
)

new.df$HR.val <- as.numeric(new.df$HR)
new.df$Lower95 <- as.numeric(new.df$Lower95)
new.df$Upper95 <- as.numeric(new.df$Upper95)


new.df$CI <- plyr::mapvalues(new.df$CI, from = "NA–NA", to = "")

cairo_pdf("plots/arranged/clin-forest.pdf", width = 8.3, height = 13)
forestplot(
  labeltext = cbind(new.df$Variable, new.df$HR, new.df$CI, new.df$p),
  mean = c(new.df$HR.val),
  lower = c(new.df$Lower95),
  upper = c(new.df$Upper95),
  lwd.ci = 2,
  ci.vertices = TRUE,
  ci.vertices.height = 0.4,
  zero = 1,
  graph.pos = 2,
  hrzl_lines = list(
    "5" = gpar(
      lwd = 60,
      lineend = "butt",
      columns = seq(1, 5),
      col = "#99999922"
    ),
    "8" = gpar(
      lwd = 60,
      lineend = "butt",
      columns = seq(1, 5),
      col = "#99999922"
    )
  ),
  txt_gp = fpTxtGp(
    label = gpar(cex = 1.25),
    ticks = gpar(cex = 1.1),
    xlab = gpar(cex = 1.2),
    title = gpar(cex = 1.2)
  ),
  col = fpColors(box = "black", lines = "black", zero = "gray50"),
  cex = 0.9,
  lineheight = "auto",
  boxsize = 0.5,
  colgap = unit(6, "mm"),
  title = "Hazard Ratio",
  new_page = FALSE
)
invisible(dev.off())


png(
  "plots/arranged/clin-forest.png",
  width = 8.3,
  height = 13,
  units = "in",
  res = 300
)
forestplot(
  labeltext = cbind(new.df$Variable, new.df$HR, new.df$CI, new.df$p),
  mean = c(new.df$HR.val),
  lower = c(new.df$Lower95),
  upper = c(new.df$Upper95),
  lwd.ci = 2,
  ci.vertices = TRUE,
  ci.vertices.height = 0.4,
  zero = 1,
  graph.pos = 2,
  hrzl_lines = list(
    "5" = gpar(
      lwd = 60,
      lineend = "butt",
      columns = seq(1, 5),
      col = "#99999922"
    ),
    "8" = gpar(
      lwd = 60,
      lineend = "butt",
      columns = seq(1, 5),
      col = "#99999922"
    )
  ),
  txt_gp = fpTxtGp(
    label = gpar(cex = 1.25),
    ticks = gpar(cex = 1.1),
    xlab = gpar(cex = 1.2),
    title = gpar(cex = 1.2)
  ),
  col = fpColors(box = "black", lines = "black", zero = "gray50"),
  cex = 0.9,
  lineheight = "auto",
  boxsize = 0.5,
  colgap = unit(6, "mm"),
  title = "Hazard Ratio",
  new_page = FALSE
)
invisible(dev.off())
