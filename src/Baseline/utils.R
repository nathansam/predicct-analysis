#' @title Plot a missingness heatmap: now with extra colour
#' @description An adaption of finalfit::missing_plot which colours missing NA
#'   values by a given variable (such as \code{SiteName}).
#' @inheritParams finalfit::missing_plot
#' @param col.name String. Variable which stratifies the missingness. I.E
#' SiteName for stratifying by site.
#' @param save Logical. If TRUE then a frequency table of NA values by site will
#'   be saved as a csv file to the current working directory
#' @return A ggplot2 object
#' @export
missing_plot2 <- function(
  .data,
  col.name = "SiteName",
  dependent = NULL,
  explanatory = NULL,
  use_labels = TRUE,
  title = NULL,
  plot_opts = NULL,
  save = FALSE
) {
  requireNamespace("ggplot2")
  if (is.null(dependent) && is.null(explanatory)) {
    df.in <<- .data
  } else {
    df.in = dplyr::select(.data, dependent, explanatory)
  }
  if (use_labels) {
    vlabels = finalfit::extract_labels(df.in)$vfill
  }

  sitenames <- unique(df.in[, col.name])

  df.in <- data.frame(lapply(df.in, as.character), stringsAsFactors = FALSE)
  for (i in 1:nrow(df.in)) {
    site.no <- df.in[i, col.name]
    for (j in 1:ncol(df.in)) {
      if (is.na(df.in[i, j]) == FALSE) {
        df.in[i, j] <- "Not missing"
      } else {
        df.in[i, j] <- site.no
      }
    }
  }

  counts <- data.frame()
  for (i in sitenames) {
    site.count <- 0
    for (j in 1:ncol(df.in)) {
      site.count <- site.count + sum(df.in[, j] == i)
    }
    counts <- rbind(counts, data.frame(Site = i, Count = site.count))
  }

  counts <- counts[order(counts$Count, decreasing = TRUE), ]

  if (save) {
    file.name <- paste(deparse(substitute(.data)), "_NA_count.csv", sep = "")
    utils::write.csv(counts, file.name)
  }
  high.na <- c("Not missing", as.character(counts[1:5, 1]))

  for (i in 1:nrow(df.in)) {
    for (j in 1:ncol(df.in)) {
      if (df.in[i, j] %in% high.na == FALSE) {
        df.in[i, j] <- "Other"
      }
    }
  }

  n.color <- length(high.na) + 1

  plot.colors <- c(
    "#102033",
    "#ff595e",
    "#ff924c",
    "#ffca3a",
    "#8ac926",
    "#1982c4",
    "#6a4c93"
  )

  df.in$.id = as.numeric(rownames(df.in))

  plot_df <- tidyr::gather(df.in, "var", "value", -.id, factor_key = TRUE)

  # Include "Other" in the legend order
  legend_levels <- c(high.na, "Other")

  # Set factor levels to ensure "Not missing" appears first in legend
  plot_df$value <- factor(plot_df$value, levels = legend_levels)

  if (is.null(title)) {
    title = paste(deparse(substitute(.data)), "Missing Values Map")
  }

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .id, y = forcats::fct_rev(var), fill = value)
  )
  p <- p +
    ggplot2::scale_fill_manual(values = plot.colors, breaks = legend_levels, name = "Recruitment site")
  p <- p + ggplot2::geom_raster() + ggplot2::xlab("Observation")
  p <- p +
    ggplot2::scale_y_discrete(
      "",
      breaks = rev(levels(plot_df$var)),
      labels = rev(vlabels)
    )
  p <- p + ggplot2::theme_minimal()
  p <- p + ggplot2::ggtitle(title) + plot_opts
  p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 1))
  p
}
