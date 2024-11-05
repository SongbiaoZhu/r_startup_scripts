#' Plot Volcano with Straight Threshold in Nature Journal Style
#'
#' This function plots a volcano plot from a dataset, producing a publication-ready figure
#' in Nature journal style. The plot is saved as a PNG file.
#'
#' @param data Data frame containing the data.
#' @param log2fc Column name (as string) representing the log2 fold change values.
#' @param p_value Column name (as string) representing the p-values.
#' @param output_file Name of the file where the plot will be saved (e.g., "volcano_plot.png").
#' @param x_label Label for the x-axis (default is "Fold Change").
#' @param y_label Label for the y-axis (default is "p-value").
#' @param title Plot title (default is "Treat vs Control").
#' @param p_threshold Threshold for significant p-values (default is 0.05).
#' @param fc_threshold Threshold for significant fold change (default is 1.0).
#'
#' @return A publication-ready volcano plot saved as a PNG file.

plot_volcano_straight <- function(data,
                                  log2fc,
                                  p_value,
                                  output_file,
                                  x_label = "Fold Change",
                                  y_label = "p-value",
                                  title = "Treat vs Control",
                                  p_threshold = 0.05,
                                  fc_threshold = 1.0) {
  # Load necessary libraries
  library(ggplot2)
  library(dplyr)
  
  # Nature journal style settings
  text_size <- 14  # Set default text size
  figure_size <- c(6, 4.5)  # Set default figure size in inches
  
  # Add significance and regulation status based on thresholds
  data <- data %>%
    mutate(
      Regulation = case_when(
        !!as.name(log2fc) > fc_threshold &
          !!as.name(p_value) < p_threshold ~ "Upregulated",!!as.name(log2fc) < -fc_threshold &
          !!as.name(p_value) < p_threshold ~ "Downregulated",
        TRUE ~ "Not Significant"
      )
    )
  
  # Ensure that Regulation has all levels, even if some are missing in the data
  data$Regulation <- factor(data$Regulation,
                            levels = c("Upregulated", "Downregulated", "Not Significant"))
  
  # Generate the volcano plot
  p <- ggplot(data, aes_string(
    x = log2fc,
    y = sprintf("-log10(%s)", p_value),
    color = "Regulation"
  )) +
    geom_point(alpha = 0.6, size = 1.5) +
    scale_color_manual(
      values = c(
        "Upregulated" = "#D55E00",
        "Downregulated" = "#0072B2",
        "Not Significant" = "#999999"
      ),
      drop = FALSE  # Ensures that colors are used even if some categories are missing
    ) +
    labs(
      title = title,
      x = bquote(log[2] * "(" * .(x_label) * ")"),
      # log2(Fold Change)
      y = bquote(-log[10] * "(" * .(y_label) * ")")  # -log10(p-value)
    ) +
    theme_classic() +
    theme(
      text = element_text(size = text_size, family = "Arial"),
      plot.title = element_text(hjust = 0.5, size = text_size + 2),
      axis.title = element_text(size = text_size),
      axis.text = element_text(size = text_size - 2),
      panel.grid = element_blank()
    )
  
  # Save the plot as a PNG file
  ggsave(
    output_file,
    plot = p,
    width = figure_size[1],
    height = figure_size[2],
    dpi = 300
  )
}

#' @examples
# if (interactive()) {
#   # Load dataset from the provided URL
#   data_url <- "https://gitee.com/zhu_song_biao/r_startup_scripts/raw/main/example_data/WT_vs_KI-HFHC_example.csv"
#   data <- read.csv(data_url)
#   
#   # Example usage of the plot_volcano function
#   plot_volcano_straight(data,
#                         log2fc = "log2FC",
#                         p_value = "pvalue",
#                         output_file = "volcano_straight_example.png")
# }
