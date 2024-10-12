# Scatter Plotting Function with Customizable Settings
# This script provides a function for generating a scatter plot with a
# regression line and correlation coefficient.
# It includes customizable axis titles, plot title, axis limits, and breaks for enhanced flexibility.
# Example code is provided at the end to help newcomers, but it won't run when the script is sourced.

# Install and load packages
library(ggplot2)
library(ggsci)
library(scales)

# Scatter Plot Function
# This function plots the scatter plot with a regression line and the Pearson
# correlation coefficient (R value).
# Arguments:
#   - data: Data frame containing the variables Sample1 and Sample2 for plotting.
#   - x_title: X-axis label (default: "Sample 1 (Log2 Abundance)").
#   - y_title: Y-axis label (default: "Sample 2 (Log2 Abundance)").
#   - plot_title: Main title for the plot (default: "Correlation of Log2 Abundances").
#   - output_file: Name of the output PNG file (default: "scatter_plot.png").
#   - range.x: X-axis limits (default: automatically determined from data).
#   - range.y: Y-axis limits (default: automatically determined from data).
#   - breaks.x: Break points for the X-axis (default: automatically determined from range.x).
#   - breaks.y: Break points for the Y-axis (default: automatically determined from range.y).
plot_scatter_with_Rvalue <- function(data,
                                     x_title = "Sample 1 (Log2 Abundance)",
                                     y_title = "Sample 2 (Log2 Abundance)",
                                     plot_title = "Correlation of Log2 Abundances",
                                     output_file = "scatter_plot.png",
                                     range.x = NULL,
                                     range.y = NULL,
                                     breaks.x = NULL,
                                     breaks.y = NULL) {
  # Calculate correlation coefficient (R value)
  correlation <- cor(data$Sample1, data$Sample2)
  
  # Set axis limits based on data if not provided
  if (is.null(range.x))
    range.x <- range(data$Sample1)
  if (is.null(range.y))
    range.y <- range(data$Sample2)
  
  # Set axis breaks based on limits if not provided
  if (is.null(breaks.x))
    breaks.x <- pretty(range.x)
  if (is.null(breaks.y))
    breaks.y <- pretty(range.y)
  
  # Create scatter plot
  p <- ggplot(data, aes(x = Sample1, y = Sample2)) +
    geom_point(color = "black",
               size = 1,
               alpha = 0.5) +  # Points with transparency
    geom_smooth(method = "lm",
                color = "#FF6347",
                se = FALSE) +  # Regression line
    annotate(
      "text",
      x = min(range.x),
      y = max(range.y),
      label = paste("R =", round(correlation, 2)),
      hjust = 0,
      vjust = 1,
      size = 5
    ) +
    theme_minimal(base_size = 16) +
    theme(
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      plot.title = element_text(
        size = 18,
        face = "bold",
        hjust = 0.5
      ),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.25, "cm")
    ) +
    labs(title = plot_title, x = x_title, y = y_title) +
    scale_x_continuous(limits = range.x, breaks = breaks.x) +
    scale_y_continuous(limits = range.y, breaks = breaks.y)
  
  # Save plot
  ggsave(
    output_file,
    plot = p,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  message(paste("Plot saved as", output_file))
}

# Example code to test the function
# Uncomment the following block to run an example plot

# Example Code:
# Simulate data and generate scatter plot
# Example 1: manually set axis limits and breaks
# Example 2: automatically set axis limits and breaks
set.seed(42)
sample_size <- 5000
sample1_abundance <- rnorm(sample_size, mean = 100000, sd = 3000)
sample2_abundance <- sample1_abundance + rnorm(sample_size, mean = 1000, sd = 1000)
log2_sample1 <- log2(sample1_abundance)
log2_sample2 <- log2(sample2_abundance)
data <- data.frame(Sample1 = log2_sample1, Sample2 = log2_sample2)
plot_scatter_with_Rvalue(
  data,
  range.x = c(16.5, 16.75),
  range.y = c(16.5, 16.75),
  breaks.x = seq(16.5, 16.75, 0.05),
  breaks.y = seq(16.5, 16.75, 0.05),
  x_title = "Custom X Axis Title",
  y_title = "Custom Y Axis Title",
  plot_title = "Custom Plot Title",
  output_file = "scatter_plot_manual_limits.png"
)
plot_scatter_with_Rvalue(
  data,
  x_title = "Custom X Axis Title",
  y_title = "Custom Y Axis Title",
  plot_title = "Custom Plot Title",
  output_file = "scatter_plot_auto_limits.png"
)