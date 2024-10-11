# Load necessary libraries
library(ggplot2)
library(ggsci)
library(dplyr)

# The plot_bars function and its wrappers plot_bars_by_sample and
# plot_bars_by_group are designed to generate bar plots in R using the ggplot2
# package with Nature journal styling. T

# Core function: plot_bars
# - Inputs: Customizable to fill bars based on sample or group.
# - Outputs: Saves the bar plot as a PNG file.

# Wrapper functions:
# - plot_bars_by_sample: Fills bars by sample.
# - plot_bars_by_group: Fills bars by group.

# This function creates a bar plot with options to customize axis labels, title, and color fill.
# Parameters:
# - data: Dataframe containing the data for plotting.
# - x: Column name for the x-axis (e.g., samples).
# - y: Column name for the y-axis (e.g., protein counts).
# - fill: Column name to determine bar fill colors (can be x or a grouping variable).
# - group: Optional grouping variable (used to control legend display).
# - output_file: Name of the output file (default: "barplot.png").
# - x_axis_name, y_axis_name: Optional custom names for the x and y axis.
# - plot_title: Optional title for the plot.

plot_bars <- function(data,
                      x,
                      y,
                      fill,
                      group = NULL,
                      output_file = "barplot.png",
                      x_axis_name = NULL,
                      y_axis_name = NULL,
                      plot_title = NULL) {
  
  # Create ggplot object
  p <- ggplot(data, aes_string(x = x, y = y, fill = fill)) +
    geom_bar(stat = "identity", color = "black", width = 0.7) +
    geom_text(aes_string(label = y), vjust = -0.5, color = "black", size = 5, family = "Arial") +
    ggsci::scale_fill_npg() +  # Nature journal style colors
    theme_minimal(base_size = 16) +
    theme(
      axis.title = element_text(size = 16, face = "bold", family = "Arial"),
      axis.text = element_text(size = 14, family = "Arial"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "plain"),
      legend.position = ifelse(!is.null(group) && fill == group, "right", "none"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, family = "Arial"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black", size = 0.75),
      axis.ticks.length = unit(0.25, "cm")
    ) +
    labs(title = plot_title, x = x_axis_name, y = y_axis_name)
  
  # Adjust y-axis limits to prevent labels from being cropped
  y_max <- max(data[[y]], na.rm = TRUE)
  p <- p + ylim(0, y_max * 1.1)  # Add 10% space above bars
  
  # Save plot to file
  ggsave(filename = output_file, plot = p, width = 8, height = 6, dpi = 300)
}

# Wrapper for plotting by sample (fill is set to x)
plot_bars_by_sample <- function(data,
                                x,
                                y,
                                output_file = "barplot_by_sample.png",
                                x_axis_name = NULL,
                                y_axis_name = NULL,
                                plot_title = NULL) {
  plot_bars(
    data, x = x, y = y, fill = x, group = NULL,
    output_file = output_file,
    x_axis_name = x_axis_name,
    y_axis_name = y_axis_name,
    plot_title = plot_title
  )
}

# Wrapper for plotting by group (fill is set to group column)
plot_bars_by_group <- function(data,
                               x,
                               y,
                               group,
                               output_file = "barplot_by_group.png",
                               x_axis_name = NULL,
                               y_axis_name = NULL,
                               plot_title = NULL) {
  plot_bars(
    data, x = x, y = y, fill = group, group = group,
    output_file = output_file,
    x_axis_name = x_axis_name,
    y_axis_name = y_axis_name,
    plot_title = plot_title
  )
}

## Not run:
# Example usage:
# Simulated data with grouping

# df <- data.frame(
#   Sample = paste0("Sample_", 1:6),
#   Protein_ID_Number = c(3200, 1600, 2500, 4600, 2700, 3156),
#   Group = c(rep("A", 3), rep("B", 3))
# )
# 
# # Plot by sample
# plot_bars_by_sample(
#   df,
#   x = "Sample",
#   y = "Protein_ID_Number",
#   output_file = "barplot_by_sample.png",
#   x_axis_name = "Sample",
#   y_axis_name = "Protein Counts",
#   plot_title = "Protein Identification by Sample"
# )
# 
# # Plot by group
# plot_bars_by_group(
#   df,
#   x = "Sample",
#   y = "Protein_ID_Number",
#   group = "Group",
#   output_file = "barplot_by_group.png",
#   x_axis_name = "Sample",
#   y_axis_name = "Protein Counts",
#   plot_title = "Protein Identification by Group"
# )

## End(Not run)
