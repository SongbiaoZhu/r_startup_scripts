#' Plot Stacked Barplot
#'
#' This function generates a stacked barplot where each bar represents a sample,
#' and the segments within the bar represent categories such as "High", "Peak Found", or "NA".
#' Optionally, numeric values can be displayed inside the bars.
#'
#' @param data Data frame containing the data for plotting.
#' @param x_var Column name (as a string) representing the variable on the x-axis (e.g., sample).
#' @param y_var Column name (as a string) representing the variable on the y-axis (e.g., counts).
#' @param fill_var Column name (as a string) representing the variable used to fill the stack (e.g., category).
#' @param fill_order Optional vector specifying the order of categories in the fill variable (e.g., from bottom to top, set c("High", "Peak Found", "NA")).
#' @param output_file File name where the plot will be saved (e.g., "stacked_barplot.png").
#' @param title Optional plot title (default is NULL, auto-generated from y_var and fill_var).
#' @param x_axis_name Name for the x-axis (default is the y_var name).
#' @param y_axis_name Name for the y-axis (default is the y_var name).
#' @param fill_name Legend name for the stacked categories (default is the fill_var name).
#' @param show_values Logical, whether to show numeric values inside the bars (default is FALSE).
#'
#' @return A publication-ready stacked barplot saved as a PNG file.
#' @export
#'

plot_stacked_barplot <- function(data,
                                 x_var,
                                 y_var,
                                 fill_var,
                                 fill_order = NULL,
                                 output_file,
                                 title = NULL,
                                 x_axis_name = NULL,
                                 y_axis_name = NULL,
                                 fill_name = NULL,
                                 show_values = FALSE) {
  # Load necessary libraries
  library(ggplot2)
  
  # Nature journal style color palette with extendability
  base_colors <- c("#56B4E9",
                   "#E69F00",
                   "#009E73",
                   "#F0E442",
                   "#0072B2",
                   "#D55E00",
                   "#CC79A7")
  color_palette <- colorRampPalette(rev(base_colors))(length(unique(data[[fill_var]])))
  
  # Nature journal style settings
  text_size <- 14  # Set default text size
  figure_size <- c(7, 5)  # Set default figure size in inches
  
  # Auto-generate plot title if not provided
  if (is.null(title)) {
    title <- paste(y_var, "by", fill_var)
  }
  
  # Auto-generate x-axis name if not provided
  if (is.null(x_axis_name)) {
    x_axis_name <- x_var  # Use the x variable name if not provided
  }
  
  # Auto-generate y-axis name if not provided
  if (is.null(y_axis_name)) {
    y_axis_name <- y_var  # Use the y variable name if not provided
  }
  
  # Auto-generate legend name (fill) if not provided
  if (is.null(fill_name)) {
    fill_name <- fill_var  # Use the fill variable name if not provided
  }
  
  # Order the fill categories if fill_order is provided (reverse order for proper stacking)
  if (!is.null(fill_order)) {
    data[[fill_var]] <- factor(data[[fill_var]], levels = rev(fill_order))
  }
  
  # Create the stacked barplot
  p <- ggplot(data, aes_string(x = x_var, y = y_var, fill = fill_var)) +
    geom_bar(stat = "identity", width = 0.7) +  # Create stacked bars
    scale_fill_manual(values = color_palette, name = fill_name) +  # Custom color palette and legend title
    labs(title = title, x = x_axis_name, y = y_axis_name) +  # Set plot title and axis labels
    theme_classic() + 
    theme(
      text = element_text(size = text_size, family = "Arial"),
      plot.title = element_text(hjust = 0.5, size = text_size + 2),
      axis.title = element_text(size = text_size),
      axis.text = element_text(size = text_size - 2),
      legend.title = element_text(size = text_size - 1),
      legend.text = element_text(size = text_size - 2)
    )
  
  # Add numeric values on bars if show_values is TRUE
  if (show_values) {
    p <- p + geom_text(
      aes_string(label = y_var),
      position = position_stack(vjust = 0.5),
      color = "black",
      size = text_size - 8,
      check_overlap = TRUE
    )
  }
  
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
if (interactive()) {
  # Example 1: Creating a stacked barplot with numeric values displayed on the bars.
  # This dataset contains three samples, each with counts for three categories: "High", "Peak Found", and "NA".
  # Numeric values will be shown inside the bars, as none of the values are too small to be visible.
  data <- data.frame(
    Sample = rep(c("Sample_1", "Sample_2", "Sample_3"), each = 3),
    Count = c(30, 20, 10, 40, 30, 20, 50, 40, 30),
    Category = rep(c("High", "Peak Found", "NA"), times = 3)
  )
  
  # Generating the stacked barplot with ordered fill categories and values displayed on bars.
  # The categories are ordered from "High" at the bottom to "NA" at the top.
  plot_stacked_barplot(
    data = data,
    x_var = "Sample",
    y_var = "Count",
    fill_var = "Category",
    fill_order = c("High", "Peak Found", "NA"),
    # Controls the order of the stacked categories
    output_file = "stacked_barplot_example1.png",
    title = "Protein Identifications by Sample",
    # Title of the plot
    x_axis_name = "",
    # Label for x-axis
    y_axis_name = "Counts",
    # Label for y-axis
    show_values = TRUE  # Displays numeric values inside the bars
  )
  
  # Example 2: Creating a stacked barplot with large counts, where small values are automatically omitted.
  # This dataset contains larger counts for each sample, with smaller numbers for some categories.
  # Numeric values will only be displayed for the larger counts to avoid cluttering the plot.
  data <- data.frame(
    Sample = rep(c("Sample_1", "Sample_2", "Sample_3"), each = 3),
    Count = c(1500, 200, 50, 1900, 150, 30, 3200, 120, 20),
    Category = rep(c("High", "Peak Found", "NA"), times = 3)
  )
  
  # Generating a stacked barplot. Numeric values are displayed for larger counts but are automatically omitted
  # for very small numbers, making the plot cleaner and easier to interpret.
  plot_stacked_barplot(
    data = data,
    x_var = "Sample",
    y_var = "Count",
    fill_var = "Category",
    fill_order = c("High", "Peak Found", "NA"),
    # Order of the stacked categories
    output_file = "stacked_barplot_example2.png",
    title = "Protein Identifications by Sample with Large Counts",
    x_axis_name = "",
    # x-axis label
    y_axis_name = "Counts",
    # y-axis label
    show_values = TRUE  # Numeric values are displayed where appropriate
  )
}
