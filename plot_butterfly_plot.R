#' Plot Butterfly Plot with Separate Axis Scales for Proteins and Peptides
#'
#' This function generates a butterfly plot with sample names in the middle, protein
#' numbers on the left, and peptide numbers on the right. Separate scales for the
#' left and right axes are used to handle different value ranges. The plot is saved as a PNG file.
#'
#' @param data Data frame containing the data for plotting.
#' @param sample_var Column name (as a string) representing the sample names to be displayed in the middle of the plot.
#' @param left_var Column name (as a string) representing the left-side variable (e.g., protein numbers).
#' @param right_var Column name (as a string) representing the right-side variable (e.g., peptide numbers).
#' @param output_file File name where the plot will be saved (e.g., "butterfly_plot.png").
#' @param title Optional plot title (default is NULL, auto-generated from left_var and right_var).
#' @param legend_left Name for the left legend category (default is NULL, auto-generated from left_var).
#' @param legend_right Name for the right legend category (default is NULL, auto-generated from right_var).
#' @param left_text_vjust Vertical adjustment for the text labels on the left bars (default is 0).
#' @param left_text_hjust Horizontal adjustment for the text labels on the left bars (default is 0).
#' @param right_text_vjust Vertical adjustment for the text labels on the right bars (default is 0).
#' @param right_text_hjust Horizontal adjustment for the text labels on the right bars (default is 1).
#'
#' @return A publication-ready butterfly plot saved as a PNG file.

plot_butterfly_plot <- function(data,
                                sample_var,
                                left_var,
                                right_var,
                                output_file,
                                title = NULL,
                                legend_left = NULL,
                                legend_right = NULL,
                                left_text_vjust = 0,
                                left_text_hjust = 0,
                                right_text_vjust = 0,
                                right_text_hjust = 1) {
  # Load necessary libraries
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  # Nature journal color palette (base colors)
  left_color <- "#56B4E9"  # Light blue for left bars (e.g., proteins)
  right_color <- "#E69F00" # Orange for right bars (e.g., peptides)
  
  # Nature journal style settings
  text_size <- 14  # Set default text size
  figure_size <- c(7, 5)  # Set default figure size in inches
  
  # Auto-generate plot title if not provided
  if (is.null(title)) {
    title <- paste(left_var, "and", right_var)
  }
  
  # Set legend category names based on inputs or defaults
  if (is.null(legend_left)) {
    legend_left <- left_var  # Use the left variable name if legend_left is NULL
  }
  
  if (is.null(legend_right)) {
    legend_right <- right_var  # Use the right variable name if legend_right is NULL
  }
  
  # Prepare the data by creating an inverted version of the left-side variable
  data <- data %>%
    mutate(Left = -data[[left_var]])  # Invert left-side values for plotting
  
  # Calculate the maximum values for left and right axes for balanced scaling
  max_left <- max(abs(data$Left))
  max_right <- max(data[[right_var]])
  y_breaks <- pretty(c(-max_left, max_right), n = 10)  # Define y-axis breaks for symmetry
  
  # Create the butterfly plot
  p <- ggplot(data) +
    # Plot left-side bars (inverted values)
    geom_bar(aes_string(x = sample_var, y = "Left", fill = "'Left'"),
             stat = "identity",
             width = 0.7) +
    # Plot right-side bars
    geom_bar(aes_string(x = sample_var, y = right_var, fill = "'Right'"),
             stat = "identity",
             width = 0.7) +
    # Add labels for left-side values
    geom_text(aes_string(x = sample_var, y = "Left", label = left_var),
              vjust = left_text_vjust,
              hjust = left_text_hjust,
              color = "black",
              size = 3.5) +
    # Add labels for right-side values
    geom_text(aes_string(x = sample_var, y = right_var, label = right_var),
              vjust = right_text_vjust,
              hjust = right_text_hjust,
              color = "black",
              size = 3.5) +
    coord_flip() +  # Flip coordinates for horizontal bars
    scale_y_continuous(name = "", breaks = y_breaks, labels = abs(y_breaks)) +  # Symmetric y-axis labels
    # Define custom colors and labels for the legend
    scale_fill_manual(values = c("Left" = left_color, "Right" = right_color),
                      name = NULL,
                      labels = c(legend_left, legend_right)) +
    # Add plot title and theme settings
    labs(title = title, x = "", y = "") +
    theme_classic() +
    theme(
      text = element_text(size = text_size, family = "Arial"),
      plot.title = element_text(hjust = 0.5, size = text_size + 2),
      axis.title = element_blank(),
      axis.text.x = element_text(size = text_size - 2),
      axis.text.y = element_text(size = text_size - 2),
      panel.grid = element_blank(),
      legend.position = "bottom"  # Place the legend at the bottom
    )
  
  # Save the plot as a PNG file
  ggsave(output_file, plot = p, width = figure_size[1], height = figure_size[2], dpi = 300)
}

#' @examples
# if (interactive()) {
#   # Simulate a dataset with sample names and protein/peptide counts
#   data <- data.frame(
#     Sample = c("Control_1", "Control_2", "Control_3", "Treat_1", "Treat_2", "Treat_3"),
#     Proteins = c(4500, 4300, 4600, 4700, 4800, 4900),  # Protein counts in thousands
#     Peptides = c(30000, 32000, 31000, 33000, 34000, 35000)  # Peptide counts in tens of thousands
#   )
# 
#   # Generate a butterfly plot with automatic legend names from variables
#   plot_butterfly_plot(
#     data = data,
#     sample_var = "Sample",
#     left_var = "Proteins",
#     right_var = "Peptides",
#     output_file = "butterfly_plot.png",
#     title = "Protein and Peptide Numbers"
#   )
# }