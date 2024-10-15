#' Plot Histogram in Nature Journal Style
#'
#' This function plots a histogram based on the given data and variable, producing a publication-ready figure
#' in Nature journal style. The plot is saved as a PNG file. The `bin_width` parameter is optional and can be
#' left as NULL for automatic determination.
#'
#' @param data Data frame containing the data.
#' @param variable Column name (as string) representing the variable to be plotted on the x-axis.
#' @param x_label Label for the x-axis.
#' @param y_label Label for the y-axis.
#' @param title Plot title.
#' @param output_file Name of the file where the plot will be saved (e.g., "histogram.png").
#' @param bin_width Width of the bins in the histogram (default is NULL, and will be determined automatically).
#'
#' @return A publication-ready histogram saved as a PNG file.

plot_histogram <- function(data,
                           variable,
                           x_label,
                           y_label,
                           title,
                           output_file,
                           bin_width = NULL) {
  # Load necessary libraries
  library(ggplot2)
  
  # Nature journal style settings
  text_size <- 14  # Set default text size
  figure_size <- c(6, 4.5)  # Set default figure size in inches
  
  # Generate the histogram plot
  p <- ggplot(data, aes_string(x = variable)) +
    geom_histogram(binwidth = bin_width,
                   color = "black",
                   fill = "#4E79A7") +
    labs(title = title, x = x_label, y = y_label) +
    theme_classic() +  # Switch to classic theme (no background)
    theme(
      text = element_text(size = text_size, family = "Arial"),
      plot.title = element_text(hjust = 0.5, size = text_size + 2),
      axis.title = element_text(size = text_size),
      axis.text = element_text(size = text_size - 2),
      panel.grid = element_blank(),  # Remove grid lines
      axis.line = element_line(color = "black")  # Keep axis lines visible
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
if (interactive()) {
  # Simulate example data
  set.seed(123)
  data <- data.frame(value = rnorm(1000, mean = 50, sd = 10))
  
  # Example usage of the plot_histogram function
  plot_histogram(data,
                 "value",
                 "Value",
                 "Frequency",
                 "Histogram",
                 "histogram_example.png")
}
