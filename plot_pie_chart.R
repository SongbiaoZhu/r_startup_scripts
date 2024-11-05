#' Plot Pie Chart in Nature Journal Style
#'
#' This function plots a pie chart based on the given data and variable, producing a publication-ready figure
#' in Nature journal style. The plot is saved as a PNG file. The pie chart is automatically labeled with the
#' corresponding values or percentages.
#'
#' @param data Data frame containing the data.
#' @param variable Column name (as string) representing the categorical variable to be used for the pie chart.
#' @param value Column name (as string) representing the values or counts for each category.
#' @param title Plot title.
#' @param output_file Name of the file where the plot will be saved (e.g., "pie_chart.png").
#'
#' @return A publication-ready pie chart saved as a PNG file.

plot_pie_chart <- function(data,
                           variable,
                           value,
                           title,
                           output_file) {
  # Load necessary libraries
  library(ggplot2)
  
  # Nature journal style settings
  text_size <- 14  # Set default text size
  figure_size <- c(6, 6)  # Set default figure size for a square pie chart in inches
  
  # Prepare data for pie chart
  data <- data %>%
    dplyr::mutate(percentage = !!sym(value) / sum(!!sym(value)) * 100)  # Calculate percentages
  
  # Generate the pie chart plot
  p <- ggplot(data, aes(x = "", y = !!sym(value), fill = !!sym(variable))) +
    geom_bar(stat = "identity", width = 1, color = "black") +  # Bar plot to simulate pie chart
    coord_polar("y") +  # Convert bar plot to pie chart using polar coordinates
    labs(title = title, x = NULL, y = NULL) +
    theme_classic() +  # Classic theme (no background)
    theme(
      text = element_text(size = text_size, family = "Arial"),
      plot.title = element_text(hjust = 0.5, size = text_size + 2),
      axis.title = element_blank(),  # Remove axis titles
      axis.text = element_blank(),  # Remove axis text
      axis.ticks = element_blank(),  # Remove axis ticks
      panel.grid = element_blank(),  # Remove grid lines
      axis.line = element_blank(),  # Remove axis lines
      legend.title = element_blank(),  # Remove legend title
      legend.position = "right"
    ) +
    geom_text(aes(label = paste0(round(percentage, 1), "%")),
              position = position_stack(vjust = 0.5),
              size = text_size / 4)  # Add percentage labels
  
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
#   # Simulate example data
#   data <- data.frame(
#     category = c("A", "B", "C", "D"),
#     count = c(30, 20, 40, 10)
#   )
#   
#   # Example usage of the plot_pie_chart function
#   plot_pie_chart(data,
#                  "category",
#                  "count",
#                  "Sample Pie Chart",
#                  "pie_chart_example.png")
# }
