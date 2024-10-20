#' Plot PCA
#'
#' This function performs Principal Component Analysis (PCA) on the provided protein abundance data
#' and generates a scatter plot of the first two principal components. Points can be colored by a
#' specified grouping variable, and optional labels can be added to the points. The output is styled for
#' publication in Nature journals.
#'
#' @param data Data frame containing the numeric protein abundance data for PCA.
#' @param group_var A character vector representing the grouping for coloring the points (e.g., sample groups).
#' @param title Optional plot title (default is NULL, auto-generated).
#' @param x_axis_name Name for the x-axis (default is "PC1").
#' @param y_axis_name Name for the y-axis (default is "PC2").
#' @param show_labels Logical, whether to display labels for the points (default is FALSE).
#' @param label_var Optional column name (as a string) representing the variable used for point labels (default is NULL).
#' @param output_file File name where the plot will be saved (e.g., "pca_plot.png").
#'
#' @return A publication-ready PCA plot saved as a PNG file.
#' @export
#'
plot_pca <- function(data,
                     group_var,
                     title = NULL,
                     x_axis_name = "PC1",
                     y_axis_name = "PC2",
                     show_labels = FALSE,
                     label_var = NULL,
                     output_file) {
  # Load necessary libraries
  library(ggplot2)
  
  # Perform PCA
  pca_result <- prcomp(data, center = TRUE, scale. = TRUE)
  
  # Create a data frame for plotting
  pca_data <- as.data.frame(pca_result$x)
  pca_data$Group <- group_var
  
  # Auto-generate plot title if not provided
  if (is.null(title)) {
    title <- "PCA Plot of Protein Abundances"
  }
  
  # Create axis labels with PC values
  x_axis_label <- paste(x_axis_name, " (", round(summary(pca_result)$importance[2, 1] * 100, 2), "%)", sep = "")
  y_axis_label <- paste(y_axis_name, " (", round(summary(pca_result)$importance[2, 2] * 100, 2), "%)", sep = "")
  
  # Create the PCA plot
  p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Group)) +
    geom_point(size = 3, alpha = 0.7) +  # Points
    labs(title = title, x = x_axis_label, y = y_axis_label) +  # Title and axis labels
    scale_color_manual(values = c("Control" = "#0072B2", "Treatment" = "#D55E00")) +  # Custom colors
    theme_classic() +
    theme(
      text = element_text(size = 14, family = "Arial"),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      panel.grid = element_blank()
    )
  
  # Add labels if show_labels is TRUE
  if (show_labels && !is.null(label_var)) {
    p <- p + geom_text(aes_string(label = label_var), vjust = -0.5, size = 3, check_overlap = TRUE)
  }
  
  # Save the plot as a PNG file
  ggsave(output_file, plot = p, width = 7, height = 5, dpi = 300)
}

#' @examples
if (interactive()) {
  # Example: Creating a PCA plot with simulated protein abundance data.
  # This dataset contains 5000 proteins (rows) for 6 samples (columns).
  set.seed(123)  # For reproducibility
  num_proteins <- 5000
  num_samples <- 6
  
  # Simulated data: 3 control and 3 treatment samples
  control_data <- matrix(rnorm(num_proteins * num_samples * 0.5, mean = 100, sd = 20), nrow = num_proteins)
  treatment_data <- matrix(rnorm(num_proteins * num_samples * 0.5, mean = 120, sd = 20), nrow = num_proteins)
  colnames(control_data) <- paste0("Control_", 1:3)
  colnames(treatment_data) <- paste0("Treatment_", 1:3)
  
  # Convert to data frame and transpose
  abundance_df <- as.data.frame(t(cbind(control_data, treatment_data)))
  colnames(abundance_df) <- paste0("Protein_", 1:num_proteins)

  # Prepare group variable (length must match the number of samples)
  group_var <- factor(purrr::map_chr(rownames(abundance_df), ~ stringr::str_split(.x, "_")[[1]][1]))
  
  # Generating the PCA plot.
  plot_pca(
    data = abundance_df,  # Use protein abundance data
    group_var = group_var,  # Group information
    output_file = "pca_plot_example.png",
    title = "PCA of Protein Abundance",
    show_labels = FALSE  # Optionally show labels
  )
}
