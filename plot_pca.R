#' Plot PCA
#'
#' This function performs Principal Component Analysis (PCA) on the provided protein abundance data
#' and generates a scatter plot of the first two principal components. Points can be colored by a
#' specified grouping variable, and optional labels can be added to the points. The output is styled for
#' publication in Nature journals.
#'
#' @param data Data frame containing the numeric protein abundance data for PCA.
#'             It must be in a wide format where rows represent samples and columns represent proteins.
#'             Each cell should contain numeric values representing protein abundance.
#'             NA values are compatible but should be handled prior to PCA.
#' @param group_var A factor or character vector representing the grouping for coloring the points
#'                  (e.g., sample groups). Its length must match the number of rows in `data`.
#'                  NA values are compatible but may affect the plot.
#' @param title Optional plot title (default is NULL, auto-generated).
#' @param x_axis_name Name for the x-axis (default is "PC1").
#' @param y_axis_name Name for the y-axis (default is "PC2").
#' @param show_labels Logical, whether to display labels for the points (default is FALSE).
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
                     output_file) {
  # Load necessary libraries
  library(ggplot2)
  
  # Perform PCA on transposed data
  pca_result <- prcomp(t(data), center = TRUE, scale. = TRUE)
  
  # Create a data frame for plotting
  pca_data <- as.data.frame(pca_result$x)
  pca_data$Group <- group_var
  pca_data$Sample <- rownames(pca_data)
  
  # Auto-generate plot title if not provided
  if (is.null(title)) {
    title <- "PCA Plot of Protein Abundances"
  }
  
  # Create axis labels with PC values
  x_axis_label <- paste(x_axis_name,
                        " (",
                        round(summary(pca_result)$importance[2, 1] * 100, 2),
                        "%)",
                        sep = "")
  y_axis_label <- paste(y_axis_name,
                        " (",
                        round(summary(pca_result)$importance[2, 2] * 100, 2),
                        "%)",
                        sep = "")
  
  # Create the PCA plot
  p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Group)) +
    geom_point(size = 3, alpha = 0.7) +  # Points
    labs(title = title, x = x_axis_label, y = y_axis_label) +  # Title and axis labels
    scale_color_manual(values = c(
      "Control" = "#0072B2",
      "Treatment" = "#D55E00"
    )) +  # Custom colors
    theme_classic() +
    theme(
      text = element_text(size = 14, family = "Arial"),
      plot.title = element_text(
        hjust = 0.5,
        size = 16,
        face = "bold"
      ),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      panel.grid = element_blank()
    )
  
  # Add labels if show_labels is TRUE
  if (show_labels) {
    p <- p + 
      geom_text(
        aes(label = Sample),
        vjust = -1.5,
        size = 2,
        check_overlap = TRUE,
        show.legend = FALSE
      )  # Adjust position for visibility
  }
  
  # Save the plot as a PNG file
  ggsave(
    output_file,
    plot = p,
    width = 7,
    height = 5,
    dpi = 300
  )
}

#' @examples
if (interactive()) {
  # Example: Creating a PCA plot with simulated protein abundance data.
  set.seed(123)  # For reproducibility
  num_proteins <- 5000
  num_samples <- 6
  
  # Simulated data: 3 control and 3 treatment samples
  control_data <- matrix(rnorm(
    num_proteins * num_samples * 0.5,
    mean = 100,
    sd = 20
  ), nrow = num_proteins)
  treatment_data <- matrix(rnorm(
    num_proteins * num_samples * 0.5,
    mean = 120,
    sd = 20
  ), nrow = num_proteins)
  colnames(control_data) <- paste0("Control_", 1:3)
  colnames(treatment_data) <- paste0("Treatment_", 1:3)
  
  # Convert to data frame and bind columns
  abundance_df <- as.data.frame(cbind(control_data, treatment_data))
  
  # Prepare group variable
  group_var <- factor(rep(c("Control", "Treatment"), each = 3))
  
  # Generating the PCA plot without labels.
  plot_pca(
    data = abundance_df,
    group_var = group_var,
    output_file = "pca_plot_nolabel_example.png",
    title = "PCA of Protein Abundance",
    show_labels = FALSE
  )
  
  # Generating the PCA plot with labels.
  plot_pca(
    data = abundance_df,
    group_var = group_var,
    output_file = "pca_plot_label_example.png",
    title = "PCA of Protein Abundance",
    show_labels = TRUE
  )
}
