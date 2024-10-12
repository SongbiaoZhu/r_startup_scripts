# --------------------------------------------------
# Correlation Matrix Plotting Function (Nature Style)
#
# This script defines a function `plot_correlation_matrix()` that generates
# a scatterplot matrix with correlation values, density plots, and linear
# regression lines, following the visual style of Nature journal figures.
#
# Instructions:
# - To use this script, source it in your R session.
# - The function parameters allow easy customization of data, axis labels,
#   and plot aesthetics.
# - An example data simulation code block is provided using `if(interactive())`
#   so it will only run in interactive sessions.
# --------------------------------------------------

# List of required packages
library(psych)
library(ggplot2)
library(ggsci)
library(gridExtra)

# --------------------------------------------------
# Function: plot_correlation_matrix
# Description: Plots a correlation matrix with Nature-style aesthetics.
# Parameters:
# - data: Dataframe with numeric values for plotting.
# - output_file: File name for saving the correlation matrix plot as PNG.
# - plot_title: Title for the plot.
# - text_size: Size of text for axis labels and correlation values.
# --------------------------------------------------
plot_correlation_matrix <- function(data,
                                    output_file = "correlation_matrix.png",
                                    plot_title = "Correlation Matrix of Protein Abundance",
                                    text_size = 1.8) {
  # Save the plot in a publication-ready format
  png(output_file,
      width = 1200,
      height = 1200,
      res = 150)
  
  # Plot scatter correlation matrix in Nature journal style
  psych::pairs.panels(
    data,
    method = "pearson",
    hist.col = "gray",
    # Histogram color
    density = TRUE,
    # Show density plots
    ellipses = TRUE,
    # Show correlation ellipses
    lm = FALSE,
    # No linear regression lines
    rug = FALSE,
    # Disable rug plots
    stars = TRUE,
    # Disable significance stars
    hist.border = NA,
    # No lines between histogram values
    # cex.cor = text_size,
    # # Size of correlation coefficient text
    # cex.axis = text_size,
    # # Size of axis labels
    cex.main = 2,
    # Size of main title
    col = "black",
    # Scatter plot point color
    family = "Arial",
    # Font family
    main = plot_title            # Main title
  )
  
  dev.off()  # Close the PNG device
}

# --------------------------------------------------
# Example usage (only runs in interactive sessions)
# --------------------------------------------------
if (interactive()) {
  # Load necessary package for simulation
  library(MASS)
  
  # Simulate example data
  set.seed(123)
  n_proteins <- 5000
  n_samples <- 6
  mean_abundance <- rep(1E5, n_samples)
  sd_abundance <- rep(2E4, n_samples)
  cor_matrix <- matrix(0.6, n_samples, n_samples)
  diag(cor_matrix) <- 1
  protein_abundance_matrix <- mvrnorm(n = n_proteins,
                                      mu = mean_abundance,
                                      Sigma = cor_matrix * sd_abundance ^ 2)
  
  # Convert to dataframe and log2 transform the data
  protein_abundance <- as.data.frame(protein_abundance_matrix)
  colnames(protein_abundance) <- c("Control1",
                                   "Control2",
                                   "Control3",
                                   "Treat1",
                                   "Treat2",
                                   "Treat3")
  protein_abundance_log2 <- log2(protein_abundance)
  
  # Draw the correlation matrix with custom settings
  plot_correlation_matrix(protein_abundance_log2, output_file = "correlation_matrix_example.png")
}
