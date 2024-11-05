#' Plot Scatter Plot with Linear Regression in Nature Journal Style
#'
#' This function creates a scatter plot with a linear regression line, equation, and \( R^2 \) value,
#' styled for publication in Nature journal format. Users can save the plot or return it for further modifications.
#'
#' @param data Data frame containing the data.
#' @param x_var Column name (as string) for the x-axis variable.
#' @param y_var Column name (as string) for the y-axis variable.
#' @param output_file Optional; name of the file to save the plot (e.g., "scatter_plot.png"). If NULL, returns plot object.
#' @param x_label Label for the x-axis (default is NULL, uses x_var).
#' @param y_label Label for the y-axis (default is NULL, uses y_var).
#' @param title Title of the plot (default is NULL).
#'
#' @return A ggplot object if `output_file` is NULL, otherwise saves the plot as a PNG file.
plot_scatter_with_regression <- function(data,
                                         x_var,
                                         y_var,
                                         output_file = NULL,
                                         x_label = NULL,
                                         y_label = NULL,
                                         title = NULL) {
  # Load necessary libraries
  library(ggplot2)
  library(dplyr)
  
  # Check if x_var and y_var exist in data
  if (!all(c(x_var, y_var) %in% names(data))) {
    stop("The specified x_var or y_var does not exist in the data.")
  }
  
  # Calculate linear regression model
  lm_model <- lm(data[[y_var]] ~ data[[x_var]])
  intercept <- round(coef(lm_model)[1], 2)
  slope <- round(coef(lm_model)[2], 2)
  r_squared <- round(summary(lm_model)$r.squared, 2)
  
  # Create annotation strings for equation and R-squared
  equation <- paste0("y = ", slope, "x + ", intercept)
  r2_label <- paste0("R² = ", r_squared)
  
  # Default axis labels and title if not provided
  if (is.null(x_label)) x_label <- x_var
  if (is.null(y_label)) y_label <- y_var
  if (is.null(title)) title <- paste("Scatter plot of", y_var, "vs", x_var)
  
  # Nature journal style settings
  nature_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  text_size <- 14
  figure_size <- c(6, 4.5)
  
  # Generate the scatter plot
  p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(color = nature_palette[2], size = 3) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = nature_palette[1]) +
    annotate("text", x = -Inf, y = Inf, label = equation,
             hjust = -0.1, vjust = 2, color = "black", size = 4) +
    annotate("text", x = -Inf, y = Inf, label = r2_label,
             hjust = -0.1, vjust = 3.5, color = "black", size = 4) +
    labs(title = title, x = x_label, y = y_label) +
    theme_classic() +
    theme(
      text = element_text(size = text_size, family = "sans"),
      plot.title = element_text(hjust = 0.5, size = text_size + 2),
      axis.title = element_text(size = text_size),
      axis.text = element_text(size = text_size - 2),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Save or return the plot
  if (is.null(output_file)) {
    return(p)
  } else {
    ggsave(output_file, plot = p, width = figure_size[1], height = figure_size[2], dpi = 300)
  }
}
#' @examples
if (interactive()) {
  # Generate a sample dataset with a linear correlation
  set.seed(123)
  data <- data.frame(
    x = rnorm(50, mean = 5)
  ) %>% 
    dplyr::mutate(y = 2 * x + rnorm(50, sd = 0.5))
  # Example usage of the plot function
  plot_scatter_with_regression(
    data = data,
    x_var = "x",
    y_var = "y",
    output_file = NULL,  # Set to a filename to save, e.g., "scatter_plot.png"
    x_label = "X Values",
    y_label = "Y Values",
    title = "Sample Scatter Plot with Regression"
  )
}