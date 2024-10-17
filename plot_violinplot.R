#' Plot Violin Plot in Nature Journal Style
#'
#' This function plots a violin plot from a dataset, producing a publication-ready figure
#' in Nature journal style. The plot is saved as a PNG file and adds median values to the plot.
#'
#' @param data Data frame containing the data.
#' @param x_var Column name (as string) representing the x-axis variable.
#' @param y_var Column name (as string) representing the y-axis variable (Abundance).
#' @param group_var Column name (as string) representing the group aesthetic (optional, default is NULL).
#' @param output_file Name of the file where the plot will be saved (e.g., "violinplot.png").
#' @param x_label Label for the x-axis (default is NULL, uses x_var).
#' @param y_label Label for the y-axis (default is "Abundance").
#' @param title Plot title (default is NULL, uses x_var and y_var).
#'
#' @return A publication-ready violin plot saved as a PNG file.
plot_violinplot <- function(data,
                            x_var,
                            y_var,
                            group_var = NULL,
                            output_file,
                            x_label = NULL,
                            y_label = "Abundance",
                            title = NULL) {
  # Load necessary libraries
  library(ggplot2)
  library(dplyr)
  
  # Nature journal color palette (base colors)
  base_nature_palette <- c("#E69F00",
                           "#56B4E9",
                           "#009E73",
                           "#F0E442",
                           "#0072B2",
                           "#D55E00",
                           "#CC79A7")
  
  # Dynamically extend the palette if needed
  nature_palette <- if (length(unique(data[[x_var]])) > length(base_nature_palette)) {
    colorRampPalette(base_nature_palette)(length(unique(data[[x_var]])))
  } else {
    base_nature_palette
  }
  
  # Nature journal style settings
  text_size <- 14  # Set default text size
  figure_size <- c(6, 4.5)  # Set default figure size in inches
  
  # Auto-generate labels if not provided
  if (is.null(x_label)) {
    x_label <- x_var
  }
  if (is.null(title)) {
    title <- paste("Violin Plot of", y_var, "by", x_var)
  }
  
  # Convert the fill variable to a factor if group_var is provided or use x_var as fill when no group_var
  fill_var <- if (is.null(group_var))
    as.factor(data[[x_var]])
  else
    as.factor(data[[group_var]])
  
  # Calculate medians for each group in x_var (and group_var if provided)
  if (!is.null(group_var)) {
    medians <- data %>%
      group_by(!!sym(x_var), !!sym(group_var)) %>%
      summarize(median_val = median(!!sym(y_var)), .groups = 'drop') %>%
      distinct()
  } else {
    medians <- data %>%
      group_by(!!sym(x_var)) %>%
      summarize(median_val = median(!!sym(y_var)), .groups = 'drop') %>%
      distinct()
  }
  
  # Generate the violin plot
  p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_violin(aes(fill = fill_var), trim = FALSE) +  # Adjust fill aesthetic
    stat_summary(
      fun = "median",
      geom = "point",
      shape = 21,
      size = 3,
      color = "black",
      fill = "white"
    ) +  # Add median points
    scale_fill_manual(values = nature_palette) +  # Use Nature journal color palette
    geom_text(
      data = medians,
      aes_string(x = x_var, y = "median_val", label = "round(median_val, 2)"),
      position = position_dodge(width = 0.75),
      vjust = ifelse(medians$median_val > median(data[[y_var]]), -0.5, 1.5),
      # Adjust label positioning
      size = 4
    ) +  # Adjust size of the label
    labs(title = title, x = x_label, y = y_label) +
    theme_classic() +
    theme(
      text = element_text(size = text_size, family = "Arial"),
      plot.title = element_text(hjust = 0.5, size = text_size + 2),
      axis.title = element_text(size = text_size),
      axis.text.x = element_text(
        angle = 30,
        hjust = 1,
        size = text_size - 2
      ),
      # Rotate x-axis labels
      axis.text.y = element_text(size = text_size - 2),
      panel.grid = element_blank()
    )
  
  # Conditionally hide or show the legend
  if (is.null(group_var)) {
    p <- p + guides(fill = "none")  # Hide the legend if group_var is NULL (no grouping)
  } else {
    p <- p + labs(fill = group_var)  # Set the legend title as the group_var
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
  # Simulate a dataset with 6 samples (Control 1-3, Treat 1-3)
  set.seed(123)
  data <- data.frame(
    Sample = rep(
      c(
        "Control_1",
        "Control_2",
        "Control_3",
        "Treat_1",
        "Treat_2",
        "Treat_3"
      ),
      each = 10
    ),
    Abundance = c(rnorm(30, mean = 10, sd = 1), rnorm(30, mean = 12, sd = 1)),
    Group = rep(c("Control", "Treat"), each = 30)
  )
  
  # Case 1: Plot by Sample, no group_var provided
  plot_violinplot(
    data,
    x_var = "Sample",
    y_var = "Abundance",
    group_var = NULL,
    # No grouping, color by Sample
    output_file = "violinplot_by_sample.png",
    x_label = "Sample",
    y_label = "Log2 Abundance",
    title = "Violin Plot of Abundance by Sample"
  )
  
  # Case 2: Plot by Group, show legend
  plot_violinplot(
    data,
    x_var = "Sample",
    y_var = "Abundance",
    group_var = "Group",
    # Grouping by Group, show legend
    output_file = "violinplot_by_group.png",
    x_label = "Sample",
    y_label = "Log2 Abundance",
    title = "Violin Plot of Abundance by Group"
  )
}
