# --------------------------------------------------
# 3-Sets Proportional Venn Diagram (Euler) Function (Nature Style)
# 
# This script defines a function `plot_venn_3sets_proportional()` that generates 
# a 3-sets proportional Venn diagram using the 'eulerr' package. The diagram 
# follows the visual style of Nature journal figures, with customization options 
# for colors, transparency, text size, and other details.
#
# Instructions:
# - To use this script, source it in your R session.
# - The function parameters allow easy customization of set names, colors, and other plot details.
# - An example code block is provided using `if(interactive())` so it will only run in interactive sessions.
# --------------------------------------------------

# Load required libraries
library(eulerr)
library(ggplot2)

# --------------------------------------------------
# Function: plot_venn_3sets_proportional
# Description: Plots a 3-sets proportional Venn diagram (Euler diagram) 
#              with Nature-style aesthetics using the 'eulerr' package.
# Parameters:
# - set1, set2, set3: Vectors containing elements of each set.
# - set1_name, set2_name, set3_name: Names for each set.
# - output_file: File name for saving the Venn diagram as PNG.
# - fill_colors: Colors for each set.
# - alpha: Transparency level for the set fill colors.
# - text_size: Size of text for set counts.
# - line_width: Line width of the circles.
# - output_size: Dimensions of the output file (inches).
# - resolution: Resolution (DPI) for the output file.
# --------------------------------------------------
plot_venn_3sets_proportional <- function(set1,
                                         set2,
                                         set3,
                                         set1_name = "Set 1",
                                         set2_name = "Set 2",
                                         set3_name = "Set 3",
                                         output_file = "venn_3sets_proportional.png",
                                         fill_colors = c("#00AFBB", "#E7B800", "#FC4E07"),
                                         alpha = 0.5,
                                         text_size = 10,
                                         line_width = 1,
                                         output_size = c(6, 6),
                                         resolution = 300) {
  
  # Create a named list of sets for eulerr package
  venn_data <- list(Set1 = set1, Set2 = set2, Set3 = set3)
  
  # Compute the Euler diagram using eulerr package
  fit <- eulerr::euler(venn_data)
  
  # Plot the Euler diagram with Nature-style aesthetics
  plot <- plot(
    fit,
    fills = fill_colors,
    edges = list(lwd = line_width),
    labels = list(fontsize = text_size),
    quantities = TRUE
  )
  
  # Save the plot as PNG file using ggplot2's ggsave function
  ggsave(
    output_file,
    plot = plot,
    width = output_size[1],
    height = output_size[2],
    dpi = resolution
  )
}

# --------------------------------------------------
# Example usage (only runs in interactive sessions)
# --------------------------------------------------
# if (interactive()) {
#   set.seed(123)
#   set1 <- sample(1:100, 30)
#   set2 <- sample(30:130, 50)
#   set3 <- sample(60:160, 90)
#   
#   # Draw the proportional 3-sets Venn diagram with custom settings
#   plot_venn_3sets_proportional(
#     set1, set2, set3,
#     set1_name = "Group A",
#     set2_name = "Group B",
#     set3_name = "Group C",
#     output_file = "venn_3sets_proportional_example.png"
#   )
# }
