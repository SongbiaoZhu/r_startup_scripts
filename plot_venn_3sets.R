# --------------------------------------------------
# 3-Sets Venn Diagram Plotting Function (Proportional, Nature Style)
# 
# This script defines a function `plot_venn_3sets()` that generates a 3-sets 
# proportional Venn diagram using the 'VennDiagram' package. The diagram follows
# the visual style of Nature journal figures, with customization options for 
# colors, transparency, text size, and label positions.
#
# Instructions:
# - To use this script, source it in your R session.
# - The function parameters allow easy customization of set names, colors, and other plot details.
# - An example code block is provided using `if(interactive())` so it will only run in interactive sessions.
# --------------------------------------------------

# Load required libraries
library(VennDiagram)
library(grid)

# --------------------------------------------------
# Function: plot_venn_3sets
# Description: Plots a 3-sets Venn diagram with Nature-style aesthetics.
# Parameters:
# - set1, set2, set3: Vectors containing elements of each set.
# - set1_name, set2_name, set3_name: Names for each set.
# - output_file: File name for saving the Venn diagram as PNG.
# - fill_colors: Colors for each set.
# - alpha: Transparency level for the set fill colors.
# - text_size: Size of text for set counts.
# - category_text_size: Size of the set category names.
# - cat_pos: Vector specifying the position (degrees) of the category names.
# - cat_dist: Distance of category names from their respective circles.
# - line_width: Line width of the circles.
# - resolution: Resolution (DPI) for the output file.
# - output_size: Dimensions of the output file (inches).
# --------------------------------------------------
plot_venn_3sets <- function(set1,
                            set2,
                            set3,
                            set1_name = "Set 1",
                            set2_name = "Set 2",
                            set3_name = "Set 3",
                            output_file = "venn_3sets.png",
                            fill_colors = c("#00AFBB", "#E7B800", "#FC4E07"),
                            alpha = 0.5,
                            text_size = 1.5,
                            category_text_size = 1.5,
                            cat_pos = c(-30, 30, 180),
                            cat_dist = c(0.04, 0.04, 0.03),
                            line_width = 2,
                            resolution = 300,
                            output_size = c(8, 8)) {
  
  # Generate Venn diagram using 'venn.diagram' function from VennDiagram package
  venn <- venn.diagram(
    x = list(Set1 = set1, Set2 = set2, Set3 = set3),
    category.names = c(set1_name, set2_name, set3_name),
    filename = NULL,  # No direct output, we'll handle file saving
    disable.logging = TRUE,
    output = TRUE,
    lwd = line_width,
    fill = fill_colors,
    alpha = alpha,
    cex = text_size,
    cat.cex = category_text_size,
    cat.pos = cat_pos,
    cat.dist = cat_dist,
    fontfamily = "Arial",
    fontface = "bold",
    cat.fontfamily = "Arial",
    cat.fontface = "bold"
  )
  
  # Save the plot as PNG file
  png(
    output_file,
    width = output_size[1],
    height = output_size[2],
    units = "in",
    res = resolution
  )
  grid.draw(venn)  # Draw the Venn diagram
  dev.off()  # Close the PNG device
}

# --------------------------------------------------
# Example usage (only runs in interactive sessions)
# --------------------------------------------------
if (interactive()) {
  set.seed(123)
  set1 <- sample(1:100, 30)
  set2 <- sample(30:130, 50)
  set3 <- sample(60:160, 100)
  
  # Draw the Venn diagram with custom settings
  plot_venn_3sets(
    set1, set2, set3,
    set1_name = "Group A",
    set2_name = "Group B",
    set3_name = "Group C",
    output_file = "venn_3sets_example.png"
  )
}
