# Load necessary libraries
library(pheatmap)
library(RColorBrewer)

# Simulate example data
set.seed(123)  # Ensure reproducibility

# Generate 50 proteins and their expression values for 6 samples
data <- data.frame(
  Accession = paste0("Protein", 1:50),
  Sample1 = rnorm(50, mean = 20, sd = 5),
  Sample2 = rnorm(50, mean = 22, sd = 5),
  Sample3 = rnorm(50, mean = 18, sd = 5),
  Sample4 = rnorm(50, mean = 25, sd = 5),
  Sample5 = rnorm(50, mean = 19, sd = 5),
  Sample6 = rnorm(50, mean = 23, sd = 5)
)

# Group information for each sample
sample_info <- data.frame(
  Sample = c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5", "Sample6"),
  Group = c("GroupA", "GroupA", "GroupA", "GroupB", "GroupB", "GroupB")
)

# Explanation:
# - 'Sample1', 'Sample2', 'Sample3' are from 'GroupA'
# - 'Sample4', 'Sample5', 'Sample6' are from 'GroupB'

# Prepare the numeric data for the heatmap
numeric_data <- as.data.frame(data[, -1])  # Exclude the 'Accession' column
rownames(numeric_data) <- data$Accession  # Use 'Accession' as row names

# Explanation:
# - 'numeric_data' is the matrix of protein expression values for each sample
# - Rows are proteins and columns are samples

# Set the sample names and map them to their corresponding group information
sample_names <- colnames(numeric_data)  # Sample names from the data
group_colors <- sample_info$Group[match(sample_names, sample_info$Sample)]  # Match groups

# Generate a color palette for the groups
n_groups <- length(unique(sample_info$Group))  # Get the number of unique groups

# Explanation:
# - We use RColorBrewer to assign colors to groups
# - If there are more than 2 groups, we use the Set1 palette
# - If there are only 2 groups, we use a custom Nature journal-style palette

if (n_groups > 2) {
  color_palette <- RColorBrewer::brewer.pal(n_groups, "Set1")
} else {
  color_palette <- c("#4E79A7", "#F28E2B")  # Dark blue and orange tones
}

# Map group information to the color palette
annotation_colors <- list(Group = setNames(color_palette, unique(group_colors)))

# Create the annotation data frame for sample grouping
annotation_col <- data.frame(Group = group_colors)
rownames(annotation_col) <- sample_names  # Set the sample names as row names for the annotation

# Explanation:
# - 'annotation_col' assigns group information to each sample for coloring
# - 'annotation_colors' maps group names to their respective colors

# Plot the heatmap with sample group coloring
pheatmap(
  numeric_data,  # Data for heatmap
  color = colorRampPalette(c("navy", "white", "firebrick3"))(50),  # Color gradient for heatmap
  cluster_rows = TRUE,  # Perform unsupervised clustering on rows (proteins)
  cluster_cols = TRUE,  # Perform unsupervised clustering on columns (samples)
  show_rownames = FALSE,  # Do not display row names (protein names)
  annotation_col = annotation_col,  # Color samples by group
  annotation_colors = annotation_colors,  # Assign colors to groups
  main = "Heatmap of 50 Proteins with Group Coloring",  # Title of the plot
  angle_col = 45,  # Rotate the sample names
  filename = "heatmap_example.png"  # Save the heatmap as a PNG file
)

# Explanation:
# - The heatmap clusters both rows (proteins) and columns (samples)
# - Group information is shown in the heatmap with colors assigned to each sample based on its group
# - The color gradient from navy to white to firebrick3 represents the expression levels
