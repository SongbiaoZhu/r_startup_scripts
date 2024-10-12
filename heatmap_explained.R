# Load necessary libraries for heatmap plotting
library(pheatmap)
library(RColorBrewer)

# Simulate example data for heatmap
set.seed(123)  # Ensure reproducibility of random numbers

# Generate random protein expression values for 50 proteins across 6 samples
data <- data.frame(
  Accession = paste0("Protein", 1:50),  # Protein identifiers
  Sample1 = rnorm(50, mean = 20, sd = 5),  # Expression values for Sample1
  Sample2 = rnorm(50, mean = 22, sd = 5),  # Expression values for Sample2
  Sample3 = rnorm(50, mean = 18, sd = 5),  # Expression values for Sample3
  Sample4 = rnorm(50, mean = 25, sd = 5),  # Expression values for Sample4
  Sample5 = rnorm(50, mean = 19, sd = 5),  # Expression values for Sample5
  Sample6 = rnorm(50, mean = 23, sd = 5)   # Expression values for Sample6
)

# Group information for each sample (e.g., experimental conditions)
sample_info <- data.frame(
  Sample = c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5", "Sample6"),
  Group = c("GroupA", "GroupA", "GroupA", "GroupB", "GroupB", "GroupB")
)
# Explanation:
# - 'Sample1', 'Sample2', 'Sample3' belong to 'GroupA'
# - 'Sample4', 'Sample5', 'Sample6' belong to 'GroupB'

# Prepare the numeric data for the heatmap
numeric_data <- as.data.frame(data[, -1])  # Exclude the 'Accession' column (protein IDs)
rownames(numeric_data) <- data$Accession  # Use 'Accession' as row names for proteins

# Get sample names from the dataset
sample_names <- colnames(numeric_data)  # Extract column names as sample identifiers

# Match the group information to the sample names
group_colors <- sample_info$Group[match(sample_names, sample_info$Sample)]

# Explanation:
# - We match the sample names from the dataset to their corresponding group information.
# - This helps color samples by their experimental group in the heatmap.

# Generate a color palette for the groups
n_groups <- length(unique(sample_info$Group))  # Number of unique groups

# Assign colors to groups using RColorBrewer for >2 groups, or custom colors for 2 groups
if (n_groups > 2) {
  color_palette <- RColorBrewer::brewer.pal(n_groups, "Set1")  # For multiple groups
} else {
  color_palette <- c("#4E79A7", "#F28E2B")  # Custom Nature-style colors (blue, orange)
}

# Map group names to their colors
annotation_colors <- list(Group = setNames(color_palette, unique(group_colors)))

# Create the annotation data frame for sample grouping
annotation_col <- data.frame(Group = group_colors)
rownames(annotation_col) <- sample_names  # Set sample names as row names in the annotation

# Explanation:
# - 'annotation_col' assigns group information (GroupA, GroupB) to each sample.
# - 'annotation_colors' links group names to their respective colors for use in the heatmap.

# Plot the heatmap with sample group coloring
pheatmap(
  numeric_data,  # Data matrix for heatmap (protein expressions)
  color = colorRampPalette(c("navy", "white", "firebrick3"))(50),  # Gradient colors for heatmap
  cluster_rows = TRUE,  # Cluster rows (proteins)
  cluster_cols = TRUE,  # Cluster columns (samples)
  show_rownames = FALSE,  # Hide protein names for clarity
  annotation_col = annotation_col,  # Add group color annotations for samples
  annotation_colors = annotation_colors,  # Apply group color mapping
  main = "Heatmap of 50 Proteins with Group Coloring",  # Title for the heatmap
  angle_col = 45,  # Rotate sample names for better readability
  filename = "heatmap_example.png"  # Save plot as a PNG file
)

# Explanation:
# - This heatmap clusters both proteins and samples based on their expression values.
# - Each sample is color-coded based on its group (GroupA or GroupB).
# - The heatmap uses a color gradient (navy to white to firebrick3) to represent expression levels.
