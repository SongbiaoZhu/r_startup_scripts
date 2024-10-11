# Function for Z-score normalization of specified columns
normalize_zscore <- function(data, columns) {
  data[columns] <- lapply(data[columns], function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
  return(data)
}

# Example usage:
# normalized_data <- normalize_zscore(df, c("Intensity_A", "Intensity_B"))
