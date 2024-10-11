# Function for Log2 transformation of specified columns
log2_transform <- function(data, columns) {
  data[columns] <- lapply(data[columns], log2)
  return(data)
}

# Example usage:
# transformed_data <- log2_transform(df, c("Intensity_A", "Intensity_B"))
