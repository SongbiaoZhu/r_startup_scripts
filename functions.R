
# Function to install and load packages (CRAN and Bioconductor)
install_load <- function(packages) {
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  
  sapply(packages, function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
      if (pkg %in% rownames(available.packages())) {
        install.packages(pkg, dependencies = TRUE)
      } else {
        BiocManager::install(pkg, ask = FALSE)
      }
      library(pkg, character.only = TRUE)
    }
  })
}

# Example usage
# install_load(c("tidyverse", "magrittr", "data.table", "lubridate"))

# Function to export a pretty formatted Excel sheet
export_pretty_excel <- function(data, filename, sheetname = "Sheet1") {
  # Load required package
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    install.packages("openxlsx")
  }
  library(openxlsx)
  
  # Create a new workbook and add data
  wb <- createWorkbook()
  addWorksheet(wb, sheetname)
  writeData(wb, sheetname, data)
  
  # Set styles for better visualization
  headerStyle <- createStyle(fontSize = 12, fontColour = "#FFFFFF", halign = "center",
                             fgFill = "#4F81BD", border = "Bottom", textDecoration = "Bold")
  addStyle(wb, sheet = sheetname, style = headerStyle, rows = 1, cols = 1:ncol(data), gridExpand = TRUE)
  
  # Auto-adjust column widths
  setColWidths(wb, sheetname, cols = 1:ncol(data), widths = "auto")
  
  # Save workbook
  saveWorkbook(wb, filename, overwrite = TRUE)
  message("Exported Excel sheet to: ", filename)
}

# Example usage:
# export_pretty_excel(iris, "Pretty_Excel.xlsx")

# Function for Log2 transformation of specified columns
log2_transform <- function(data, columns) {
  data[columns] <- lapply(data[columns], log2)
  return(data)
}

# Example usage:
# transformed_data <- log2_transform(df, c("Intensity_A", "Intensity_B"))

# Function for Z-score normalization of specified columns
normalize_zscore <- function(data, columns) {
  data[columns] <- lapply(data[columns], function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
  return(data)
}

# Example usage:
# normalized_data <- normalize_zscore(df, c("Intensity_A", "Intensity_B"))
