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
