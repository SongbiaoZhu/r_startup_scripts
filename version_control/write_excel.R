#' Write Data Frame to Publication-Ready Excel File
#'
#' This function writes a data frame to an Excel file with customizable header styles,
#' automatic column width adjustments, and data formatting, ensuring a tidy, publication-ready output.
#'
#' @param df Data frame to be written to the Excel file.
#' @param file_path Full path (including file name) where the Excel file will be saved.
#' @param sheet_name Name of the sheet to be added in the Excel file (default: "Sheet1").
#' @param header_style_type Style for the header row. Options: "blue_bg", "green_bg", "dark_grey_bg" (default: "dark_grey_bg").
#'
#' @return An Excel file saved at the specified location.

write_excel <- function(df, file_path, sheet_name = "Sheet1", header_style_type = "dark_grey_bg") {
  
  # Step 1: Create a new workbook
  wb <- openxlsx::createWorkbook()
  
  # Step 2: Add a worksheet with the specified name
  openxlsx::addWorksheet(wb, sheet_name)
  
  # Step 3: Write the data to the worksheet
  openxlsx::writeData(wb, sheet_name, df)
  
  # Step 4: Automatically adjust column widths based on content
  column_widths <- sapply(df, function(col) {
    # Calculate the maximum width based on the longest content in the column
    if (is.numeric(col)) {
      formatted_col <- format(col, scientific = FALSE, digits = 10)
      max_width <- max(nchar(formatted_col))
      max_width <- max_width * 1.3
    } else {
      max_width <- max(nchar(as.character(col)))
      max_width <- max_width * 1.3
      max_width <- min(max_width, 70)  # Cap the width for readability
    }
    max(max_width, nchar(colnames(df)))  # Ensure column names fit
  })
  
  openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol(df), widths = column_widths)
  
  # Step 5: Apply the selected header style
  if (header_style_type == "blue_bg") {
    header_style <- openxlsx::createStyle(
      fontSize = 14,
      fontColour = "white",
      fgFill = "#4F81BD",
      halign = "center",
      valign = "center",
      textDecoration = "bold"
    )
  } else if (header_style_type == "green_bg") {
    header_style <- openxlsx::createStyle(
      fontSize = 14,
      fontColour = "white",
      fgFill = "#4CAF50",
      halign = "center",
      valign = "center",
      textDecoration = "bold"
    )
  } else if (header_style_type == "dark_grey_bg") {
    header_style <- openxlsx::createStyle(
      fontSize = 14,
      fontColour = "white",
      fgFill = "#636363",
      halign = "center",
      valign = "center",
      textDecoration = "bold"
    )
  } else {
    stop("Unknown header style type. Choose one of: 'blue_background', 'green_bg', 'dark_grey_bg'.")
  }
  
  # Apply header style
  openxlsx::addStyle(wb, sheet_name, style = header_style, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
  
  # Step 6: Adjust row heights for better visibility
  openxlsx::setRowHeights(wb, sheet_name, rows = 1, heights = 20)  # Header row height
  openxlsx::setRowHeights(wb, sheet_name, rows = 2:(nrow(df) + 1), heights = 18)  # Data rows
  
  # Step 7: Apply border style for data cells
  data_style <- openxlsx::createStyle(
    fontSize = 11,
    border = "TopBottomLeftRight",
    borderColour = "grey"
  )
  openxlsx::addStyle(wb, sheet_name, style = data_style, rows = 2:(nrow(df) + 1), cols = 1:ncol(df), gridExpand = TRUE)
  
  # Step 8: Freeze top row and add filters
  openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
  openxlsx::addFilter(wb, sheet_name, rows = 1, cols = 1:ncol(df))
  
  # Step 9: Save the workbook to the specified file path
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
  
  # Notify the user
  message("Data has been written to Excel in a publication-ready format.")
}
#' @examples
# if(interactive()){
#   # Save a sample data frame with the default style
#   write_excel(mtcars, "output.xlsx")
# 
#   # Save with a custom sheet name and green header style
#   write_excel(mtcars, "output_custom.xlsx", sheet_name = "Cars", header_style_type = "green_bg")
# 
# }
