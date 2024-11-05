write_excel <- function(df, file_path, sheet_name = "Sheet1", header_style_type = "dark_grey_bg") {
  
  # Step 1: Create a new workbook
  wb <- openxlsx::createWorkbook()
  
  # Step 2: Add a worksheet with the specified name
  openxlsx::addWorksheet(wb, sheet_name)
  
  # Step 3: Write the data to the worksheet
  openxlsx::writeData(wb, sheet_name, df)
  
  # Step 4: Automatically adjust column widths based on content
  column_widths <- sapply(df, function(col) {
    # Calculate the auto column width by determining the maximum length of the content in the column
    if (is.numeric(col)) {
      formatted_col <- format(col, scientific = FALSE, digits = 10)
      max_width <- max(nchar(formatted_col))
      max_width <- max_width * 1.3
    } else {
      max_width <- max(nchar(as.character(col)))
      max_width <- max_width * 1.3
      max_width <- min(max_width, 70)
    }
    
    max_width <- max(max_width, nchar(colnames(df)))
    return(max_width)
  })
  
  openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol(df), widths = column_widths)
  
  # Step 5: Apply selected header style
  if (header_style_type == "blue_background") {
    header_style <- openxlsx::createStyle(
      fontSize = 14,
      fontColour = "white",
      fgFill = "#4F81BD",   # Blue background
      halign = "center",
      valign = "center",
      textDecoration = "bold"
    )
  } else if (header_style_type == "green_bg") {
    header_style <- openxlsx::createStyle(
      fontSize = 14,
      fontColour = "white",
      fgFill = "#4CAF50",   # Green background
      halign = "center",
      valign = "center",
      textDecoration = "bold"
    )
  } else if (header_style_type == "dark_grey_bg") {
    header_style <- openxlsx::createStyle(
      fontSize = 14,
      fontColour = "white",
      fgFill = "#636363",   # Dark grey background
      halign = "center",
      valign = "center",
      textDecoration = "bold"
    )
  } else {
    stop("Unknown header style type. Choose one of: 'blue_bg', 'green_bg', 'dark_grey_bg'.")
  }
  
  # Apply header style
  openxlsx::addStyle(wb, sheet_name, style = header_style, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
  
  # Step 6: Increase the height of the header row and data rows
  openxlsx::setRowHeights(wb, sheet_name, rows = 1, heights = 20)  # Header row height
  openxlsx::setRowHeights(wb, sheet_name, rows = 2:nrow(df) + 1, heights = 18)  # Data row height
  
  # Step 7: Apply styling for data rows (borders around each cell)
  data_style <- openxlsx::createStyle(
    fontSize = 11,
    border = "TopBottomLeftRight",
    borderColour = "grey"
  )
  openxlsx::addStyle(wb, sheet_name, style = data_style, rows = 2:(nrow(df) + 1), cols = 1:ncol(df), gridExpand = TRUE)
  
  # Step 8: Freeze the top row and add filters
  openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
  openxlsx::addFilter(wb, sheet_name, rows = 1, cols = 1:ncol(df))
  
  # Step 9: Save the workbook to the specified file path
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
  
  # Inform the user that the data has been written successfully
  message("Data has been written to Excel in a publication-ready format.")
}
