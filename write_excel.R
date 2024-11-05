#' Write Data to a Tidy, Publication-Ready Excel File
#'
#' This function writes a data frame to an Excel file, applying formatting and styling to 
#' make the output publication-ready. The function automatically adjusts column widths,
#' styles the header row, applies borders to data rows, freezes the header row, and adds
#' filters for easier navigation of the data.
#'
#' @param df A data frame containing the data to be written to Excel.
#' @param file_path A character string representing the path and file name where the Excel file will be saved.
#' @param sheet_name A character string representing the name of the sheet (default is "Sheet1").
#'
#' @return A publication-ready Excel file is saved at the specified location.
#'
#' @examples
#' if (interactive()) {
#'   # Example data frame
#'   df <- data.frame(
#'     Protein = c("Protein A", "Protein B", "Protein C"),
#'     Abundance = c(10.5, 23.8, 15.4),
#'     Gene = c("Gene A", "Gene B", "Gene C"),
#'     stringsAsFactors = FALSE
#'   )
#'   
#'   # Write the example data to an Excel file
#'   write_excel(df, "output_example.xlsx", sheet_name = "Proteins")
#' }
#'
write_excel <- function(df, file_path, sheet_name = "Sheet1") {
  
  # Step 1: Create a new workbook
  wb <- openxlsx::createWorkbook()
  
  # Step 2: Add a worksheet with the specified name
  openxlsx::addWorksheet(wb, sheet_name)
  
  # Step 3: Write the data to the worksheet
  openxlsx::writeData(wb, sheet_name, df)
  
  # Step 4: Automatically adjust column widths based on content
  openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol(df), widths = "auto")
  
  # Step 5: Apply styling to header row (Bold, centered, white text on blue background)
  header_style <- openxlsx::createStyle(
    fontSize = 14,         # Larger font size for header
    fontColour = "white",  # White text color for header
    fgFill = "#4F81BD",    # Background color (blue)
    halign = "center",     # Center align the header
    valign = "center",     # Vertically center the header text
    textDecoration = "bold" # Make header text bold
  )
  openxlsx::addStyle(wb, sheet_name, style = header_style, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
  
  # Step 6: Increase the height of the header row to make it more prominent
  openxlsx::setRowHeights(wb, sheet_name, rows = 1, heights = 20)  # Adjust the height (e.g., 30 pixels)
  
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
