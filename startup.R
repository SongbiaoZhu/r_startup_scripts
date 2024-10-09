# RStudio Startup Script

# Load necessary packages
install_load <- function(packages) {
  sapply(packages, function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  })
}
install_load(c("tidyverse", "magrittr"))

# Create project folders if they do not exist
folders <- c("data", "res", "doc", "pub")
for (folder in folders) {
  if (!dir.exists(folder)) {
    dir.create(folder)
    message(paste("Created folder:", folder))
  } else {
    message(paste("Folder already exists:", folder))
  }
}

# Set global options
options(stringsAsFactors = FALSE)  # Avoid factors by default
set.seed(123)  # Set seed for reproducibility

# Set default plot settings
theme_set(theme_minimal(base_size = 14))

# Check for package updates (optional)
# update.packages(checkBuilt = TRUE, ask = FALSE)

# Print message indicating the startup script has run successfully
message("RStudio startup script executed successfully.")
