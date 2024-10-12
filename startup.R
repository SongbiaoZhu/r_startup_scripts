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

# Function to source scripts from Gitee or GitHub
source_git <- function(script_names = c("startup.R", "functions.R"),
                       base = "gitee") {
  # base can be gitee, or github
  
  # Define the base URLs for Gitee and GitHub
  base_url_gitee <- "https://gitee.com/zhu_song_biao/r_startup_scripts/raw/main/"
  base_url_github <- "https://raw.githubusercontent.com/SongbiaoZhu/r_startup_scripts/main/"
  
  # Select the correct base URL
  base_url <- if (base == "gitee")
    base_url_gitee
  else
    base_url_github
  
  # Loop through the script names and source them
  for (script in script_names) {
    script_url <- paste0(base_url, script)
    message("Sourcing: ", script_url)
    source(script_url)
  }
}
# source_git(
#   script_names = c("functions.R", "make_r_friendly_names.R"),
#   base = "gitee"
# )

# Set paths
data_path <- "data/"
res_path <- "res/"
doc_path <- "doc/"
pub_path <- "pub/"

# Create project folders if they do not exist
folders <- c(data_path, res_path, doc_path, pub_path)

lapply(folders, function(folder) {
  if (!dir.exists(folder)) {
    dir.create(folder)
    message(paste("Created folder:", folder))
  } else {
    message(paste("Folder already exists:", folder))
  }
})

# Set global options
options(stringsAsFactors = FALSE)  # Avoid factors by default
set.seed(123)  # Set seed for reproducibility

# Print message indicating the startup script has run successfully
message("RStudio startup script executed successfully.")
