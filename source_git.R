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
