# Define the package list
packages <- c(
  "ggplot2", "dplyr", "readr", "tibble", "stringr", "forcats", "purrr", "tidyr",
  "readxl", "rsdmx", "tools", "knitr", "scales", "viridis",
  "zoo", "lubridate", "ggrepel", "curl", "rmarkdown", "fredr", "eurostat", "Hmisc"
)

# # Install any missing packages
# installed <- packages %in% rownames(installed.packages())
# if (any(!installed)) {
#   install.packages(packages[!installed], dependencies = TRUE)
# }

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))
