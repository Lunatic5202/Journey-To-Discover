pkgs <- c("tidyverse", "jsonlite", "janitor", "patchwork", "scales",
          "knitr", "here", "rmarkdown", "ggrepel", "viridis",
          "shiny", "shinydashboard", "DT", "plotly", "bslib")

to_install <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(to_install)) {
  install.packages(to_install)
  message("Installed: ", paste(to_install, collapse = ", "))
} else {
  message("All packages already installed.")
}
