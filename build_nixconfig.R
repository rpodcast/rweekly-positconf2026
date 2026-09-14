library(rix)

rix(
  r_ver = "4.6.1",
  r_pkgs = c(
    "knitr",
    "gh",
    "ggplot2",
    "purrr",
    "httr2",
    "jsonlite",
    "dplyr",
    "tidyr",
    "readr",
    "magick",
    "sf",
    "rnaturalearth",
    "rnaturalearthdata",
    "rmarkdown"
  ),
  system_pkgs = c("imagemagick"),
  ide = "none",
  project_path = ".",
  overwrite = TRUE,
  print = FALSE
)
