library(rix)

rix(
  r_ver = "4.6.1",
  r_pkgs = c(
    "quarto",
    "knitr",
    "gh",
    "ggplot2",
    "purrr",
    "httr2",
    "jsonlite",
    "dplyr",
    "tidyr",
    "readr"
  ),
  system_pkgs = c("quarto"),
  ide = "none",
  project_path = ".",
  overwrite = TRUE,
  print = FALSE
)
