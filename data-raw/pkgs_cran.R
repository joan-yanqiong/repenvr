## code to prepare `pkgs_cran` dataset goes here
pkgs_cran <- readRDS("data-raw/pkgs_cran.rds")
usethis::use_data(pkgs_cran, overwrite = TRUE)
