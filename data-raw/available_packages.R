## code to prepare `available_bioconductor_packages` dataset goes here
pkgs_bioconductor <- readRDS("data-raw/available_bioconductor_pkgs.rds")
pkgs_cran <- readRDS("data-raw/available_cran_packages.rds")

usethis::use_data(pkgs_bioconductor, pkgs_cran, overwrite = TRUE, internal = TRUE)
