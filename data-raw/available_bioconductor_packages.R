## code to prepare `available_bioconductor_packages` dataset goes here
available_bioconductor_packages <- readRDS("data-raw/available_bioconductor_pkgs.rds")

usethis::use_data(available_bioconductor_packages, overwrite = TRUE)
