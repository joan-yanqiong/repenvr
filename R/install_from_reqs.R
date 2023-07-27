#' Install packages from a requirements file
#'
#' @param req_path path to requirements file
#' @export
#' @importFrom dplyr %>% pull filter
#' @importFrom pak pkg_install
install_from_reqs <- function(req_path, libpath) {
    if (!file.exists(req_path)) {
        stop("Invalid path to requirements file.")
    }
    reqs <- read.csv(req_path, row.names = 1)
    if (nrow(reqs) < 1) {
        stop("No packages to install.")
    }

    # Check if package is available, only base, cran and bioconductor packages
    pkgs_base <- data.frame(installed.packages(priority = "base")) %>% pull(Package)
    pkgs_bioconductor <- available()
    pkgs_cran <- data.frame(CRAN_package_db()) %>% pull(Package)
    pkgs_available <- c(pkgs_base, pkgs_bioconductor, pkgs_cran)

    # install packages
    avail_known <- reqs %>%
        filter(source %in% c("CRAN", "Base")) %>%
        pull(pkg_incl_version)
    avail_unknown <- reqs %>%
        filter(source %in% c("Github", "Other")) %>%
        pull(pkg_incl_version)
    # Install packages from all packages except bioconductor
    pak::pkg_install(c(avail_known, avail_unknown), lib = libpath)

    # TODO: installing specific bioconductor packages not possible yet with pak
    # adapt when this changes
    reqs %>%
        filter(source == "Bioconductor", Package %in% pkgs_bioconductor) %>%
        pull(Package) %>%
        pak::pkg_install(., lib = libpath)
}
