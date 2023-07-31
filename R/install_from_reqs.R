#' Detect installed conda packages
#'
#' Use the conda environment file to detect which R packages (and bioconductor)
#' packages were installed using conda.
#'
#' @param conda_env_path path to conda environment file (yml)
#' @return list of installed conda packages
#' @examples
#' get_installed_conda_pkgs("/Users/JohnDoe/Documents/environment.yml")
#' @export
#' @importFrom stringr str_match
#' @importFrom yaml read_yaml
get_installed_conda_pkgs <- function(conda_env_path) {
    conda_env <- read_yaml(conda_env_path)
    conda_pkgs <- conda_env$dependencies

    conda_python <- str_match(conda_pkgs, "python=(.*)")[, 2]
    conda_python_version <- conda_python[!is.na(conda_python)]

    conda_r <- str_match(conda_pkgs, "r-base=(.*)")[, 2]
    conda_r_version <- conda_r[!is.na(conda_r)]

    r_pkgs <- str_match(conda_pkgs[is.na(conda_r)], "r-(.*)")[, 2]
    r_pkgs <- r_pkgs[!is.na(r_pkgs)]

    bioconda_pkgs <- str_match(conda_pkgs[is.na(conda_r)], "bioconductor-(.*)")[, 2]
    bioconda_pkgs <- bioconda_pkgs[!is.na(bioconda_pkgs)]

    return(c(r_pkgs, bioconda_pkgs))
}

#' Install packages from a requirements file
#'
#' Install packages using a requirements file (csv) using the exact versions,
#' except for Bioconductor packages (to be implemented in the future; dependent
#' on pak).
#' In case you installed packages directly with conda, you can skip those
#' packages, then supply `conda_env_path`.
#'
#' @param req_path path to requirements file
#' @param libpath path to library (default = .libPaths())
#' @param upgrade upgrade packages (default = FALSE)
#' @param conda_env_path path to conda environment file. If provided, remove
#' conda packages from packages that need to be installed
#' @export
#' @importFrom dplyr %>% pull filter
#' @importFrom BiocManager available
#' @importFrom pak pkg_install
#' @importFrom tools CRAN_package_db
install_from_reqs <- function(req_path, libpath = .libPaths(), upgrade = FALSE, conda_env_path = "") {
    if (!file.exists(req_path)) {
        stop("Invalid path to requirements file.")
    }
    reqs <- read.csv(req_path, row.names = 1)
    if (nrow(reqs) < 1) {
        stop("No packages to install.")
    }
    if (file.exists((conda_env_path))) {
        # Remove packages that are installed with conda
        pkgs_installed_with_conda <- get_installed_conda_pkgs(conda_env_path)
        reqs <- reqs %>% filter(!(Package %in% pkgs_installed_with_conda))
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
    pak::pkg_install(c(avail_known, avail_unknown), lib = libpath, upgrade = upgrade)

    # TODO: installing specific bioconductor packages not possible yet with pak
    # adapt when this changes
    reqs %>%
        filter(source == "Bioconductor", Package %in% pkgs_bioconductor) %>%
        pull(Package) %>%
        pak::pkg_install(., lib = libpath, upgrade = upgrade)
}
