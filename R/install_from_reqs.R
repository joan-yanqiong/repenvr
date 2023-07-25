#' Install packages from a requirements file
#'
#' @param req_path path to requirements file
#' @export
#' @importFrom dplyr %>% pull
#' @importFrom pak pkg_install
install_from_reqs <- function(req_path) {
    if (!file.exists(req_path)) {
        stop("Invalid path to requirements file.")
    }
    reqs <- read.csv(req_path, row.names = 1)
    pkgs_to_install <- reqs %>% pull(pkg_incl_version)
    pkgs_to_install <- pkgs_to_install[!is.na(pkgs_to_install)]
    pkg_install(pkgs_to_install)
}
