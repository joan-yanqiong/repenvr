#' Install packages from a requirements file
#'
#' @param req_path path to requirements file
#' @export
function(req_path) {
    reqs <- read.csv(req_path, row.names = 1)
    pkgs_to_install <- reqs %>% pull(pkg_incl_version)
    pkgs_to_install <- pkgs_to_install[!is.na(pkgs_to_install)]

    pak::pkg_install(pkgs_to_install)
}
