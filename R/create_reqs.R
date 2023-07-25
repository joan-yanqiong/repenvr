#' Create requirements file
#'
#' @param project_dir path to project directory
#' @param output_dir path to output directory where requirements file will be saved
#'
#' @return dataframe with requirements
#' @export
#' @examples examples create_requirements("~/Coding/R", "~/Coding/reqs")
#' @importFrom dplyr %>% mutate select filter ungroup
create_requirements <- function(project_dir, output_dir, libpath = .libPaths(), return_path = TRUE) {
    # Load additional libraries
    all_files <- list.files(project_dir, recursive = TRUE, full.names = TRUE)
    script_files <- all_files[endsWith(all_files, ".R")]

    # Combine currently loaded, implicitly and explicitly used packages
    implicit_pkgs <- unique(unlist(
        sapply(script_files, get_implicit_pkgs, USE.NAMES = FALSE)
    ))

    explicit_pkgs <- unique(unlist(
        sapply(script_files, get_explicit_pkgs, USE.NAMES = FALSE)
    ))
    loaded_pkgs <- get_pkgs("loaded")

    used_pkgs <- unique(c(implicit_pkgs, explicit_pkgs))

    # Compare to the installed packages
    installed_pkgs <- get_installed_pkgs(libpath = libpath)
    used_pkgs_valid <- intersect(used_pkgs, installed_pkgs$Package)
    print(length(used_pkgs_valid))
    reqs <- installed_pkgs %>% ungroup() %>%
        filter(Package %in% used_pkgs_valid) %>%
        select(Package, Version, LibPath, pkg_incl_version) %>%
        write.csv(paste0(output_dir, "/requirements.csv"))
    if(return_path) {
        return(paste0(output_dir, "/requirements.csv"))
    }
    return(reqs)
}
