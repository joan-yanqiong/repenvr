#' Get functions from unknown packages
#'
#' @param path_to_script path to script for which you want the functions
#' @return list of functions that are not from an existing package
#' @importFrom NCmisc list.functions.in.file
get_functions_from_unknown_pkgs <- function(path_to_script) {
    # those for which a package could not be found- may be functions within
    # functions, or from packages that aren't loaded.
    if (!file.exists(path_to_script)) {
        stop("File doesn't exist")
    }
    return(list.functions.in.file(path_to_script)$`character(0)`)
}

#' Get functions defined by user
#'
#' Given a script, extracts all the functions and checks whether these functions
#' are defined by the user. This is done by looking up whether the function is
#' part of package that is either loaded, attached or installed.
#'
#' @param path_to_script path to script for which you want the functions
#' @param pkgs_to_incl packages to include
#' @return list of functions that are not from an existing package
#'
#' @examples examples
#' @export
get_user_functions <- function(path_to_script = NULL, pkgs_to_incl = c("loaded", "attached", "installed")[3]) {
    unknown_functions <- get_functions_from_unknown_pkgs(path_to_script)
    potential_pkgs <- sapply(unknown_functions, find_pkg_for_function, USE.NAMES = TRUE)
    incl_pkgs <- get_pkgs(pkgs = pkgs_to_incl, include_base = TRUE)
    if (is.null(path_to_script)) {
        return(names(potential_pkgs)[!(potential_pkgs %in% incl_pkgs)])
    }
    if (!file.exists(path_to_script)) {
        stop("Invalid path")
    }
    # Add packages that are implicitly used (e.g. "stringr::str_detect")
    implicit_pkgs <- get_implicit_pkgs(path_to_script)
    # print(implicit_pkgs)
    print(potential_pkgs)
    print(names(potential_pkgs)[!(potential_pkgs %in% unique(c(incl_pkgs, implicit_pkgs)))])

    return(names(potential_pkgs)[!(potential_pkgs %in% c(incl_pkgs, implicit_pkgs))])
}

#' Check if package is from GitHub
#'
#' @param pkg package name
#' @param libpath path to library
#' @return TRUE if package is from GitHub
#' @examples is_github_pkg("tidyverse") # FALSE
#' is_github("tidyverse/tidyverse") # TRUE
#' @export
is_github_pkg <- function(pkg, libpath) {
    !is.null(packageDescription(pkg, lib.loc = libpath)$GithubRepo)
}
#' Get GitHub URL for package
#'
#' @param pkg package name
#' @param libpath path to library
#' @return GitHub URL for package
#' @examples get_gh_url("tidyverse") # NULL
#' @export
get_gh_url <- function(pkg, libpath = .libPaths()) {
    info <- packageDescription(pkg, lib.loc = libpath)
    if (is.null(info$GithubRepo)) {
        warning("Package is not from GitHub")
        return(NULL)
    }
    return(paste0(info$GithubUsername, "/", info$GithubRepo, "@", info$GithubSHA1))
}

#' Valid package
#'
#' Check whether a package is valid based on dataframe from get_installed_pkgs.
#'
#' @param pkg package name
#' @param installed_pkgs dataframe from get_installed_pkgs()
#'
#' @return package name without version or error if package is not valid
#' @examples is_matched_pkg("tidyverse", c("tidyverse", "dplyr")) # "tidyverse"
#' @export
#' @importFrom stringr str_detect
is_matched_pkg <- function(pkg, installed_pkgs) {
    try(
        expr = {
            is_valid_pkg <- str_detect(installed_pkgs$pkg_incl_version, pkg)
            if (sum(is_valid_pkg) > 0) {
                return(installed_pkgs[is_valid_pkg, "Package"] %>% pull())
            }
        }, silent = TRUE
    )
}
