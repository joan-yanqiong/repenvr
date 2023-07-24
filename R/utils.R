#' Get functions from unknown packages
#'
#' @param path_to_script path to script for which you want the functions
#' @return list of functions that are not from an existing package
#' @import NCmisc
get_functions_from_unknown_pkgs <- function(path_to_script) {
    # those for which a package could not be found- may be functions within
    # functions, or from packages that aren't loaded.
    if (!file.exists(path_to_script)) {
        stop("File doesn't exist")
    }
    return(NCmisc::list.functions.in.file(path_to_script)$`character(0)`)
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
