#' Find package of the function
#'
#' Uses the name of a function to look for the package.
#'
#' @param function_name Name of the function.
#' @param lib.loc Location of the packages that are installed.
#'
#' @return List of the packages where the given function exists
#' @examples
#' # ADD_EXAMPLES_HERE
find_pkg_for_function <- function(function_name, lib.loc = .libPaths()) {
    out <- help.search(paste0("^", function_name, "$"),
        agrep = FALSE,
        lib.loc = lib.loc
    )
    return(out$matches[, "Package"])
}
#' Get packages
#'
#' @param pkgs category of packages to obtain: loaded (default), attached or installed
#' @return list of packages
#
#' @examples get_pkgs("loaded")
#' @export
#' @importFrom devtools session_info
get_pkgs <- function(pkgs = c("loaded", "attached", "installed")[1], include_base = TRUE) {
    return(session_info(pkgs = pkgs, info = "packages", include_base = include_base)$packages$package)
}

#' Get list of implicit packages
#'
#' @param path_to_script path to Rscript for which you want to obtain the
#' implicitly called packges
#' @return return
#'
#' @examples examples
#' @source https://stackoverflow.com/questions/59725527/list-all-the-packages-required-in-a-script-assuming-packagefunction-in-r
#' @export
get_implicit_pkgs <- function(path_to_script) {
    if (!file.exists(path_to_script)) {
        stop("No valid path to script.")
    }
    lines_of_script <- readLines(path_to_script)
    parsed_script <- parse(text = lines_of_script)
    parsed_script <- getParseData(parsed_script)
    # Extract packages
    return(parsed_script$text[parsed_script$token == "SYMBOL_PACKAGE"])
}
