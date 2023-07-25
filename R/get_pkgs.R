#' Get installed packages
#'
#' @param libpath path to library
#' @return data frame with installed packages
#' @examples get_installed_pkgs()
#' @export
#' @importFrom dplyr %>% mutate rowwise
get_installed_pkgs <- function(libpath = .libPaths()) {
    if (!dir.exists(libpath)) {
        stop("Invalid library path")
    }
    installed_pkgs <- data.frame(installed.packages(lib.loc = libpath)) %>%
        rowwise() %>%
        mutate(
            is_github = is_github(Package),
            pkg_incl_version = ifelse(is_github, get_gh_url(Package), paste0(Package, "@", Version))
        )
    return(installed_pkgs)
}


#' Find package of the function
#'
#' Uses the name of a function to look for the package.
#'
#' @param function_name Name of the function.
#' @param lib.loc Location of the packages that are installed.
#'
#' @return List of the packages where the given function exists
#' @examples
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

#' Extract package from script
#'
#' @param pkg_loader package loader (e.g. library, pacman, require)
#' @param txt text to extract package from
#'
#' @return packages
#' @importFrom stringr str_detect str_extract_all str_split
extract_pkg_from_script <- function(pkg_loader, txt) {
    regex_for_parentheses <- "(?<=\\().*?(?=\\))"
    is_from_loader <- str_detect(txt, pkg_loader)
    tmp_pkg <- as.vector(str_extract_all(txt[is_from_loader], regex_for_parentheses, simplify = TRUE))
    has_multi_pkgs <- str_detect(tmp_pkg, ", ")
    tmp_multi_pkgs <- as.vector(str_split(tmp_pkg[has_multi_pkgs], ", ", simplify = TRUE))
    return(c(tmp_pkg[!has_multi_pkgs], tmp_multi_pkgs))
}

#' Get explicit packages that are used
#'
#' @param path_to_script path to script for which you want the functions
#' @return list of packages
#' @importFrom stringr str_replace_all
#' @export
get_explicit_pkgs <- function(path_to_script) {
    pkg_loaders <- c("library", "pacman", "require")

    # Import lines of code
    lines_of_script <- readLines(path_to_script, encoding = "UTF-8")

    # remove the single quotes that surround the pkgs
    str_without_quotes <- str_replace_all(lines_of_script, '\"', "")

    # find out which entries are libraries, pacman, require
    explicit_pkgs <- lapply(pkg_loaders, extract_pkg_from_script, txt = str_without_quotes)
    return(unique(do.call(c, explicit_pkgs)))
}
