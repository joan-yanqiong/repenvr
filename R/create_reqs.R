#' Create requirements file
#'
#' @param project_dir path to project directory
#' @param output_dir path to output directory where requirements file will be saved
#'
#' @return dataframe with requirements
#' @export
#' @examples examples create_requirements("~/Coding/R", "~/Coding/reqs")
#' @importFrom dplyr %>% mutate
#' @importFrom stringr str_extract_all
#' @importFrom tidyr separate
#' @importFrom devtools session_info
create_requirements <- function(project_dir, output_dir) {
    # Load additional libraries
    all_files <- list.files(project_dir, recursive = TRUE, full.names = TRUE)
    script_files <- all_files[endsWith(all_files, ".R")]

    # Combine loaded, implicitly and explicitly used packages
    implicit_pkgs <- unique(unlist(
        sapply(script_files, get_implicit_pkgs, USE.NAMES = FALSE)
    ))

    explicit_pkgs <- unique(unlist(
        sapply(script_files, get_explicit_pkgs, USE.NAMES = FALSE)
    ))
    loaded_pkgs <- get_pkgs("loaded")
    used_pkgs <- unique(c(loaded_pkgs, implicit_pkgs, explicit_pkgs))

    # Compare to the installed packages
    installed_pkgs <- data.frame(session_info())
    reqs <- installed_pkgs[intersect(used_pkgs, rownames(installed_pkgs)), c("packages.source", "packages.package", "packages.ondiskversion")]
    colnames(reqs) <- c("source", "package", "version")

    reqs <- reqs %>%
        separate(source, into = c("src", "src_version"), sep = " ", extra = "merge", remove = FALSE) %>%
        mutate(
            src_version = str_extract_all(
            src_version,
            "(?<=\\().*?(?=\\))"
        )) %>% mutate(pkg_incl_version = ifelse(src == "CRAN",
        paste0(package, "@", version), src_version))
    reqs <- apply(reqs, 2, as.character)
    write.csv(reqs, paste0(output_dir, "/requirements.csv"))
    return(reqs)
}
