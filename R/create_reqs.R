#' Create requirements file
#'
#' @param project_dir path to project directory
#' @param output_dir path to output directory where requirements file will be saved
#'
#' @return dataframe with requirements
#' @export
#' @examples examples create_requirements("~/Coding/R", "~/Coding/reqs")
create_reqs <- function(project_dir, output_dir) {
    # Load additional libraries
    all_files <- list.files(project_dir, recursive = TRUE, full.names = TRUE)
    script_files <- all_files[endsWith(all_files, ".R")]

    implicit_pkgs <- unique(unlist(
        sapply(script_files, get_implicit_pkgs, USE.NAMES = FALSE)
    ))
    loaded_pkgs <- get_pkgs("loaded")
    used_pkgs <- unique(c(loaded_pkgs, implicit_pkgs))

    installed_pkgs <- data.frame(devtools::session_info())
    reqs <- installed_pkgs[intersect(used_pkgs, rownames(installed_pkgs)), c("packages.source", "packages.package", "packages.ondiskversion")]
    colnames(reqs) <- c("source", "package", "version")

    reqs <- reqs %>%
        separate(source, into = c("src", "src_version"), sep = " ", extra = "merge", remove = FALSE) %>%
        mutate(
            src_version = stringr::str_extract_all(
            src_version,
            "(?<=\\().*?(?=\\))"
        )) %>% mutate(pkg_incl_version = ifelse(src == "CRAN",
        paste0(package, "@", version), src_version))
    reqs <- apply(reqs, 2, as.character)
    write.csv(reqs, paste(output_dir, "/requirements.csv"))
    return(reqs)
}