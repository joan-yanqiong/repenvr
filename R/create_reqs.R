#' Create requirements file
#'
#' @param project_dir path to project directory
#' @param output_dir path to output directory where requirements file will be saved
#' @param libpath path to library with installed packages
#' @param return_path whether to return path to requirements file
#' @param is_offline whether to run in offline mode, by 1 (TRUE; default) and 0 = FALSE
#' @param installed_pkgs path to installed packages
#' @return dataframe with requirements
#' @export
#' @examples examples create_reqs("~/Coding/R", "~/Coding/reqs")
#' @importFrom dplyr %>% mutate select filter ungroup pull rowwise case_when
#' @importFrom BiocManager available
#' @importFrom tools CRAN_package_db
#' @importFrom curl has_internet
create_reqs <- function(project_dir, output_dir = NULL, libpath = .libPaths(), return_path = TRUE, is_offline = TRUE, installed_pkgs = NULL) {
    # Constants
    cols_oi <- c("Package", "Version", "pkg_incl_version", "source", "conda_install")

    if (!dir.exists(project_dir)) {
        stop("Invalid project path")
    }
    if (is.null(output_dir)) {
        output_dir <- project_dir
    }
    # Add source information
    pkgs_base <- data.frame(installed.packages(priority = "base")) %>% pull(Package)

    # Obtain the available bioconductor packages
    if (!is_offline) {
        if (has_internet()) {
            pkgs_bioconductor <- available()
            pkgs_cran <- data.frame(CRAN_package_db()) %>% pull(Package)
        }
    }

    # Obtain the files to scan for packages, only use R and rmarkdown files Rmd files
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
    if (is.null(installed_pkgs)) {
        installed_pkgs <- get_installed_pkgs(libpath = libpath)
    } else if (file.exists(installed_pkgs)) {
        installed_pkgs <- readRDS(installed_pkgs)
    } else {
        stop("Invalid path to installed packages")
    }
    # Check if package in used packages are matched to installed packages
    matched_pkgs <- unlist(sapply(used_pkgs, is_matched_pkg, installed_pkgs = installed_pkgs), use.names = FALSE)
    used_pkgs <- unique(c(used_pkgs, matched_pkgs))

    used_pkgs_valid <- intersect(used_pkgs, installed_pkgs$Package)
    reqs <- installed_pkgs %>% ungroup() %>%
        filter(Package %in% used_pkgs_valid) %>%
        rowwise() %>%
        mutate(source = case_when(
                is_github_pkg(Package, libpath) ~ "GitHub",
                Package %in% pkgs_cran ~ "CRAN",
                Package %in% pkgs_base ~ "Base",
                Package %in% pkgs_bioconductor ~ "Bioconductor",
                TRUE ~ "Other"
        ), conda_install = case_when(
            source == "CRAN" ~ glue::glue("r-{tolower(Package)}={Version}"),
            source == "Bioconductor" ~ glue::glue("bioconductor-{tolower(Package)}={Version}"),
            .default = "non-conda",
            )
        ) %>%
        select(all_of(cols_oi)) %>%
        write.csv(paste0(output_dir, "/requirements.csv"))
    if(return_path) {
        return(paste0(output_dir, "/requirements.csv"))
    }
    return(reqs)
}
