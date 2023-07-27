# Unload all previously loaded packages + remove previous environment
rm(list = ls(all = TRUE))
pacman::p_unload()

# Set working directory
cmd_args <- commandArgs(trailingOnly = FALSE)
has_script_filepath <- startsWith(cmd_args, "--file=")
if (sum(has_script_filepath)) {
    setwd(dirname(unlist(strsplit(cmd_args[has_script_filepath], "=")))[2])
}

# Load libraries
pacman::p_load(glue, data.table, tidyverse, stringr)
devtools::load_all("./", export_all = FALSE)

if (!interactive()) {
    # Define input arguments when running from bash
    parser <- setup_default_argparser(
        description = "Get metadata",
    )
    parser$add_argument("-i", "--input_file",
        type = "character",
        default = NULL, help = "Path to Seurat object"
    )
    args <- parser$parse_args()
} else {
    # Provide arguments here for local runs
    args <- list()
    args$log_level <- 5
}

# Set up logging
# logr <- init_logging(log_level = args$log_level)
# log_info(ifelse(interactive(),
# "Running interactively...",
# "Running from command line/terminal..."
# ))

# log_info("Create output directory...")
# create_dir(args$output_dir)

# Load additional libraries
project_dir <- "/Users/joankant/Library/CloudStorage/OneDrive-UHN/Coding/reqs/"
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
installed_pkgs <- get_installed_pkgs()
used_pkgs_valid <- c(intersect(used_pkgs, installed_pkgs$Package))

installed_pkgs <- get_installed_pkgs()

reqs <- installed_pkgs %>%
    ungroup() %>%
    filter(Package %in% used_pkgs_valid)


reqs <- reqs %>%
    rowwise() %>%
    mutate(source = case_when(
        Package %in% pkgs_cran ~ "CRAN",
        is_github(Package) ~ "GitHub",
        Package %in% pkgs_base ~ "Base",
        Package %in% pkgs_bioconductor ~ "Bioconductor",
        TRUE ~ "Other"
    ))
