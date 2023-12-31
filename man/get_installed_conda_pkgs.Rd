% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/install_from_reqs.R
\name{get_installed_conda_pkgs}
\alias{get_installed_conda_pkgs}
\title{Detect installed conda packages}
\usage{
get_installed_conda_pkgs(conda_env_path)
}
\arguments{
\item{conda_env_path}{path to conda environment file (yml)}
}
\value{
list of installed conda packages
}
\description{
Use the conda environment file to detect which R packages (and bioconductor)
packages were installed using conda.
}
\examples{
get_installed_conda_pkgs("/Users/JohnDoe/Documents/environment.yml")
}
