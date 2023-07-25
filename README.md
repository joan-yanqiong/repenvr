# REProducible ENVironments for R projects
Package to enable reproducible environments for you R projects. 

## Installation

`devtools::install_github("joan-yanqiong/repenvr")`

or 

`pak::pkg_install("joan-yanqiong/repenvr")`

or 

`pacman::p_install("joan-yanqiong/repenvr")`

## Usage

`create_requirements()` needs `project_dir`, `output_dir`, and optionally `libpath` (path to where your packages are installed, by default `.libPaths()`) 
1. Scan the R files in a directory for packages that are used in your code.
2. Create requirements.csv file for reproducing the same environment

**NOTE**: in case you use conda environments and your R packages are installed in that environment, then either 1) set `libpath` to the corresponding path, or 2) activate your environment, start R and run this `create_requirements(project_dir, output_dir)`.

`install_from_reqs()` needs path to requirements file.
