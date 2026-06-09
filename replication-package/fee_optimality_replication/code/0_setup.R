# 0_setup.R
#
# One-time install script for the R dependencies of the
# "Fee Optimality in a Two-Sided Market" replication package.
#
# Run from the repository root:
#   Rscript code/0_setup.R
#
# Installs:
#   1. All CRAN packages used anywhere in code/
#   2. The two locally-developed R packages shipped under code/
#      (FoodDeliveryTools, EconTools)
#
# Idempotent: packages already installed are skipped.

# Locate the repository root. When launched via Rscript from the repo root,
# this is just getwd(). When launched from elsewhere, allow REPO_ROOT to be
# passed as an environment variable.
repo_root <- Sys.getenv("REPO_ROOT", unset = getwd())
if (!dir.exists(file.path(repo_root, "code/FoodDeliveryTools"))) {
    stop(sprintf(
        "Could not find %s. Launch from the repository root or set REPO_ROOT.",
        file.path(repo_root, "code/FoodDeliveryTools")))
}

cran_packages <- c(
    "AER",
    "data.table",
    "doBy",
    "dplyr",
    "fixest",
    "geosphere",
    "Matrix",
    "parallel",
    "pracma",
    "RColorBrewer",
    "R.matlab",
    "scales",
    "stringdist",
    "stringi",
    "stringr",
    "tidyr",
    "wesanderson"
)

install_if_missing <- function(pkg, ...) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        message(sprintf("Installing %s ...", pkg))
        install.packages(pkg, ...)
    } else {
        message(sprintf("%-20s OK", pkg))
    }
}

# CRAN
default_repo <- getOption("repos")
if (is.null(default_repo) || identical(default_repo, c(CRAN = "@CRAN@"))) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
}
for (pkg in cran_packages) install_if_missing(pkg)

# noncensus (a dependency of EconTools) was archived from CRAN, so a plain
# install.packages() fails on a fresh machine. Three-tier install:
#   1. CRAN, in case the package is ever restored there;
#   2. the version-pinned tarball from the CRAN archive (preferred fallback:
#      hosted by CRAN itself, pins v0.1, needs no extra packages; noncensus
#      is data-only, so the source install requires no compilers);
#   3. the author's GitHub repository (ramhiser/noncensus), as a last resort
#      should the archive URL ever change.
if (!requireNamespace("noncensus", quietly = TRUE)) {
    try(install.packages("noncensus"), silent = TRUE)
    if (!requireNamespace("noncensus", quietly = TRUE)) {
        message("CRAN install of noncensus failed; trying CRAN archive...")
        try(install.packages(
            paste0("https://cran.r-project.org/src/contrib/Archive/",
                   "noncensus/noncensus_0.1.tar.gz"),
            repos = NULL, type = "source"), silent = TRUE)
    }
    if (!requireNamespace("noncensus", quietly = TRUE)) {
        message("CRAN archive install of noncensus failed; trying GitHub...")
        install_if_missing("remotes")
        remotes::install_github("ramhiser/noncensus")
    }
} else {
    message("noncensus            OK")
}

# Local packages
local_pkgs <- c("EconTools", "FoodDeliveryTools")
for (pkg in local_pkgs) {
    pkg_path <- file.path(repo_root, "code", pkg)
    message(sprintf("Installing local package %s from %s ...", pkg, pkg_path))
    install.packages(pkg_path, repos = NULL, type = "source")
}

message("\nAll R dependencies installed.")
