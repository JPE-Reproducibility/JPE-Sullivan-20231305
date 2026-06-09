load.eqm.results <- function(pattern, base.dir, cbsa.codes, suffix = ''){
    # Load pricing equilibrium results
    #
    # Inputs
    #   pattern: pattern identifying equilibriu type (at beginning of filename)
    #   base.dir: directory containing equilibrium results
    #   cbsa.codes: data.frame linking CBSA codes with full names
    #   suffix: suffix identifying equilibria

    # Construct list of all files in the base directory
    result.files <- list.files(base.dir)

    # Adjust pattern for grep
    if (suffix == ''){
        pattern.adj <- sprintf('^%s_[a-z]*\\.rds', pattern)
    } else {
        pattern.adj <- sprintf('^%s.*%s\\.rds', pattern, suffix)
    }

    # Find paths
    inpaths  <- grep(pattern.adj, result.files, value = TRUE)
    inpaths <- sprintf('%s/%s', base.dir, inpaths)

    # Read in data
    eqm   <- lapply(inpaths, readRDS)

    # Determine metro area codes
    pattern.adj <- sprintf('.*%s_', pattern)
    remove.suffix <- sprintf('(%s)?.rds', suffix)
    codes <- sub(pattern.adj, '', sub(remove.suffix, '', inpaths))
    eqm.names <- sapply(codes,  function(x) cbsa.codes$CBSA_name[cbsa.codes$cbsa == x])
    names(eqm) <- eqm.names
    return(eqm)
}
