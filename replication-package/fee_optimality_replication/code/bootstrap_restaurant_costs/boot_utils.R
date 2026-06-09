# Shared helpers for the restaurant-cost bootstrap pipeline.

# Number of bootstrap replicates expected across the pipeline. Must match
# the B used by the demand-estimation MATLAB FE bootstrap that fills
# data/bootstrap/dat/Psi/.
NBOOT <- 100

# Number of parallel worker processes. Override with the BOOTSTRAP_NCORES
# environment variable (e.g. on a cluster); locally defaults to one less
# than the available cores.
get.ncores <- function(){
    n.env <- Sys.getenv('BOOTSTRAP_NCORES')
    if (nzchar(n.env)) return(as.integer(n.env))
    max(parallel::detectCores() - 1, 1)
}

# Initialise a PSOCK cluster pre-loaded with the packages our bootstrap
# workers need.
setup.cluster <- function(ncores = get.ncores()){
    cl <- parallel::makeCluster(ncores, outfile = '')
    doParallel::registerDoParallel(cl)
    parallel::clusterEvalQ(cl, library(FoodDeliveryTools))
    parallel::clusterEvalQ(cl, library(EconTools))
    parallel::clusterEvalQ(cl, library(Matrix))
    return(cl)
}

# Discover the Psi/est_table_*.csv files written by the demand-estimation
# MATLAB FE bootstrap. Three of the four downstream bootstrap scripts iterate
# over these (one row per replicate). Stops loudly if none are present so a
# replicator doesn't silently get an empty bootstrap.
find.boot.paths <- function(nboot = NBOOT){
    boot.dir <- 'data/bootstrap/dat/Psi'
    if (!dir.exists(boot.dir)){
        stop(sprintf(
            'Bootstrap prerequisite missing: %s does not exist.\n',
            boot.dir),
            'Run code/demand_estimation/fe_estimation_boot.m (the MATLAB ',
            'fixed-effect bootstrap) first; it writes the Psi/est_table_*.csv ',
            'files this pipeline iterates over.')
    }
    paths <- list.files(boot.dir, pattern = '^est_table_[0-9]+\\.csv$',
                        full.names = TRUE)
    if (length(paths) == 0){
        stop(sprintf(
            'Bootstrap prerequisite missing: %s exists but contains no ',
            'est_table_*.csv files. Run code/demand_estimation/',
            'fe_estimation_boot.m first.', boot.dir))
    }
    # Restrict to replicate indices in 1:nboot
    b <- as.integer(sub('.*est_table_([0-9]+)\\.csv$', '\\1', paths))
    keep <- b >= 1 & b <= nboot
    list(paths = paths[keep], b = b[keep])
}

# Convert a parLapply result list (one element per replicate, NULL on success,
# a character message on failure) into a printed summary.
report.failures <- function(results, step.name){
    failures <- Filter(Negate(is.null), results)
    if (length(failures) == 0){
        message(sprintf('%s: all %d replicates completed.',
                        step.name, length(results)))
        return(invisible())
    }
    warning(sprintf('%s: %d of %d replicates failed.',
                    step.name, length(failures), length(results)),
            call. = FALSE, immediate. = TRUE)
    for (f in failures) message('  ', f)
}
