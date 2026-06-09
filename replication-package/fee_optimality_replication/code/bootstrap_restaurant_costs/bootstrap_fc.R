# Estimate restaurant fixed-cost parameters for each bootstrap replicate.
# Iterates over Psi/est_table_<b>.csv (from code/demand_estimation/
# fe_estimation_boot.m) using the matching EPi_<b>.rds from bootstrap_EPi.R.
# Output: data/bootstrap/dat/restoFC/restaurant_FC_<b>.rds — consumed
# by restaurant_FC_estimation/GMM_CCP_table.R for Table 4 SEs.
library(EconTools)
library(FoodDeliveryTools)
library(Matrix)
source('code/bootstrap_restaurant_costs/boot_utils.R')

main <- function(){
    nsim.suffix <- '_nsim50'

    cl <- setup.cluster()
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterEvalQ(cl, library(data.table))

    EPi.dir <- 'data/bootstrap/dat/EPi'
    FC.dir  <- 'data/bootstrap/dat/restoFC'
    create.dir(FC.dir)

    psi <- find.boot.paths()

    # Skip replicates already on disk.
    fc.paths <- sprintf('%s/restaurant_FC_%d.rds', FC.dir, psi$b)
    keep <- !file.exists(fc.paths)
    boot.paths <- psi$paths[keep]

    inpaths <- collect.inpaths(nsim.suffix)
    FC.BL <- readRDS(inpaths$inpath.FC.est)
    sigma.start <- FC.BL$sigma.hat

    inpath.dat <- sprintf('data/eqm_data/eqm_data%s.rds', nsim.suffix)
    dat <- readRDS(inpath.dat)

    results <- parallel::parLapply(cl = cl, X = boot.paths,
                                   fun = run.boot.est,
                                   dat = dat, FC.dir = FC.dir, EPi.dir = EPi.dir,
                                   sigma.start = sigma.start)
    report.failures(results, 'bootstrap_fc')
}


run.boot.est <- function(boot.path, dat, FC.dir, EPi.dir, sigma.start){
    b <- as.integer(sub('.*est_table_([0-9]+)\\.csv$', '\\1', boot.path))
    outpath  <- sprintf('%s/restaurant_FC_%d.rds', FC.dir, b)
    EPi.path <- sprintf('%s/EPi_%d.rds',           EPi.dir, b)

    if (!file.exists(EPi.path)){
        return(sprintf('Replicate %d skipped — missing EPi (%s)', b, EPi.path))
    }
    EPi <- readRDS(EPi.path)

    tryCatch({
        estimate.restaurant.FCs.v2(dat, EPi, outpath, prob.spec = 'ccp',
                                   minimal = TRUE, outpath.plot = NULL,
                                   sigma.start = sigma.start)
        NULL
    }, error = function(cond){
        sprintf('Replicate %d FC estimation failed: %s', b, conditionMessage(cond))
    })
}

main()
