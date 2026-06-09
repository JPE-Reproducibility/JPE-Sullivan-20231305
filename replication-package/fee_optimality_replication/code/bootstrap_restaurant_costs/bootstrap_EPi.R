# Compute bootstrap replicates of restaurants' expected profits.
# Iterates over Psi/est_table_<b>.csv (from code/demand_estimation/
# fe_estimation_boot.m) using the matching CCP_outputs<b>.rds from
# bootstrap_ccp.R and restaurant_price_friction_<b>.rds from bootstrap_friction.R.
library(EconTools)
library(FoodDeliveryTools)
library(Matrix)
source('code/bootstrap_restaurant_costs/boot_utils.R')

main <- function(){
    nsim.suffix   <- '_nsim50'

    cl <- setup.cluster()
    on.exit(parallel::stopCluster(cl), add = TRUE)

    DIR <- list(
        rMC.dir = 'data/bootstrap/dat/restoMC',
        CCP.dir = 'data/bootstrap/dat/CCP',
        EPi.dir = 'data/bootstrap/dat/EPi'
    )
    create.dir(DIR$EPi.dir)

    psi <- find.boot.paths()

    # Skip replicates already on disk.
    epi.paths <- sprintf('%s/EPi_%d.rds', DIR$EPi.dir, psi$b)
    keep <- !file.exists(epi.paths)
    boot.paths <- psi$paths[keep]

    inpath.dat <- sprintf('data/eqm_data/eqm_data%s.rds', nsim.suffix)
    dat <- readRDS(inpath.dat)

    results <- parallel::parLapply(cl = cl, X = boot.paths, fun = run.boot,
                                   dat = dat, DIR = DIR)
    report.failures(results, 'bootstrap_EPi')
}


run.boot <- function(boot.path, dat, DIR){
    b <- as.integer(sub('.*est_table_([0-9]+)\\.csv$', '\\1', boot.path))
    outpath <- sprintf('%s/EPi_%d.rds', DIR$EPi.dir, b)
    if (file.exists(outpath)) return(NULL)

    param.tab <- read.dat(boot.path)
    dat <- update.demand.param(param.tab, dat)

    CCP.path   <- sprintf('%s/CCP_outputs%d.rds',       DIR$CCP.dir, b)
    CCP.path.c <- sprintf('%s/CCP_outputs_chain%d.rds', DIR$CCP.dir, b)
    CCP.path.i <- sprintf('%s/CCP_outputs_indep%d.rds', DIR$CCP.dir, b)
    rMC.path   <- sprintf('%s/restaurant_price_friction_%d.rds', DIR$rMC.dir, b)

    missing <- c(CCP = CCP.path, CCP.chain = CCP.path.c, CCP.indep = CCP.path.i,
                 rMC = rMC.path)
    missing <- missing[!file.exists(missing)]
    if (length(missing) > 0){
        return(sprintf('Replicate %d skipped — missing inputs: %s', b,
                       paste(names(missing), collapse = ', ')))
    }

    tryCatch({
        compute.CCP.profits(dat, readRDS(CCP.path), readRDS(CCP.path.c),
                            readRDS(CCP.path.i), readRDS(rMC.path), outpath,
                            Jbar = 1)
        NULL
    }, error = function(cond){
        sprintf('Replicate %d expected profits failed: %s', b, conditionMessage(cond))
    })
}


main()
