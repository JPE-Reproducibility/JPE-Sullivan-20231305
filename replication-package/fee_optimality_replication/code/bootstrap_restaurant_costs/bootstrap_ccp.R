# Produce bootstrap replicates of the CCP estimates.
library(FoodDeliveryTools)
source('code/bootstrap_restaurant_costs/boot_utils.R')

main <- function(){
    nsim.suffix <- '_nsim50'

    outdir <- 'data/bootstrap/dat'
    EconTools::create.dir(outdir)
    outdir <- sprintf('%s/CCP', outdir)
    EconTools::create.dir(outdir)

    cl <- setup.cluster()
    on.exit(parallel::stopCluster(cl), add = TRUE)

    inpath.dat <- sprintf('data/eqm_data/eqm_data%s.rds', nsim.suffix)
    dat <- readRDS(inpath.dat)

    results <- parallel::parLapply(cl = cl, X = seq_len(NBOOT), fun = run.boot,
                                   dat = dat, outdir = outdir)
    report.failures(results, 'bootstrap_ccp')
}


run.boot <- function(b, dat, outdir){
    outpath.b <- sprintf('%s/CCP_outputs%d.rds', outdir, b)
    if (file.exists(outpath.b)) return(NULL)

    set.seed(b)
    for (market in names(dat)){
        dat[[market]] <- take.bootstrap.draw.v2(dat[[market]], return.dat = TRUE)
    }

    tryCatch({
        CCP.estimation(dat, outpath.b, minimal = TRUE)
        NULL
    }, error = function(cond){
        sprintf('Replicate %d CCP estimation failed: %s', b, conditionMessage(cond))
    })
}

main()
