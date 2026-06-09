# Bootstrap the restaurant-pricing friction parameter phi.
# Iterates over Psi/est_table_<b>.csv (from code/demand_estimation/
# fe_estimation_boot.m). Output: data/bootstrap/dat/restoMC/
# restaurant_price_friction_<b>.rds — consumed by recover_restaurant_costs/
# summarize_costs_v3.R for Table 3 SEs.
library(EconTools)
library(FoodDeliveryTools)
library(Matrix)
source('code/bootstrap_restaurant_costs/boot_utils.R')

main <- function(){
    nsim.suffix <- '_nsim50'

    base.dir <- 'data/bootstrap/dat'
    outdir   <- sprintf('%s/restoMC', base.dir)
    create.dir(outdir)

    psi        <- find.boot.paths()
    boot.paths <- psi$paths

    inpath.dat  <- sprintf('data/eqm_data/eqm_data%s.rds', nsim.suffix)
    inpath.menu <- 'output/numerator_menu_pricing/disagg_results/price_indices.csv'
    inpath.phi  <- 'output/recover_restaurant_costs/retaurant_costs.rds'

    dat  <- readRDS(inpath.dat)
    rhos <- read.csv(inpath.menu)
    phi  <- readRDS(inpath.phi)$phi

    cl <- setup.cluster()
    on.exit(parallel::stopCluster(cl), add = TRUE)

    results <- parallel::parLapply(cl, boot.paths, fun = run.boot,
                                   dat = dat, outdir = outdir,
                                   phi = phi, rhos = rhos)
    report.failures(results, 'bootstrap_friction')
}

run.boot <- function(boot.path, dat, outdir, phi, rhos){
    b <- as.integer(sub('.*est_table_([0-9]+)\\.csv$', '\\1', boot.path))
    outpath <- sprintf('%s/restaurant_price_friction_%d.rds', outdir, b)

    if (file.exists(outpath)) return(NULL)

    tryCatch({
        param.tab <- read.dat(boot.path)
        dat <- update.demand.param(param.tab, dat)

        interval.radius <- 0.14
        interval        <- c(phi - interval.radius, phi + interval.radius)
        tol             <- 1e-3

        opts <- load.opts()

        result <- phi.bisection(dat, rhos, opts, interval = interval, tol = tol, b = b)
        saveRDS(result, outpath)
        NULL
    }, error = function(cond){
        sprintf('Replicate %d phi bisection failed: %s', b, conditionMessage(cond))
    })
}

main()
