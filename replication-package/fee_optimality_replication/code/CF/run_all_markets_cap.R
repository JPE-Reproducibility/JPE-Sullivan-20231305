# Solve the capped-commission counterfactual equilibria for each cap level
# in cap.levels x each market. Output: output/CF_feefirst/spec<nsim>/
# cap<L>_<code>_take2.rds, one per (cap level, market). These files feed the
# cap-variation exhibits via analyze_vary_cap.R, relative_change_plot_take2.R,
# variety_vs_fixed_costs.R, etc.
#
# Cap levels are iterated in descending order so that cap20_*_take2.rds is
# written before cap15, preserving the cap15 warm-start at solve.market().
library(Matrix)
library(FoodDeliveryTools)
library(EconTools)
library(parallel)

main <- function(){

    cap.levels <- 40:15

    # Which platforms?
    keep.platforms <- c(1, 1, 1, 1)
    NF <- length(keep.platforms)

    # Set options
    # Preliminaries
    nsim.suffix <- '_nsim50'

    inpaths <- collect.inpaths(nsim.suffix)

    # Output directory
    outdir <- 'output/CF_feefirst'
    create.dir(outdir)
    outdir <- sprintf('%s/spec%s', outdir, nsim.suffix)
    create.dir(outdir)

    # Set options
    eqm.opts <- list()
    eqm.opts$impose.p.r <- TRUE




    eqm.opts$no.multi <- FALSE
    eqm.opts$no.comple <- FALSE
    eqm.opts$max.joint <- FALSE

    # Numerical parameters

    num.param <- load.num.param()
    num.param$tol.FP   <- 3.5e-7
    num.param$tol.menu <- 1e-4
    num.param$max.iter.menu <- 50
    num.param$learn.rate.menu <- 0.35
    num.param$LR.CR       <- 0.55
    num.param$LR.CR.early <- 0.40
    num.param$tol.C <- 0.01
    num.param$tol.R <- 0.005
    num.param$max.iter.CR <- 100
    if (eqm.opts$no.multi){
        num.param$LR.CR <- 0.65
        num.param$LR.CR.early <- 0.55
    }

    # Which platforms to include?
    eqm.opts$platforms <- keep.platforms


    cbsa.codes <- read.dat(inpaths$inpath.cbsa.codes)

    # Load eqm objects
    load.pMC <- TRUE
    eqm.objs <- load.all.eqm.objs.feefirst(nsim.suffix, load.pMC)

    eqm.objs.co <- eqm.objs$county

    markets <- names(eqm.objs.co)
    markets <- rev(markets)

    # Initialize cluster
    ncores <- as.integer(Sys.getenv('CF_NCORES', 6))
    cl <- makeCluster(ncores)
    parallel::clusterEvalQ(cl, library(Matrix))
    parallel::clusterEvalQ(cl, library(FoodDeliveryTools))
    parallel::clusterEvalQ(cl, library(EconTools))

    for (cap.level in cap.levels){
        solve.cap.level(cl, cap.level, eqm.objs.co, eqm.opts, cbsa.codes,
                        outdir, nsim.suffix)
    }
    stopCluster(cl)
}

solve.cap.level <- function(cl, cap.level, eqm.objs.co, eqm.opts,
                            cbsa.codes, outdir, nsim.suffix){
    cf.name <- sprintf('cap%d', cap.level)
    suffix  <- ''
    markets <- names(eqm.objs.co)
    markets <- rev(markets)
    for (market in markets){
        solve.market(cl, market, cf.name, eqm.objs.co, eqm.opts,
                     cbsa.codes, outdir, nsim.suffix,
                      suffix)
    }
}

solve.market <- function(cl, market, cf.name, eqm.objs.co, eqm.opts,
                         cbsa.codes, outdir, nsim.suffix,
                         suffix){
    # Solve for equilibria in a metro area indicated by "market"
    eqm.objs.m <- eqm.objs.co[[market]]
    code <- cbsa.codes$cbsa[which(cbsa.codes$CBSA_name == market)]

    outpath <- sprintf('%s/%s_%s%s_take2.rds', outdir, cf.name, code, suffix)
    if (cf.name == 'cap15'){
        init.path <- sprintf('%s/cap20_%s%s_take2.rds', outdir, code, suffix)
    } else {
        init.path <- outpath
    }
    if (file.exists(init.path)){
        init.fees <- readRDS(init.path)
    } else {
        init.fees <- NULL
    }

    counties <- names(eqm.objs.m)

    EQM <- parLapply(cl, counties, fun = solve.county, eqm.objs.m = eqm.objs.m,
              cf.name = cf.name, eqm.opts = eqm.opts, nsim.suffix = nsim.suffix,
              outdir = outdir, init.fees = init.fees)
    names(EQM) <- counties

    # Save the equilibria
    saveRDS(EQM, outpath)
}

solve.county <- function(co, eqm.objs.m, cf.name, eqm.opts, nsim.suffix,
                         outdir, init.fees){

    if (is.null(init.fees)){
        C.init <- NULL
    } else {
        C.init <- init.fees[[co]]$C
    }

    eqm.objs.j <- eqm.objs.m[[co]]

    eqm.co <- compute.eqm.feefirst(eqm.objs.j,
                                   cf.name     = cf.name,
                                   eqm.opts    = eqm.opts,
                                   nsim.suffix = nsim.suffix,
                                   outdir      = outdir,
                                   C.init      = C.init)
    return(eqm.co)
}

main()

