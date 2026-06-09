# Solve the two-sided commission-cap counterfactual: simultaneously cap the
# consumer fee and the restaurant commission at the same fraction of the
# basket. Output: output/CF_feefirst/spec<nsim>/twosided<L>_<code>.rds,
# one per (cap level L, market). Consumed by analyze_twosided.R.
#
# Consumer fees under each cap are read from the prior cap30_*_take2.rds runs
# (so run_all_markets_cap.R must have completed first).
library(Matrix)
library(FoodDeliveryTools)
library(EconTools)
library(parallel)

main <- function(){

    cap.levels <- 15:40

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


    # Which platforms to include?
    eqm.opts$platforms <- keep.platforms


    cbsa.codes <- read.dat(inpaths$inpath.cbsa.codes)

    # Load eqm objects
    load.pMC <- TRUE
    eqm.objs <- load.all.eqm.objs.feefirst(nsim.suffix, load.pMC)

    eqm.objs.co <- eqm.objs$county

    markets <- names(eqm.objs.co)

    ## Load fees under 30% cap
    inpaths.30  <- grep('cap30.*take2', list.files(outdir), value = TRUE)
    Fees <- list()
    for (k in 1:length(inpaths.30)){
        path.k <- sprintf('%s/%s', outdir, inpaths.30[k])
        eqm.k <- readRDS(path.k)
        counties <- names(eqm.k)
        for (co in counties){
            Fees[[co]] <- eqm.k[[co]]$C
        }
    }


    # Parallelization
    ncores <- as.integer(Sys.getenv('CF_NCORES', 10))
    cl <- makeCluster(ncores)

    parallel::clusterEvalQ(cl, library(Matrix))
    parallel::clusterEvalQ(cl, library(FoodDeliveryTools))
    parallel::clusterEvalQ(cl, library(EconTools))

    parallel::clusterExport(cl, 'solve.market')

    parLapply(cl, cap.levels, fun = solve.cap.level,
              eqm.objs.co, eqm.opts = eqm.opts, cbsa.codes = cbsa.codes,
              outdir = outdir, nsim.suffix = nsim.suffix,
              Fees = Fees)

    stopCluster(cl)
}

solve.cap.level <- function(cap.level, eqm.objs.co, eqm.opts,
                            cbsa.codes, outdir, nsim.suffix, Fees){
    cf.name <- sprintf('twosided%d', cap.level)
    markets <- names(eqm.objs.co)
    for (market in markets){
        solve.market(market, cf.name, eqm.objs.co, eqm.opts,
                     cbsa.codes, outdir, nsim.suffix,
                     Fees)
    }
}

solve.market <- function(market, cf.name, eqm.objs.co, eqm.opts,
                         cbsa.codes, outdir, nsim.suffix,
                         Fees){
    # Solve for equilibria in a metro area indicated by "market"
    eqm.objs.m <- eqm.objs.co[[market]]
    code <- cbsa.codes$cbsa[which(cbsa.codes$CBSA_name == market)]

    outpath <- sprintf('%s/%s_%s.rds', outdir, cf.name, code)

    if (file.exists(outpath)){
        return(NULL)
    }
    
    EQM <- list()
    counties <- names(eqm.objs.m)
    for (co in counties){
        eqm.objs.j <- eqm.objs.m[[co]]
        fees <- Fees[[co]]

        EQM[[co]] <- compute.eqm.feefirst(eqm.objs.j,
                                          cf.name     = cf.name,
                                          eqm.opts    = eqm.opts,
                                          nsim.suffix = nsim.suffix,
                                          outdir      = outdir,
                                          fees        = fees)
    }

    # Save the equilibria
    saveRDS(EQM, outpath)
}

main()

