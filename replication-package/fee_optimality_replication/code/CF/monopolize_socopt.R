# Solve the social-optimum fee path under a platform-monopolist counterfactual:
# only one platform survives (keep.platforms = c(1,0,0,0)), the other three
# are removed from the choice set entirely. Output:
# output/CF_feefirst/spec<nsim>/sub1000_socopt_<code>.rds, one per
# market. The 'sub1000_' prefix is the bitstring encoding of keep.platforms.
# Consumed by compare_priv_soc.R.
library(Matrix)
library(FoodDeliveryTools)
library(EconTools)
library(parallel)

main <- function(){

    cf.name  <- 'socopt'

    # Which platforms?
    keep.platforms <- c(1, 0, 0, 0)
    NF <- length(keep.platforms)
    if (!all(keep.platforms == rep(1, times = NF))){
        keep.prefix <- Reduce(paste0, as.character(keep.platforms))
        prefix <- sprintf('sub%s_', keep.prefix)
        cf.name <- paste0(prefix, cf.name)
    }

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
    eqm.opts$impose.p.r <- FALSE

    # Newer options
    eqm.opts$max.joint  <- FALSE
    eqm.opts$no.comple  <- FALSE
    eqm.opts$no.multi   <- FALSE

    # Which platforms to include?
    eqm.opts$platforms <- keep.platforms

    suffix <- ifelse(eqm.opts$impose.p.r & !grepl('^cap', cf.name), '_fixR', '')

    cbsa.codes <- read.dat(inpaths$inpath.cbsa.codes)

    # Load eqm objects
    load.pMC <- TRUE
    eqm.objs <- load.all.eqm.objs.feefirst(nsim.suffix, load.pMC)

    eqm.objs.co <- eqm.objs$county

    markets <- names(eqm.objs.co)


    num.param <- load.num.param()
    num.param$tol.FP   <- 3.5e-7
    num.param$tol.menu <- 1e-4
    num.param$max.iter.menu <- 30
    num.param$learn.rate.menu <- 0.25

    # Set some numerical parameters
    num.param$LR.CR       <- 0.40
    num.param$LR.CR.early <- 0.40
    num.param$tol.C <- 0.01
    num.param$tol.R <- 0.005
    num.param$max.iter.CR <- 100

    # Parallelization
    ncores <- min(length(markets), as.integer(Sys.getenv('CF_NCORES', 8)))
    cl <- makeCluster(ncores)

    parallel::clusterEvalQ(cl, library(Matrix))
    parallel::clusterEvalQ(cl, library(FoodDeliveryTools))
    parallel::clusterEvalQ(cl, library(EconTools))

    parLapply(cl, markets, fun = solve.market, cf.name = cf.name,
              eqm.objs.co, eqm.opts = eqm.opts, cbsa.codes = cbsa.codes,
              outdir = outdir, nsim.suffix = nsim.suffix,
              suffix = suffix, num.param = num.param)
    stopCluster(cl)
}

solve.market <- function(market, cf.name, eqm.objs.co, eqm.opts,
                         cbsa.codes, outdir, nsim.suffix,
                         suffix, num.param){
    # Solve for equilibria in a metro area indicated by "market"

    eqm.objs.m <- eqm.objs.co[[market]]
    code <- cbsa.codes$cbsa[which(cbsa.codes$CBSA_name == market)]

    EQM <- list()
    counties <- names(eqm.objs.m)
    for (co in counties){
        eqm.objs.j <- eqm.objs.m[[co]]
        EQM[[co]] <- compute.eqm.feefirst(eqm.objs.j,
                                          cf.name     = cf.name,
                                          eqm.opts    = eqm.opts,
                                          nsim.suffix = nsim.suffix,
                                          outdir      = outdir,
                                          num.param   = num.param)
    }

    # Save the equilibria
    outpath <- sprintf('%s/%s_%s%s.rds', outdir, cf.name, code, suffix)
    saveRDS(EQM, outpath)
}

main()

