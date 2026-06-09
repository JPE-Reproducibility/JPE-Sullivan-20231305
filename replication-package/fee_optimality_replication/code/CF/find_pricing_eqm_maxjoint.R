# Solve the joint-profit-maximising fees counterfactual: platforms set fees
# to maximise the sum of profits across all platforms (rather than each
# maximising its own profit in Bertrand-Nash competition). Output:
# output/CF_feefirst/spec<nsim>/maxjoint_<code>.rds, one per market.
# Consumed by analyze_max_joint.R.
#
# Warm-starts from prior maxjoint_*.rds runs if present (self-bootstrap).
library(Matrix)
library(FoodDeliveryTools)
library(EconTools)
library(parallel)

main <- function(){

    cf.name  <- 'maxjoint'

    # Preliminaries
    nsim.suffix <- '_nsim50'

    # Which platforms?
    keep.platforms <- c(1, 1, 1, 1)
    NF <- length(keep.platforms)
    if (!all(keep.platforms == rep(1, times = NF))){
        keep.prefix <- Reduce(paste0, as.character(keep.platforms))
        prefix <- sprintf('sub%s_', keep.prefix)
        cf.name <- paste0(prefix, cf.name)
    }

    # Paths
    ## To inputs
    inpaths <- collect.inpaths(nsim.suffix)
    ## To outputs
    outdir <- 'output/CF_feefirst'
    create.dir(outdir)
    outdir <- sprintf('%s/spec%s', outdir, nsim.suffix)
    create.dir(outdir)

    # Read CBSA codes
    cbsa.codes <- read.dat(inpaths$inpath.cbsa.codes)

    # Specify initial fees
    if (grepl('NM', cf.name)){
        adjust.C <- 1.50
        adjust.R <- -0.05
    } else {
        adjust.C <- 0
        adjust.R <- 0
    }
    init.fees <- list()
    cf.dir <- sprintf('output/CF_feefirst/spec%s', nsim.suffix)
    init.files <- grep('^maxjoint_[a-z]+\\.rds$', list.files(cf.dir), value = TRUE)
    for (f in init.files){
        inpath.f <- sprintf('%s/%s', cf.dir, f)
        eqm.f <- readRDS(inpath.f)
        counties <- names(eqm.f)
        for (co in counties){
            C.co <- eqm.f[[co]]$C + adjust.C
            R.co <- eqm.f[[co]]$R + adjust.R
            init.fees[[co]] <- list(C = C.co, R = R.co)
        }
    }

    # Set options
    eqm.opts <- list()
    eqm.opts$impose.p.r <- FALSE
    eqm.opts$max.joint  <- TRUE
    eqm.opts$no.comple  <- FALSE
    eqm.opts$no.multi   <- FALSE

    ## Ensure the CF name and eqm.opts are compatible
    if (grepl('NC', cf.name) & !eqm.opts$no.comple){
        stop('NC in cf.name but no.comple is turned off')
    }
    if (!grepl('NC', cf.name) & eqm.opts$no.comple){
        stop('NC not in cf.name but no.comple is turned on')
    }
    if (grepl('NM', cf.name) & !eqm.opts$no.multi){
        stop('NM in cf.name but no.multi is turned off')
    }
    if (!grepl('NM', cf.name) & eqm.opts$no.multi){
        stop('NM not in cf.name but no.multi is turned on')
    }
    if (grepl('maxjoint', cf.name) & !eqm.opts$max.joint){
        stop('maxjoint in cf.name but eqm.opts$max.joint is turned off')
    }
    if (!grepl('maxjoint', cf.name) & eqm.opts$max.joint){
        stop('maxjoint not in cf.name but eqm.opts$max.joint is turned on')
    }


    # Which platforms to include?
    eqm.opts$platforms <- keep.platforms

    suffix <- ifelse(eqm.opts$impose.p.r & !grepl('^cap', cf.name), '_fixR', '')

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
    num.param$LR.CR       <- 0.55
    num.param$LR.CR.early <- 0.40
    num.param$tol.C <- 0.01
    num.param$tol.R <- 0.005
    num.param$max.iter.CR <- 100
    if (eqm.opts$no.multi){
        num.param$LR.CR <- 0.65
        num.param$LR.CR.early <- 0.55
    }

    ncores <- min(length(markets), as.integer(Sys.getenv('CF_NCORES', 8)))
    cl <- makeCluster(ncores)

    parallel::clusterEvalQ(cl, library(Matrix))
    parallel::clusterEvalQ(cl, library(FoodDeliveryTools))
    parallel::clusterEvalQ(cl, library(EconTools))
    parLapply(cl, markets, fun = solve.market, cf.name = cf.name,
              eqm.objs.co, eqm.opts = eqm.opts, cbsa.codes = cbsa.codes,
              outdir = outdir, nsim.suffix = nsim.suffix,
              suffix = suffix, num.param = num.param, init.fees = init.fees)
    stopCluster(cl)
}

solve.market <- function(market, cf.name, eqm.objs.co, eqm.opts,
                         cbsa.codes, outdir, nsim.suffix,
                         suffix, num.param, init.fees = NULL){
    # Solve for equilibria in a metro area indicated by "market"

    eqm.objs.m <- eqm.objs.co[[market]]
    code <- cbsa.codes$cbsa[which(cbsa.codes$CBSA_name == market)]

    EQM <- list()
    counties <- names(eqm.objs.m)
    for (co in counties){
        eqm.objs.j <- eqm.objs.m[[co]]

        if (is.null(init.fees)){
            C.init <- NULL
            R.init <- NULL
        } else {
            init.fees.j <- init.fees[[co]]
            C.init <- init.fees.j$C
            R.init <- init.fees.j$R
        }

        EQM[[co]] <- compute.eqm.feefirst(eqm.objs.j,
                                          cf.name     = cf.name,
                                          eqm.opts    = eqm.opts,
                                          nsim.suffix = nsim.suffix,
                                          outdir      = outdir,
                                          num.param   = num.param,
                                          C.init      = C.init,
                                          R.init      = R.init)
    }

    # Save the equilibria
    outpath <- sprintf('%s/%s_%s%s.rds', outdir, cf.name, code, suffix)
    saveRDS(EQM, outpath)
}

main()

