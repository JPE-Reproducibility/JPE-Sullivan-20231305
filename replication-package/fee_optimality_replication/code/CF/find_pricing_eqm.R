# Solve the three Bertrand-Nash equilibrium counterfactuals that share the
# same multi-market dispatch pattern:
#   baseline — Bertrand-Nash with all platforms, full complementarity, multihoming
#   NC       — same, but with no complementarity in restaurant adoption costs
#   NM       — same, but with no multihoming on the restaurant side
# Output: output/CF_feefirst/spec<nsim>/<cf.name>_<code>.rds, one per
# (scenario, market). Consumed by analyze_max_joint.R (and the baseline files
# also by compare_priv_soc.R, decompose_rpi_soc_fees.R, variety_vs_fixed_costs_soc_priv.R).
#
# Scenarios are run baseline -> NC -> NM so that NC and NM can warm-start
# from the baseline equilibria written in the first pass. NM additionally
# bumps the initial consumer fee up and the restaurant fee down (adjust.C,
# adjust.R) to help the solver escape the baseline basin.
library(Matrix)
library(FoodDeliveryTools)
library(EconTools)
library(parallel)

main <- function(){

    scenarios <- list(
        list(cf.name = 'baseline', no.comple = FALSE, no.multi = FALSE),
        list(cf.name = 'NC',       no.comple = TRUE,  no.multi = FALSE),
        list(cf.name = 'NM',       no.comple = FALSE, no.multi = TRUE)
    )

    # Preliminaries
    nsim.suffix <- '_nsim50'

    # Which platforms?
    keep.platforms <- c(1, 1, 1, 1)
    NF <- length(keep.platforms)

    # Paths
    inpaths <- collect.inpaths(nsim.suffix)
    outdir <- 'output/CF_feefirst'
    create.dir(outdir)
    outdir <- sprintf('%s/spec%s', outdir, nsim.suffix)
    create.dir(outdir)

    cbsa.codes <- read.dat(inpaths$inpath.cbsa.codes)

    # Load eqm objects (once; reused across scenarios)
    load.pMC <- TRUE
    eqm.objs <- load.all.eqm.objs.feefirst(nsim.suffix, load.pMC)
    eqm.objs.co <- eqm.objs$county
    markets <- names(eqm.objs.co)

    ncores <- min(length(markets), as.integer(Sys.getenv('CF_NCORES', 8)))
    cl <- makeCluster(ncores)
    on.exit(stopCluster(cl), add = TRUE)
    parallel::clusterEvalQ(cl, library(Matrix))
    parallel::clusterEvalQ(cl, library(FoodDeliveryTools))
    parallel::clusterEvalQ(cl, library(EconTools))

    for (sc in scenarios){
        run.scenario(sc, keep.platforms, eqm.objs.co, cbsa.codes,
                     outdir, nsim.suffix, markets, cl)
    }
}

run.scenario <- function(sc, keep.platforms, eqm.objs.co, cbsa.codes,
                         outdir, nsim.suffix, markets, cl){

    cf.name <- sc$cf.name
    if (!all(keep.platforms == 1)){
        prefix <- sprintf('sub%s_', Reduce(paste0, as.character(keep.platforms)))
        cf.name <- paste0(prefix, cf.name)
    }

    # NM is initialised off the baseline equilibria with a bump on the
    # consumer fee (and a small pull on the restaurant fee) so the solver
    # leaves the baseline basin; baseline and NC use no adjustment.
    if (sc$no.multi){
        adjust.C <- 1.50
        adjust.R <- -0.05
    } else {
        adjust.C <- 0
        adjust.R <- 0
    }
    init.fees <- list()
    init.files <- grep('^baseline_[a-z]+\\.rds$', list.files(outdir), value = TRUE)
    for (f in init.files){
        eqm.f <- readRDS(sprintf('%s/%s', outdir, f))
        for (co in names(eqm.f)){
            init.fees[[co]] <- list(C = eqm.f[[co]]$C + adjust.C,
                                    R = eqm.f[[co]]$R + adjust.R)
        }
    }

    # Set options
    eqm.opts <- list()
    eqm.opts$impose.p.r <- FALSE
    eqm.opts$max.joint  <- FALSE
    eqm.opts$no.comple  <- sc$no.comple
    eqm.opts$no.multi   <- sc$no.multi
    eqm.opts$platforms  <- keep.platforms

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

    suffix <- ifelse(eqm.opts$impose.p.r & !grepl('^cap', cf.name), '_fixR', '')

    num.param <- load.num.param()
    num.param$tol.FP   <- 2.5e-7
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

    parLapply(cl, markets, fun = solve.market, cf.name = cf.name,
              eqm.objs.co, eqm.opts = eqm.opts, cbsa.codes = cbsa.codes,
              outdir = outdir, nsim.suffix = nsim.suffix,
              suffix = suffix, num.param = num.param, init.fees = init.fees)
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
