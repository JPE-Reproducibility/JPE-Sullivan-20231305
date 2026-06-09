# Solve the joint-profit-maximising fees under no multihoming on the
# restaurant side (maxjointNM). Output:
# output/CF_feefirst/spec<nsim>/maxjointNM_<code>_take2.rds, one per
# market. Consumed by analyze_max_joint.R.
#
# Warm-starts from previously-solved baseline_*_take2.rds equilibria. Inner
# parallelism is over counties within each market; the outer market loop is
# serial.
library(Matrix)
library(FoodDeliveryTools)
library(EconTools)
library(parallel)

main <- function(){
    cf.name <- 'maxjointNM'

    keep.platforms <- c(1, 1, 1, 1)
    NF <- length(keep.platforms)

    nsim.suffix <- '_nsim50'

    inpaths <- collect.inpaths(nsim.suffix)

    outdir <- 'output/CF_feefirst'
    create.dir(outdir)
    outdir <- sprintf('%s/spec%s', outdir, nsim.suffix)
    create.dir(outdir)

    eqm.opts <- list()
    eqm.opts$impose.p.r <- FALSE
    eqm.opts$max.joint  <- TRUE
    eqm.opts$no.comple  <- FALSE
    eqm.opts$no.multi   <- TRUE

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

    eqm.opts$platforms <- keep.platforms

    suffix <- ifelse(eqm.opts$impose.p.r & !grepl('^cap', cf.name), '_fixR', '')

    cbsa.codes <- read.dat(inpaths$inpath.cbsa.codes)

    load.pMC <- TRUE
    eqm.objs <- load.all.eqm.objs.feefirst(nsim.suffix, load.pMC)
    eqm.objs.co <- eqm.objs$county
    markets <- names(eqm.objs.co)

    ncores <- as.integer(Sys.getenv('CF_NCORES', 10))
    cl <- makeCluster(ncores)
    on.exit(stopCluster(cl), add = TRUE)
    parallel::clusterEvalQ(cl, library(Matrix))
    parallel::clusterEvalQ(cl, library(FoodDeliveryTools))
    parallel::clusterEvalQ(cl, library(EconTools))

    for (market in markets){
        solve.market(cl, market, cf.name, eqm.objs.co, eqm.opts,
                     cbsa.codes, outdir, nsim.suffix,
                     suffix)
    }
}

solve.market <- function(cl, market, cf.name, eqm.objs.co, eqm.opts,
                         cbsa.codes, outdir, nsim.suffix,
                         suffix){
    eqm.objs.m <- eqm.objs.co[[market]]
    code <- cbsa.codes$cbsa[which(cbsa.codes$CBSA_name == market)]

    outpath <- sprintf('%s/%s_%s%s_take2.rds', outdir, cf.name, code, suffix)
    init.path <- sprintf('%s/baseline_%s%s_take2.rds', outdir, code, suffix)

    if (file.exists(init.path)){
        init.fees <- readRDS(init.path)
    } else {
        init.fees <- NULL
    }

    counties <- names(eqm.objs.m)

    clusterExport(cl, c("eqm.objs.m", "eqm.opts", "nsim.suffix",
                        "outdir"), envir = environment())

    EQM <- parLapply(cl, counties, fun = solve.county, eqm.objs.m = eqm.objs.m,
                     cf.name = cf.name, eqm.opts = eqm.opts, nsim.suffix = nsim.suffix,
                     outdir = outdir, init.fees = init.fees)
    names(EQM) <- counties

    saveRDS(EQM, outpath)
}

solve.county <- function(co, eqm.objs.m, cf.name, eqm.opts, nsim.suffix,
                         outdir, init.fees){

    if (is.null(init.fees)){
        C.init <- NULL
        R.init <- NULL
    } else {
        C.init <- init.fees[[co]]$C
        R.init <- init.fees[[co]]$R
    }

    eqm.objs.j <- eqm.objs.m[[co]]

    eqm.co <- compute.eqm.feefirst(eqm.objs.j,
                                   cf.name     = cf.name,
                                   eqm.opts    = eqm.opts,
                                   nsim.suffix = nsim.suffix,
                                   outdir      = outdir,
                                   C.init      = C.init,
                                   R.init      = R.init)
    return(eqm.co)
}

main()
