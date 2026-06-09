load.all.eqm.objs <- function(nsim.suffix = '_nsim250'){
    # Load all objects used in computing equilibria

    inpaths <- collect.inpaths(nsim.suffix)
    dat    <- readRDS(inpaths$inpath.dat)
    geo    <- load.geo(inpaths$inpath.geo)
    rMC    <- readRDS(inpaths$inpath.rMC)
    pp.est <- readRDS(inpaths$inpath.FC.est)
    cbsa.codes <- EconTools::read.dat(inpaths$inpath.cbsa.codes)

    markets <- names(dat)

    # Update phi
    for (market in markets){
        dat[[market]]$phi <- rMC$phi
    }

    eqm.objs <- list()

    for (market in markets){
        code.m <- cbsa.codes$cbsa[which(cbsa.codes$CBSA_name == market)]
        dat.m <- dat[[market]]
        G.mat  <- dat.m$G.mat
        mc.on  <- rMC$costs[[market]]$costs.on
        mc.off <- rMC$costs[[market]]$costs.off
        mc.m <- list(on = mc.on, off = mc.off)
        zips.m  <- names(dat.m$J.G.1.m)

        ## Market-specific options
        opts <- load.opts(verbose = TRUE)
        opts$resto.param$mc <- mc.m

        ## Restaurant FCs
        kappa <- construct.kappa(pp.est, market, zips.m)

        # Package outputs together
        eqm.objs.m <- list(dat.m = dat.m,
                           opts = opts,
                           kappa = kappa,
                           code.m = code.m)

        eqm.objs[[market]] <- eqm.objs.m
    }

    return(eqm.objs)
}
