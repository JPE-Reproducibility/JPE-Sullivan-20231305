load.all.eqm.objs.feefirst <- function(nsim.suffix, load.pMC){
    # Load all objects used in computing equilibria, adapted to the model
    # variant in which consumer fees are selected in the first stage
    # By default, load data objects for the pricing zones that are subregions
    # of the original markets
    #
    # Inputs
    #   nsim.suffix: suffix indicating number of simulates
    #   load.pMC: load platform marginal costs? This argument should be
    #       FALSE when used in platform marginal cost estimation and
    #       TRUE when used for solving equilibria in the platform
    #       pricing game

    inpaths <- collect.inpaths(nsim.suffix)
    dat    <- readRDS(inpaths$inpath.dat)
    geo    <- load.geo(inpaths$inpath.geo)
    rMC    <- readRDS(inpaths$inpath.rMC)
    pp.est <- readRDS(inpaths$inpath.FC.est)
    cbsa.codes <- EconTools::read.dat(inpaths$inpath.cbsa.codes)

    markets <- names(dat)

    # Initialize options
    opts <- load.opts(verbose = TRUE)

    # Load sub-market definitions
    pzones <- readRDS(inpaths$inpath.regions)

    ## Platform marginal costs in data.frame form
    if (load.pMC){
        pMC <- load.pMC.by.county(nsim.suffix)
    }

    # County-level markets
    eqm.objs.co <- list()

    for (market in markets){
        code.m <- cbsa.codes$cbsa[which(cbsa.codes$CBSA_name == market)]
        dat.m <- dat[[market]]
        G.mat  <- dat.m$G.mat

        # Restaurant marginal costs
        dat.m$phi <- rMC$phi
        mc.m <- list(on  = rMC$costs[[market]]$costs.on,
                     off = rMC$costs[[market]]$costs.off)
        zips.m  <- names(dat.m$J.G.1.m)
        zips0.m <- names(dat.m$buy.zip)

        # Platform marginal costs
        if (load.pMC){
            pMC.m  <- pMC[[market]]
        } else {
            pMC.df <- NULL
        }

        pzones.m  <- pzones[[market]]
        pzones.co <- pzones.m$Zones.c

        Dat.co <- list()

        n.co <- length(pzones.co)
        for (j in 1:n.co){
            zone.name <- names(pzones.co)[j]
            keep.zips <- pzones.co[[j]]
            if (load.pMC){
                pMC.co <- pMC.m[[j]]
            } else {
                pMC.co <- NULL
            }
            Dat.co[[zone.name]] <- package.zone.data(keep.zips, dat.m,
                                                     mc.m, opts, pp.est,
                                                     market, pMC.co)
        }
        eqm.objs.co[[market]] <- Dat.co
    }
    eqm.objs <- list()
    eqm.objs$county <- eqm.objs.co
    return(eqm.objs)
}

load.pMC.by.county <- function(nsim.suffix){
    # Load files containing platforms' marginal costs
    indir <- sprintf('output/estimate_platform_costs/MC_H/spec%s',
                     nsim.suffix)
    pMC.files <- list.files(indir)
    pMC.files <- grep('_full\\.rds', pMC.files, value = TRUE)
    names(pMC.files) <- sub('(_full)?\\.rds$', '', pMC.files)
    inpath.codes <- collect.inpaths(nsim.suffix)$inpath.cbsa.codes
    codes <- read.dat(inpath.codes)

    pMC <- list()
    markets <- sort(codes$CBSA_name)
    for (market in markets){
        # Extract market costs
        code.m <- codes$cbsa[which(codes$CBSA_name == market)]
        file.m <- sprintf('%s/%s', indir, pMC.files[code.m])
        pMC.m  <- readRDS(file.m)
        pMC.m  <- pMC.m$by.co
        counties.m <- names(pMC.m)

        # Store costs
        pMC[[market]] <- list()
        for (co in counties.m){
            pMC[[market]][[co]] <- pMC.m[[co]]$MC.joint
        }
    }
    return(pMC)
}

package.zone.data <- function(keep.zips, dat.m, mc.m, opts, pp.est, market,
                              pMC.co){
    # Package data for the pricing zone consisting of `keep.zips`
    dat.j <- limit.to.pricing.zone(dat.m, keep.zips)

    # Restaurants' marginal costs
    mc.j <- mc.m
    zips.j <- names(dat.j$J.G.1.m)
    mc.j$off <- mc.j$off[which(names(mc.j$off) %in% zips.j)]
    mc.j$on  <- mc.j$on[which(names(mc.j$on) %in% zips.j)]

    # Options
    opts.j <- opts
    opts.j$resto.param$mc <- mc.j

    # Restaurant FCs
    kappa.j <- construct.kappa(pp.est, market, zips.j)

    # Platform marginal costs
    if (!is.null(pMC.co)){
        zips0 <- names(dat.j$fees)
        zips.pMC <- rownames(pMC.co[[1]])
        zips  <- intersect(zips0, zips.pMC)
        pMC.df <- do.call(cbind, pMC.co)
        pMC.j <- pMC.df[zips, ]

        # formatting
        pMC.j <- as.data.frame(pMC.j)
        colnames(pMC.j) <- paste0('mc', 1:ncol(pMC.j))
        pMC.j$zip <- rownames(pMC.j)
    }

    # Store objects
    eqm.objs.j <- list()
    eqm.objs.j$dat.m  <- dat.j
    eqm.objs.j$mc.m   <- mc.j
    eqm.objs.j$opts   <- opts.j
    eqm.objs.j$kappa  <- kappa.j
    if (!is.null(pMC.co)){
        eqm.objs.j$pMC.df <- pMC.j
    }

    return(eqm.objs.j)
}


