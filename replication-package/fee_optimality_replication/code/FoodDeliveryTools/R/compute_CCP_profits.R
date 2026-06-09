compute.CCP.profits <- function(dat, CCPs, CCPs.c, CCPs.i, rMC.est,
                                outpath, outdir = NULL,
                                n.multi.sim = 15, Jbar = 0){
    # Use estimated CCPs and empirical frequencies to compute expected profits
    # of joining certain platform portfoios
    #
    # Parallelization removed

    markets <- names(dat)

    num.param <- load.num.param()
    num.param$n.multi.sim <- n.multi.sim

    opts <- load.opts()
    opts$verbose         <- TRUE

    # Process CCPs
    P   <- process.CCPs(CCPs)
    P.c <- process.CCPs(CCPs.c)
    P.i <- process.CCPs(CCPs.i)

    #== Compute expected profits and revenues for each market ==#
    outputs <- lapply(X = markets,
                      FUN = compute.market.EPis, dat = dat,
                      P = P, P.c = P.c, P.i = P.i, Jbar = Jbar, rMC.est = rMC.est,
                      num.param = num.param, opts = opts, outdir = outdir)
    names(outputs) <- markets
    # Profits
    EPi <- lapply(markets, function(m) outputs[[m]]$EPi.m)
    names(EPi) <- markets
    # Revenues
    Rev <- lapply(markets, function(m) outputs[[m]]$Rev)
    names(Rev) <- markets

    #== Save results ==#
    saveRDS(EPi, file = outpath)
    outpath.rev <- sub('EPi_', 'Rev_', outpath)
    saveRDS(Rev, outpath.rev)
}

compute.market.EPis <- function(market, dat, P, P.c, P.i, Jbar, rMC.est,
                                num.param, opts, outdir){
    # Compute expected profits for a particular market; this is a wrapper for
    # compute.EPi that performs some data preparation

    pracma::fprintf('Market: %s\n', market)

    # Extract data and choice probabilities
    dat.m <- dat[[market]]
    P.m   <- P[[market]]
    P.c.m <- P.c[[market]]
    P.i.m <- P.i[[market]]
    ## Combined list
    P.all <- list()
    for (z in names(P.c.m)){
        z.c <- paste0(z, 'c')
        P.all[[z.c]] <- P.c.m[[z]]
    }
    for (z in names(P.i.m)){
        z.i <- paste0(z, 'i')
        P.all[[z.i]] <- P.i.m[[z]]
    }

    # Process marginal costs
    dat.m$phi <- rMC.est$phi
    rMC.m <- rMC.est$costs[[market]]
    mc.on  <- rMC.m$costs.on
    mc.off <- rMC.m$costs.off
    opts$resto.param$mc <- list(on = mc.on, off = mc.off)

    # Determine ZCTAs for which to compute profits
    J.G.sum <- sapply(dat.m$J.G.1.m, sum)
    names(J.G.sum) <- names(dat.m$J.G.1.m)

    if (is.null(Jbar)){
        zips.star <- names(P.all)
        zips.star <- intersect(zips.star, names(J.G.sum)[J.G.sum >= 1])
    } else {
        zips.star <- names(J.G.sum)[J.G.sum >= Jbar]
    }

    if (market == "New York-Newark-Jersey City, NY-NJ-PA") {
         keep.counties <- c('Westchester County NY', 'New York County NY',
                            'Bergen County NJ', 'Queens County NY',
                            'Kings County NY', 'Essex County NJ',
                            'Bronx County NY',
                            'Nassau County NY', 'Richmond County NY')
         ny.zips0 <- dat.m$resto.m$zip[which(dat.m$resto.m$county %in% keep.counties)]
         ny.zips <- c(paste0(ny.zips0, 'c'),
                      paste0(ny.zips0, 'i'))
         zips.star <- intersect(zips.star, ny.zips)
    }
    # Compute expected profits
    EPi.m <- compute.EPi(market, dat.m, P.all, num.param, opts, zips.star)
    Rev.m <- EPi.m$Rev
    EPi.m <- EPi.m$E.Pi

    if (!is.null(outdir)){
        # Save the market-specific results
        market.label <- sub('-.*$', '', market)
        market.label <- gsub(' ', '_', market.label)
        outpath <- sprintf('%s/%s_continuum.rds', outdir, market.label)
        print(paste0('Saving: ', outpath))
        saveRDS(EPi.m, file = outpath)

        ## Save revenues
        outpath <- sprintf('%s/revenues_%s_continuum.rds', outdir, market.label)
        saveRDS(Rev.m, file = outpath)
    }

    outputs <- list(EPi.m = EPi.m, Rev.m = Rev.m)
    return(outputs)
}

compute.EPi <- function(market, dat.m, P.all, num.param, opts, zips.star){
    # Compute expected profits for the subset of ZCTAs given by `zips.star`

    J.G.sim  <- NULL
    # Compute expected profits
    print('Computing expected profits')
    E.Pi <- compute.expected.profits(dat.m, J.G.sim, num.param, opts,
                                     zips.star = zips.star, P.all = P.all)
    Rev  <- E.Pi$Rev
    E.Pi <- E.Pi$E.Pi

    outputs <- list(E.Pi = E.Pi, Rev = Rev)
    return(outputs)
}

