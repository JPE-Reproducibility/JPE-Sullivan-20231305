compute.market.profits <- function(dat.m, num.param, opts, selected.zip = NULL,
                                   menu.subset = NULL, Rhos = NULL,
                                   keep.platforms = NULL,
                                   return.Rho = FALSE,
                                   return.revenue = FALSE){
    # Compute the profits of restaurants in each zipcode in a market under each possible choice
    # of platform portfolio.
    #
    # Inputs
    #   dat.m$J.G.m: list of restaurants' portfolio choices (specific to market m)
    #   dat.m$buy.m: transactions dataset (specific to market m)
    #   dat.m$fees: consumer fees charged by platforms in each ZIP
    #   dat.m$comm: commissions charged to restaurants by platforms in
    #       each ZIP
    #   dat.m$demand.param: parameters of the demand model
    #   opts$resto.param: parameters of the restaurant model
    #   dat.m$zip.mat.m: mapping between zip codes and sets of zip codes in range of these zip codes
    #   selected.zip: Use a single zip code's data?
    #   menu.subset: Subset of zip code for which to find menu pricing equilibrium
    #   Rhos: fixed restaurant prices. Provide "NULL" to compute a pricing eqm

    NF <- dat.m$nplatforms - 1
    if (is.null(keep.platforms)){
        keep.platforms <- rep(1, times = NF)
    }
    keep.G <- determine.portfolios(dat.m$G.mat, keep.platforms = keep.platforms)
    drop.G <- setdiff(1:dat.m$nportfolios, keep.G)

    # Extract options
    resto.param     <- opts$resto.param
    verbose         <- opts$verbose

    # Compute restaurants in range of the zip code
    J.G.m <- dat.m$J.G.1.m
    zip.map <- dat.m$zip.map.1
    # Extract portfolio membership matrix
    G.mat <- dat.m$G.mat

    # Determine menu prices
    if (is.null(Rhos)){
        mc <- resto.param$mc
        Rhos <- menu.price.eqm(dat.m, mc, num.param = num.param,
                               menu.subset = menu.subset,
                               opts = opts, verbose = FALSE,
                               keep.platforms = keep.platforms)
    }

    # Compute sales in each ZIP
    Sales.mats <- compute.zip.sales(dat.m, Rhos = Rhos,
                                    selected.zip = selected.zip,
                                    opts = opts,
                                    keep.platforms = keep.platforms)

    # Compute profits
    Profits  <- list()
    Revenues <- list()
    if (is.null(selected.zip)){
        # Compute profits for all zip codes with a positive number of restaurants
        # Determine which zip codes have positive numbers of restaurants
        loop.zips <- names(J.G.m)
        zips.pos <- sapply(loop.zips, function(x) sum(J.G.m[[x]]) > 0)
        loop.zips <- loop.zips[which(zips.pos)]
    } else {
        loop.zips <- selected.zip
    }

    # Determine commissions
    for (z in loop.zips){
        p.r <- dat.m$comm[[z]]
        # Compute profits
        ## Check if there are any nearby buyers
        buyers.present <- check.zip.for.nearby.buyers(z, dat.m)
        sales.present  <- check.nearby.sales(z, dat.m, Sales.mats)
        ## If so, compute profits
        if (buyers.present & sales.present){
            Profits[[z]] <- compute.zip.profits(z, Sales.mats, zip.map,
                                                resto.param, p.r,
                                                G.mat, Rhos = Rhos)

            ## Set profits for dropped portfolios to a very negative number
            Profits[[z]][drop.G] <- -Inf

            if (return.revenue){
                Revenues[[z]] <- compute.zip.profits(z, Sales.mats, zip.map,
                                                    resto.param, p.r,
                                                    G.mat, Rhos = Rhos,
                                                    return.revenue = TRUE)
            }
        }
    }

    if (!return.Rho & !return.revenue){
        outputs <- Profits
    } else {
        outputs <- list()
        outputs$Profits <- Profits
        if (return.Rho){
            outputs$Rhos <- Rhos
        } else {
            outputs$Revenues <- Revenues
        }
    }
    return(outputs)
}

check.nearby.sales <- function(z, dat.m, Sales.mats){
    Z.z <- dat.m$zip.map.1[[z]]
    Z.z0 <- sub('[ci]$', '', Z.z)
    Z.z0 <- intersect(Z.z0, names(Sales.mats))
    sales.present <- length(Z.z0) > 0
    return(sales.present)
}


