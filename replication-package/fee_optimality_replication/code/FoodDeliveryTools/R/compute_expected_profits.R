compute.expected.profits <- function(dat.m, J.G.sim, num.param, opts,
                                     zips.star = NULL, P.all = NULL){
    # Compute expected profits from joining each platform portfolio
    # for each ZCTA in a market
    #
    # This version applies to the model in which platforms choose consumer
    # fees before restaurants choose which platforms to join
    #
    # Wraps compute.market.profits, converting the probabilities in P.all
    # into expected restaurant counts (the continuum approximation).

    # Find the list of zip codes under consideration
    if (is.null(zips.star)){
        top.zips <- names(dat.m$J.G.1.m)
    } else {
        top.zips <- zips.star
    }

    # Only include ZCTAs with a positive number of buyers in range
    pos.buy.zips <- c()
    for (z in top.zips){
        nbuy.z <- c()
        for (z.z in dat.m$zip.map.1[[z]]){
            z0 <- sub('[ci]$', '', z.z)
            buy.z0 <- dat.m$buy.zip[[z0]]
            nbuy.z[z.z] <-  ifelse(is.null(buy.z0), 0, nrow(buy.z0))
        }
        nbuy.z <- sum(nbuy.z, na.rm = TRUE)
        if (nbuy.z > 0){
            pos.buy.zips <- c(pos.buy.zips, z)
        }
    }
    top.zips <- pos.buy.zips

    # Obtain list of ZIPs
    zips.m <- names(dat.m$J.G.1.m)

    # Obtain vector of # of restaurants in each ZIP
    nres.m <- sapply(zips.m, function(z) sum(dat.m$J.G.1.m[[z]]))

    # Convert probabilities to restaurant counts
    dat.m$J.G.1.m <- lapply(zips.m, function(z) nres.m[z]*P.all[[z]])
    names(dat.m$J.G.1.m) <- zips.m

    # Ensure there are no missing portfolios
    J.lengths <- sapply(dat.m$J.G.1.m, length)
    names(J.lengths) <- names(dat.m$J.G.1.m)
    for (k in which(J.lengths == 0)){
        dat.m$J.G.1.m[[k]] <- rep(0, times = dat.m$nportfolios)
        names(dat.m$J.G.1.m[[k]]) <- grep('^G', colnames(dat.m$resto.m), value = TRUE)
    }

    # Compute profits
    E.Pi <- compute.market.profits(dat.m, num.param = num.param,
                                   opts = opts, selected.zip = NULL,
                                   menu.subset = top.zips, return.revenue = TRUE)
    Rev  <- E.Pi$Revenues
    E.Pi <- E.Pi$Profits

    Rev  <- lapply(top.zips, function(z) Rev[[z]])
    E.Pi <- lapply(top.zips, function(z) E.Pi[[z]])
    names(E.Pi) <- top.zips
    names(Rev)  <- top.zips
    outputs <- list(E.Pi = E.Pi, Rev = Rev)
    return(outputs)
}


