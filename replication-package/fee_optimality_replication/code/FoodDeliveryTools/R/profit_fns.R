
compute.expected.utility <- function(p.c, Jp.G, G.mat, buy.z, demand.param, G.indices, Rhos.z = NULL,
                                     use.expected.delta = FALSE){
    # Compute the expected utility of each consumer's problem
    #
    # Inputs
    #   use.expected.delta: use expected delta instead of inclusive value.
    #       This approach corresponds to elminating the epsilon shocks

    # Extract and otherwise prepare inputs
    gamma       <- demand.param$gamma
    Jp.G        <- as.numeric(Jp.G)
    nportfolios <- length(Jp.G)
    nplatforms  <- ncol(G.mat)
    nbuy        <- nrow(buy.z)

    # Compute platform portfolios' inclusive values
    deltas <- compute.deltas(p.c, demand.param, buy.z)
    deltas <- cbind(0, deltas)
    if (use.expected.delta){
        V.G <- compute.expected.delta(deltas, demand.param, G.mat, G.indices,
                                          Rhos.z = Rhos.z, buy.z = buy.z)
    } else {
        V.G <- compute.inclusive.values(deltas, demand.param, G.mat, G.indices,
                                        Rhos.z = Rhos.z, buy.z = buy.z)
    }

    # Compute each consumer's expected utility
    eV.0 <- exp(V.G/gamma)
    eV <- t(t(eV.0)*Jp.G)
    ## Set cells that are sufficiently low to zero
    eV[eV < 1e-20] <- 0

    eV.sum <- rowSums(eV)

    incl.np <- ('mu.eta' %in% names(demand.param))
    if (incl.np){
        # Compute Eta
        Eta <- compute.Eta(demand.param, buy.z)
        # Compute expected utility
        eu <- gamma*log(1 + eV.sum*exp(Eta/gamma))
    } else {
        eu <- gamma*log(eV.sum)
    }

    return(eu)
}

compute.expected.utility.v2 <- function(p.c.z, J.G.z, G.mat, buy.z, demand.param,
                                        G.indices, Rhos = NULL,
                                        use.expected.delta = FALSE,
                                        keep.platforms = NULL){
    # Compute the expected utility of each consumer's problem
    #
    # Inputs
    #   use.expected.delta: use expected delta instead of inclusive value.
    #       This approach corresponds to eliminating the epsilon shocks

    NF <- ncol(G.mat) - 1
    if (is.null(keep.platforms)){
        keep.platforms <- rep(1, times = NF)
    }
    keep.G <- determine.portfolios(G.mat, keep.platforms)

    # Extract and otherwise prepare inputs
    gamma       <- demand.param$gamma
    nportfolios <- length(J.G.z[[1]])
    nplatforms  <- ncol(G.mat)
    nbuy        <- nrow(buy.z)

    Z.z <- names(J.G.z)

    # Compute platform portfolios' inclusive values
    deltas <- compute.deltas(p.c.z, demand.param, buy.z, incl.0 = TRUE)

    if (use.expected.delta){
        V.G <- compute.expected.delta.v2(deltas, demand.param, G.mat, G.indices,
                                         Rhos = Rhos, buy.z = buy.z)
        eV.0 <- lapply(V.G, function(x) exp(x/demand.param$gamma))
        names(eV.0) <- names(V.G)
    } else {
        # Compute an inclusive value for each nZIP (nearby ZIP)
        V.G  <- list()
        eV.0 <- list()
        for (z.z in Z.z){
            Rhos.z <- Rhos[[z.z]]
            is.chain <- chain.stat(z.z)
            V.G[[z.z]] <- compute.inclusive.values(deltas, demand.param,
                                                   G.mat, G.indices,
                                                   Rhos.z = Rhos.z,
                                                   buy.z = buy.z,
                                                   is.chain = is.chain)
            eV.0[[z.z]] <- exp(V.G[[z.z]]/demand.param$gamma)
        }
    }

    # Drop portfolios
    for (z.z in names(eV.0)){
        V.G[[z.z]]  <- V.G[[z.z]][, keep.G]
        eV.0[[z.z]] <- eV.0[[z.z]][, keep.G]
    }

    # Compute each consumer's expected utility
    eV <- list()
    eV.sum <- list() # sum within a ZIP

    n.keep.G <- length(keep.G)
    for (z.z in Z.z){
        eV[[z.z]] <- t(t(eV.0[[z.z]])*J.G.z[[z.z]][keep.G])
        ## Set cells that are sufficiently low to zero
        eV[[z.z]][eV[[z.z]] < 1e-20] <- 0
        eV.sum[[z.z]] <- .rowSums(eV[[z.z]], m = nbuy, n = n.keep.G)
    }
    ## Aggregate eV.sum across ZIPs
    eV.totsum <- Reduce('+', eV.sum)

    incl.np <- ('mu.eta' %in% names(demand.param))
    if (incl.np){
        # Compute Eta
        Eta <- compute.Eta(demand.param, buy.z)
        # Compute expected utility
        eu <- gamma*log(1 + eV.totsum*exp(Eta/gamma))
    } else {
        eu <- gamma*log(eV.totsum)
    }

    return(eu)
}


set.exclusive.Rhos <- function(Rhos, opts, G.mat){
    # Adjust menu prices if specified by options
    if (!is.null(opts) & !is.null(Rhos)){
        if ('cf.name' %in% names(opts)){
            if (opts$cf.name == 'exclusive'){
                # Set high menu prices for online platforms on non-exclusive platform portfolios
                non.exclusive <- which(rowSums(G.mat) > 2)
                for (z in names(Rhos)){
                    for (k in non.exclusive){
                        rzk <- Rhos[[z]][[k]]
                        Rhos[[z]][[k]][2:length(rzk)] <- 1000
                    }
                }
            }
        }
    }
    return(Rhos)
}


check.zip.for.nearby.buyers <- function(z, dat.m){
    # Check if there are buyers nearby the ZCTA.
    z0 <- sub('[ic]$', '', z)
    Z.z <- dat.m$zip.map[[z0]]
    Z.z <- intersect(Z.z, names(dat.m$buy.zip))
    buyers <- sapply(Z.z, function(z.z) nrow(dat.m$buy.zip[[z.z]]))
    val <- sum(buyers) > 0
    return(val)
}

compute.zip.profits <- function(z, Sales.mats, zip.map, resto.param, p.r,
                                G.mat, Rhos = NULL, return.revenue = FALSE){
    # Compute profits under each platform subset for a restaurant
    # in a particular ZIP
    #
    # Inputs
    #   z: ZIP of interest
    #   Sales.mats: Sales in each zip code
    #   zip.map: Mapping between zip codes and set of in-range zip codes

    # Determine which zip codes are in range
    Z.z <- zip.map[[z]]
    Z.z0 <- sub('[ci]$', '', Z.z)
    Z.z0 <- intersect(Z.z0, names(Sales.mats))

    # Add up the sales across zip codes
    Sales.j <- Reduce('+', lapply(Z.z0, function(z.z) Sales.mats[[z.z]][, , z]))

    # Compute the profit from joining each platform portfolio
    if (is.null(Rhos)){
        Rhos.z <- NULL
    } else {
        Rhos.z <- Rhos[[z]]
    }
    mc <- resto.param$mc
    nplatforms <- ncol(G.mat)
    test.mode <- (z == 'a')

    if (test.mode & length(mc) > 1){
        mc.off.z <- median(mc$off)
        mc.on.z  <- median(mc$on)
        mc.z <- c(mc.off.z, rep(mc.on.z, times = nplatforms - 1))
    } else if (length(mc) > 1){
        if (length(mc[['on']] > 1)){
            mc.off.z <- mc[['off']][z]
            mc.on.z  <- mc[['on']][z]
            mc.z <- c(mc.off.z, rep(mc.on.z, times = nplatforms - 1))
        } else {
            mc.z <- NULL
        }
    } else {
        mc.z <- NULL
    }
    profit.G <- compute.profit.mat(Sales.j, p.r, resto.param, G.mat,
                                   Rhos.z = Rhos.z, mc.z = mc.z,
                                   return.revenue = return.revenue)

    return(profit.G)
}


compute.profit.mat <- function(Sales.j, p.r, resto.param, G.mat,
                               Rhos.z, mc.z = NULL,
                               return.revenue = FALSE){
    # Compute the profit from joining each platform portfolio
    #
    # Inputs
    #   Sales.j: Restaurant j's sales on each platform when a member of each
    #       platform portfolio
    #   p.r: Commission rates charged by platforms to restaurants
    nportfolios <- nrow(Sales.j)
    nplatforms  <- ncol(Sales.j)

    if (is.null(mc.z)){
        mc <- resto.param$mc
        het.profits <- length(mc) > 1
        if (het.profits){
            mc.z <- c(mc['off'], rep(mc['on'], times = nplatforms - 1))
        }
    }

    markup.mat <- pracma::zeros(nportfolios, nplatforms)
    rho.mat    <- markup.mat

    p.r.alt <- c(0, p.r) # To account for the direct channel

    for (g in 1:nportfolios){
        idx.g <- which(G.mat[g, ] == 1)
        rho.g <- as.numeric(Rhos.z[[g]])
        if (!return.revenue){
            markup.mat[g, idx.g] <- (1 - p.r.alt[idx.g])*rho.g - mc.z[idx.g]
        } else {
            rho.mat[g, idx.g] <- rho.g
        }
    }

    if (!return.revenue){
        profit.mat <- Sales.j*markup.mat
        profit.mat[Sales.j == 0] <- 0
    } else {
        profit.mat <- Sales.j*rho.mat
        profit.mat[Sales.j == 0] <- 0
    }
    # Compute platform-subset-level profits (before accounting for fixed costs)
    profit.G <- rowSums(profit.mat)

    return(profit.G)
}


