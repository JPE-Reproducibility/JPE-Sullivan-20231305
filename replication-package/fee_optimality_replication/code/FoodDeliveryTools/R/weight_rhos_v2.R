
weight.rhos.v2 <- function(Sales.tots, Rhos, dat.m, zip.subset = NULL){
    # Return the average menu price for each platform for sales made in particular ZIP, weighted by sales
    zips0 <- names(Sales.tots)
    if (!is.null(zip.subset)){
        zips0 <- intersect(zips0, zip.subset)
    }

    zip.map <- dat.m$zip.map.1

    G.mat     <- dat.m$G.mat
    G.indices <- dat.m$G.indices

    Rhos.avg <- list()
    for (z0 in zips0){
        # Compute sales-weighted average prices by looping over
        # ZIPs, platform subsets, and platforms

        ## Identify nearby ZIPs
        Z.z <- names(Sales.tots[[z0]][1, 1, ])

        ## Extract sales at nearby ZIPs
        S <- list()
        for (z.z in Z.z){
            S[[z.z]] <- Sales.tots[[z0]][, , z.z]
        }
        if (length(S) == 0){
            next
        }

        ## Compute sales-weighted average prices
        rhos.avg <- rep(0, times = dat.m$nplatforms)
        for (f in 1:dat.m$nplatforms){
            weights <- c()
            rhos    <- c()
            G.idx <- which(G.mat[, f] == 1)
            for (z.z in names(S)){
                for (g in G.idx){
                    fg.idx <- which(G.indices[[g]] == f)
                    rho.fgz <- Rhos[[z.z]][[g]][fg.idx]
                    sales.fgz <- S[[z.z]][g, f]

                    rhos <- c(rhos, rho.fgz)
                    weights <- c(weights, sales.fgz)

                }
            }
            rhos.avg[f] <- weighted.mean(rhos, weights)
        }
        Rhos.avg[[z0]] <- rhos.avg
    }

    if (length(Rhos.avg) == 1){
        R <- matrix(Rhos.avg[[1]], nrow = 1)
        rownames(R) <- zips0
    } else {
        R <- Reduce(rbind, Rhos.avg)
        rownames(R) <- zips0
    }
    R[is.nan(R)]  <- 0

    return(R)
}


weight.rhos.by.resto <- function(Sales.tots, Rhos, dat.m, mc.on, zip.subset = NULL){
    # Return the average menu price for each platform for sales made by a
    # restaurant particular ZIP, weighted by sales

    # Preliminaries
    resto.zips <- names(dat.m$J.G.1.m)
    G.mat <- dat.m$G.mat[, 2:5]

    # Initialize outputs
    Mean.markups <- list()
    PROFIT       <- list()
    SALES        <- list()

    for (rz in resto.zips){
        mc.rz <- mc.on[rz]
        Profits.all <- list()
        Sales.all   <- list()
        for (k in 1:length(Sales.tots)){
            zip.k <- names(Sales.tots)[k]
            Sm <- Sales.tots[[k]]
            idx <- which(dimnames(Sm)[[3]] == rz)
            if (length(idx) == 1){
                sales.k <- Sm[, 2:5, idx]

                # Compute markups
                Rhos.rz <- Rhos[[rz]]
                profits.k <- sales.k
                for (g in 2:nrow(profits.k)){
                    rhos.g <- Rhos.rz[[g]]
                    rhos.g <- rhos.g[2:length(rhos.g)]
                    idx.platform <- which(G.mat[g, ] == 1)
                    profits.k[g, idx.platform] <- (rhos.g - mc.rz)*sales.k[g, idx.platform]
                }
                Profits.all[[zip.k]] <- profits.k
                Sales.all[[zip.k]]   <- sales.k
            }
        }

        if (length(Profits.all) == 0){
            next
        }
        Profits.all <- Reduce('+', Profits.all)
        Sales.all   <- Reduce('+', Sales.all)
        mean.markup <- Profits.all/Sales.all
        mean.markup[is.nan(mean.markup)] <- 0
        ## Weight by portfolio
        mean.markup.by.f <- c()
        for (f in 1:ncol(mean.markup)){
            mean.markup.by.f[f] <- weighted.mean(mean.markup[, f], Sales.all[, f])
        }
        Mean.markups[[rz]] <- mean.markup.by.f

        PROFIT[[rz]] <- Profits.all
        SALES[[rz]]  <- Sales.all
    }

    Profit.tot  <- Reduce('+', PROFIT)
    Sales.tot   <- Reduce('+', SALES)
    markups.tot <- Profit.tot/Sales.tot
    markups.tot[is.nan(markups.tot)] <- 0
    ## Weight by portfolio
    markups.tot.by.f <- c()
    for (f in 1:ncol(markups.tot)){
        markups.tot.by.f[f] <- weighted.mean(markups.tot[, f], Sales.tot[, f])
    }

    outputs <- list()
    outputs$markups.by.zip <- Mean.markups
    outputs$markups.tot    <- markups.tot.by.f

    return(outputs)
}
