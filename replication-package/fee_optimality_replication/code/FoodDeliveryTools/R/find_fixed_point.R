find.fixed.point <- function(kappa, dat.m, opts, num.param,
                             more.outputs = FALSE,
                             J.G.0 = NULL, Rhos = NULL,
                             by.demo = FALSE, J.m = NULL,
                             keep.platforms = NULL){
    # Find an equilibrium in restaurants' choices of platform portfolios
    # when consumer fees are set in a previous stage of the model
    #

    #
    # Inputs
    #   kappa: parameters of the restaurant fixed costs model
    #   dat.m$fees: platform consumer fees
    #   dat.m$comm: Platform commission rates
    #   dat.m: data
    #   num.param: Numerical parameters
    #   J.m: overwrite J equilibrium with these restaurant counts
    #   Rhos: fixed restaurant prices. Provide "NULL" to compute a pricing eqm

    NF <- dat.m$nplatforms - 1
    if (is.null(keep.platforms)){
        keep.platforms <- rep(1, times = NF)
    }
    keep.G <- determine.portfolios(dat.m$G.mat, keep.platforms)
    drop.G <- setdiff(1:dat.m$nportfolios, keep.G)

    # Extract numerical parameters
    max.iter.FP <- num.param$max.iter.FP
    tol.FP      <- num.param$tol.FP
    learn.rate  <- num.param$learn.rate.FP

    # Extract options
    verbose      <- opts$verbose
    resto.param  <- opts$resto.param
    mc.m         <- resto.param$mc

    joint.max <- TRUE
    Rho.iter  <- FALSE

    # Extract data for the market
    buy.m     <- dat.m$buy.m
    resto.m   <- dat.m$resto.m
    J.G.m     <- dat.m$J.G.1.m
    fees      <- dat.m$fees
    zip.mat.m <- dat.m$zip.mat.m
    Mu.f      <- dat.m$Mu.f

    # Select initial restaurant decisions
    if (!is.null(J.m)){
        J.G.0 <- J.m
    } else if (is.null(J.G.0)){
        J.G.0 <- J.G.m
    }
    zips.m <- names(J.G.m)
    nres.m <- sapply(zips.m, function(x) sum(J.G.0[[x]]))

    # Subset J.G to include only zip codes with at least one restaurant
    zips.pos <- zips.m[which(nres.m[zips.m] > 0)]
    J.G.0 <- lapply(zips.pos, function(z) J.G.0[[z]])
    names(J.G.0) <- zips.pos

    # If platforms have been dropped, re-assign restaurants in J.G.0 to
    # portfolios excluding dropped platforms
    if (length(drop.G) > 0){
        for (z in names(J.G.0)){
            n.drop <- sum(J.G.0[[z]][drop.G])
            J.G.0[[z]][1] <- J.G.0[[z]][1] + n.drop
            J.G.0[[z]][drop.G] <- 0
        }
    }

    for (iter in 1:max.iter.FP){
        # Compute the profitability of each restaurant on each portfolio
        #dat.m$J.G.1.m <- J.G.0
        for (z in names(J.G.0)){
            dat.m$J.G.1.m[[z]] <- J.G.0[[z]]
        }

        profits.out <- compute.market.profits(dat.m, num.param = num.param,
                                              opts = opts, Rhos = Rhos,
                                              keep.platforms = keep.platforms,
                                              return.Rho = TRUE)
        profits <- profits.out$Profits

        # Exit if the J.m counts are fixed
        if (!is.null(J.m)){
            J.G.1 <- J.m
            break
        }

        # Compute responses to profits
        G.shares <- compute.market.G.shares(profits, kappa, Mu.f = Mu.f)
        zips.1 <- intersect(names(nres.m), names(G.shares))
        zips.1 <- intersect(zips.1, zips.pos)
        J.G.1 <- lapply(zips.1, function(zip) nres.m[zip]*G.shares[[zip]])
        names(J.G.1) <- zips.1

        # Compare these choices to the initial J.G list
        cmp.zips <- intersect(zips.pos, intersect(names(J.G.0), names(J.G.1)))
        dist <- sapply(cmp.zips, function(zip) mean(abs(J.G.1[[zip]] - J.G.0[[zip]])/nres.m[zip]))
        dist <- mean(dist)
        if (verbose){
            pracma::fprintf('Dist (iter %d) = %f\n', iter, dist)
        }
        # Assess convergence
        if (dist < tol.FP){
            ## Break out of the loop if convergence has occurred
            break
        } else {
            ## Otherwise, change the baseline J.G.0 list
            J.G.0 <- lapply(names(J.G.1), function(z) learn.rate*J.G.1[[z]] + (1 - learn.rate)*J.G.0[[z]])
            names(J.G.0) <- names(J.G.1)
        }
    }

    if (!isTRUE(dist < tol.FP)){
        warning(sprintf(paste0('find.fixed.point did not converge: dist = %g ',
                               'after %d iterations (tol.FP = %g); proceeding ',
                               'with the final iterate.'),
                        dist, max.iter.FP, tol.FP))
    }

    # Update dat.m
    G.vars <- grep('^G[01]', colnames(dat.m$resto.m), value = TRUE)
    for (z in names(J.G.1)){
        names(J.G.1[[z]]) <- G.vars
    }
    dat.m$J.G.1.m <- J.G.1

    if (more.outputs){
        # Restaurant price responses
        if (is.null(Rhos)){
            Rhos <- profits.out$Rhos
        }

        ## Sales
        sales.dat <- compute.zip.sales(dat.m, Rhos = Rhos,
                                       opts = opts,
                                       more.outputs = TRUE, by.demo = by.demo)
        Sales.mats     <- sales.dat$Sales.mats
        Sales.tots     <- sales.dat$Sales.tots
        Sales.platform <- sales.dat$Sales.platform
        Sales.np.tot   <- sales.dat$Sales.np.tot
        EU             <- sales.dat$EU
        EU.sum         <- sales.dat$EU.sum
        Sales.tots.c   <- sales.dat$Sales.tots.c
        Sales.tots.i   <- sales.dat$Sales.tots.i

        out <- list(J.G.1          = J.G.1,
                    profits        = profits,
                    Rhos           = Rhos,
                    Sales.mats     = Sales.mats,
                    Sales.tots     = Sales.tots,
                    Sales.platform = Sales.platform,
                    Sales.np.tot   = Sales.np.tot,
                    EU             = EU,
                    EU.sum         = EU.sum,
                    p.c            = dat.m$fees)
        if (by.demo){
            out$by.demo <- sales.dat$by.demo
        }
    } else {
        out <- J.G.1
    }

    return(out)
}
