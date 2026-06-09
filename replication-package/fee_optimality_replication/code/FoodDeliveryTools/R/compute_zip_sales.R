compute.zip.sales <- function(dat.m, more.outputs = FALSE, Rhos = NULL,
                              selected.zip = NULL, opts = NULL,
                              loop.zips = NULL, scale.down = FALSE,
                              by.demo = FALSE, keep.platforms = NULL){
    # Compute the sales matrix for each zip code in a market
    # The sales matrix tells us, for a given zip code, the sales of a
    # restaurant in range of the zip code on each portfolio set.
    # It does *not* provide the sales of a restaurant in each zip code
    #
    # Inputs
    #     dat.m$fees: list with the prices in each zip code
    #     dat.m$J.G.m: restaurant counts
    #     more.outputs: Provide S.tot matrices?
    #     Rhos: adjust utilities for menu prices?
    #     selected.zip: Compute only sales for zip codes within range of selected.zip?
    #     loop.zips: ZCTAs for which to compute sales
    #     scale.down: When there is less than one restaurant in a platform portfolio,
    #       report the sales for the fraction of a restaurant in the platform portfolio
    #       rather than a full restaurant in the platform portfolio
    #     by.demo: report sales by demographics?

    if (is.null(opts)){
        opts <- load.opts()
    }

    if (is.null(keep.platforms)){
        keep.platforms <- rep(1, times = dat.m$nplatforms - 1)
    }

    # Use restaurant heterogeneity?
    if (is.null(loop.zips)){
        if (!is.null(selected.zip)){
            rest.het <- grepl('[ci]$', selected.zip)
        } else {
            rest.het <- TRUE
        }
    } else {
        rest.het <- grepl('[ci]$', loop.zips[1])
    }

    # Extract data
    buy.zip      <- dat.m$buy.zip
    demand.param <- dat.m$demand.param
    G.mat        <- dat.m$G.mat

    fees <- dat.m$fees
    if (rest.het){
        J.G.m <- dat.m$J.G.1.m
    } else {
        J.G.m <- dat.m$J.G.m
    }
    zips.m <- names(J.G.m)

    incl.np <- ('mu.eta' %in% names(demand.param))

    # Initialize output
    Sales.mats <- list()
    if (more.outputs){
        S.mats     <- list()
        S.tots     <- list()
        Sales.tots <- list()
        EU <- list()

        if (by.demo){
            S.tot.young     <- list()
            S.tot.unmarried <- list()
            S.tot.lowinc    <- list()

            Sales.tot.y <- list()
            Sales.tot.u <- list()
            Sales.tot.l <- list()

            EU.y <- list()
            EU.u <- list()
            EU.l <- list()
        }

        if (incl.np){
            Sales.np <- list()

            if (by.demo){
                Sales.np.y <- list()
                Sales.np.u <- list()
                Sales.np.l <- list()
            }
        }
    }

    # Fill in output by looping over zipcodes
    if (is.null(loop.zips)){
        if (!is.null(selected.zip)){
            loop.zips <- dat.m$zip.map.1[[selected.zip]]
        } else {
            loop.zips <- zips.m
        }
    }

    # These objects will be used in the loop
    G.indices  <- dat.m$G.indices
    fg.indices <- dat.m$fg.indices
    T.i        <- dat.m$T.i

    # Loop over ZIPs, computing sales in each
    for (z in loop.zips){
            if (rest.het){
                z0 <- substr(z, 1, 5)
            } else {
                z0 <- z
            }
            if (z0 %in% names(Sales.mats)){
                # Do not compute demand in each ZIP twice
                next
            }

            # Extract data for ZIP z
            buy.z <- buy.zip[[z0]]
            if (is.null(buy.z)){
                #pracma::fprintf("Skipped for missing buy data = 0: %s\n", z0)
                next
            }
            if (nrow(buy.z) == 0){
                #pracma::fprintf("Skipped for nbuy = 0: %s\n", z0)
                next
            }
            p.c <- fees[[z0]]

            ## Subset to ZIPs within range of the ZIP in question
            Z.z <- intersect(dat.m$zip.map.1[[z]], names(J.G.m))
            J.G <- lapply(Z.z, function(z.z) J.G.m[[z.z]])
            names(J.G) <- Z.z
            nresto <- sum(Reduce(c, J.G))

            if (is.null(p.c)){
                #pracma::fprintf("Skipped for missing fee data: %s\n", z0)
                next
            }

            if (nresto == 0){
                #pracma::fprintf("Skipped for nresto = 0: %s\n", z0)
                next
            }

            # If the ZIP has positive sales and non-missing prices, compute its sales
            sales.dat <- compute.S.mat.v2(J.G, G.mat, p.c, demand.param, buy.z,
                                          more.outputs = more.outputs,
                                          Rhos       = Rhos,
                                          G.indices  = G.indices,
                                          fg.indices = fg.indices,
                                          scale.down = scale.down,
                                          T.i        = T.i,
                                          by.demo    = by.demo,
                                          keep.platforms = keep.platforms)

            if (more.outputs){
                S.mats[[z0]]     <- sales.dat$S.mat
                S.tots[[z0]]     <- sales.dat$S.tot
                Sales.mats[[z0]] <- sales.dat$Sales.mat
                Sales.tots[[z0]] <- sales.dat$Sales.tot
                EU[[z0]]         <- sales.dat$EU

                if (incl.np){
                    Sales.np[[z0]] <- sales.dat$Sales.np
                }

                if (by.demo){
                    demo.out <- sales.dat$by.demo
                    Sales.tot.y[[z0]] <- demo.out$Sales.tot.y
                    Sales.tot.u[[z0]] <- demo.out$Sales.tot.u
                    Sales.tot.l[[z0]] <- demo.out$Sales.tot.l

                    EU.y[[z0]] <- demo.out$EU.y
                    EU.u[[z0]] <- demo.out$EU.u
                    EU.l[[z0]] <- demo.out$EU.l

                    if (incl.np){
                        Sales.np.y[[z0]] <- demo.out$Sales.np.y
                        Sales.np.u[[z0]] <- demo.out$Sales.np.u
                        Sales.np.l[[z0]] <- demo.out$Sales.np.l
                    }
                }

            } else {
                Sales.mats[[z0]] <- sales.dat
            }
        }


    if (more.outputs){
        # Platform-specific sales
        Sales.tot.m <- lapply(Sales.tots, function(x) apply(x, c(1, 2), sum))

        Sales.tot.m <- Reduce('+', Sales.tot.m)

        Sales.platform <- colSums(Sales.tot.m)

        # By restaurant type
        if (rest.het){
            Sales.tot.c <- list()
            Sales.tot.i <- list()
            for (z0 in names(Sales.tots)){
                S0 <- Sales.tots[[z0]]
                Z.z <- dimnames(S0)[[3]]
                Z.z.c <- grep('c$', Z.z, value = TRUE)
                Z.z.i <- grep('i$', Z.z, value = TRUE)
                Sales.tot.c[[z0]] <- apply(S0[, , Z.z.c], 2, sum)
                Sales.tot.i[[z0]] <- apply(S0[, , Z.z.i], 2, sum)
            }
            Sales.tot.c <- Reduce('+', Sales.tot.c)
            Sales.tot.i <- Reduce('+', Sales.tot.i)
        }

        # Total welfare
        EU.sum <- Reduce('+', EU)

        # Collate outputs
        out <- list(S.mats = S.mats, S.tots = S.tots,
                    Sales.mats = Sales.mats, Sales.tots = Sales.tots,
                    Sales.platform = Sales.platform, Sales.tot.m = Sales.tot.m,
                    EU = EU, EU.sum = EU.sum)
        if (rest.het){
            out$Sales.tot.c <- Sales.tot.c
            out$Sales.tot.i <- Sales.tot.i
        }

        if (incl.np){
            out$Sales.np <- Sales.np
            out$Sales.np.tot <- Reduce('+', Sales.np)
        }

        if (by.demo){
            Sales.tot.m.y <- lapply(Sales.tot.y, function(x) apply(x, c(1, 2), sum))
            Sales.tot.m.y <- Reduce('+', Sales.tot.m.y)
            Sales.platform.y <- colSums(Sales.tot.m.y)

            Sales.tot.m.u <- lapply(Sales.tot.u, function(x) apply(x, c(1, 2), sum))
            Sales.tot.m.u <- Reduce('+', Sales.tot.m.u)
            Sales.platform.u <- colSums(Sales.tot.m.u)

            Sales.tot.m.l <- lapply(Sales.tot.l, function(x) apply(x, c(1, 2), sum))
            Sales.tot.m.l <- Reduce('+', Sales.tot.m.l)
            Sales.platform.l <- colSums(Sales.tot.m.l)

            demo.out <- list()
            demo.out$Sales.tot.m.y <- Sales.tot.m.y
            demo.out$Sales.tot.m.u <- Sales.tot.m.u
            demo.out$Sales.tot.m.l <- Sales.tot.m.l

            demo.out$Sales.platform.y <- Sales.platform.y
            demo.out$Sales.platform.u <- Sales.platform.u
            demo.out$Sales.platform.l <- Sales.platform.l

            demo.out$EU.sum.y <- Reduce('+', EU.y)
            demo.out$EU.sum.u <- Reduce('+', EU.u)
            demo.out$EU.sum.l <- Reduce('+', EU.l)

            if (incl.np){
                Sales.np.tot.y <- Reduce('+', Sales.np.y)
                Sales.np.tot.u <- Reduce('+', Sales.np.u)
                Sales.np.tot.l <- Reduce('+', Sales.np.l)

                # Add to the list
                demo.out$Sales.np.tot.y <- Sales.np.tot.y
                demo.out$Sales.np.tot.u <- Sales.np.tot.u
                demo.out$Sales.np.tot.l <- Sales.np.tot.l
            }

            out$by.demo <- demo.out
        }
    } else {
        out <- Sales.mats
    }
    return(out)
}
