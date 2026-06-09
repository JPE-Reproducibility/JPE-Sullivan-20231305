compute.zip.derivatives <- function(dat.m, Rhos = NULL, level = TRUE,
                                    loop.zips = NULL,
                                    outside = FALSE, opts = NULL,
                                    keep.platforms = NULL){
    # Compute price derivatives matrices for each zip code in a market
    # Each matrix tells us, for a particular zip code, the derivatives
    # of the platform-specific sales of a restaurant in range of the zip code
    # on each portfolio set with respect to the restuarant's menu prices
    # on the platforms in this portfolio set.
    #
    # Inputs
    #     dat.m$fees: list with platforms' consumer fees in each zip code
    #     Rhos: adjust utilities for menu prices?
    #     level:  compute derivatives of sales **levels** instead of sales **shares**?
    #     outside: compute derivative of no-purchase option?

    if (is.null(opts)){
        opts <- load.opts()
    }

    # Use restaurant heterogeneity?
    if (is.null(loop.zips)){
        rest.het <- TRUE
    } else {
        rest.het <- grepl('[ci]$', loop.zips[1])
    }

    buy.zip      <- dat.m$buy.zip
    demand.param <- dat.m$demand.param
    G.mat        <- dat.m$G.mat

    # Determine zipcodes in market
    if (rest.het){
        zips.m <- rownames(dat.m$zip.mat.1.m)
    } else {
        zips.m <- rownames(dat.m$zip.mat.m)
    }

    # Initialize output
    S.derivs <- list()
    if (outside){
        S.out <- list()
    }

    if (is.null(loop.zips)){
        loop.zips <- zips.m
    }

    # Adjust menu prices if specified by options
    Rhos <- set.exclusive.Rhos(Rhos, opts, G.mat)

    # These objects will be used in the loop
    G.indices  <- dat.m$G.indices
    fg.indices <- dat.m$fg.indices
    T.i        <- dat.m$T.i

    # Fill in output by looping over ZIPs
    for (z in loop.zips){
        if (rest.het){
            z0 <- substr(z, 1, 5)
        } else {
            z0 <- z
        }

        if (z0 %in% names(S.derivs)){
            # Do not compute derivatives in each ZIP twice
            next
        }

        # Extract data for zipcode z
        buy.z <- buy.zip[[z0]]
        if (is.null(buy.z)){
            next
        }
        if (nrow(buy.z) == 0){
            next
        }
        p.c   <- dat.m$fees[[z0]]
        J.G <- dat.m$J.G.1.m

        ## Subset to ZIPs within range of the ZIP in question
        Z.z <- intersect(dat.m$zip.map.1[[z]], names(J.G))
        J.G <- lapply(Z.z, function(z.z) J.G[[z.z]])
        names(J.G) <- Z.z
        nresto <- sum(Reduce(c, J.G))

        if (is.null(p.c) | nresto == 0){
            next
        }

        # If the ZIP has positive sales and non-missing prices, compute its derivatives
        if ((nrow(buy.z)) > 0 & (!is.null(p.c)) & (nresto > 0)){
            S.derivs[[z0]] <- compute.S.derivatives.v2(J.G, G.mat, p.c,
                                                       demand.param, buy.z,
                                                       Rhos       = Rhos,
                                                       level      = level,
                                                       G.indices  = G.indices,
                                                       fg.indices = fg.indices,
                                                       T.i        = T.i,
                                                       keep.platforms = keep.platforms)

            if (outside){
                S.out[[z0]] <- compute.outside.diversion(J.G, G.mat, p.c,
                                                         demand.param, buy.z,
                                                         G.indices = G.indices,
                                                         fg.indices = fg.indices,
                                                         Rhos = Rhos, level = level,
                                                         T.i = T.i,
                                                         keep.platforms = keep.platforms)
            }
        }
    }

    if (!outside){
        output <- S.derivs
    } else {
        output <- list(S.derivs = S.derivs, S.out = S.out)
    }

    return(output)
}
