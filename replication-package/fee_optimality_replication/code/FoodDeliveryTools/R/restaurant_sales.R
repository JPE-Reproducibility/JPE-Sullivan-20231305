restaurant.sales <- function(dat.m, Rhos, sales.zips = NULL,
                             loop.zips = NULL, opts = NULL,
                             scale.down = FALSE,
                             outside = FALSE,
                             keep.platforms = NULL){
    # Compute the sales and sales derivatives of a restaurant in
    # each zip code.
    #
    # Inputs
    #   dat.m: Data
    #   Rhos: Menu prices
    #   sales.zips: Subset of ZCTAs for which to compute sales.
    #       These are the ZCTAs for which we compute restaurants' sales
    #   loop.zips:  zips within a radius of each zip in "sales.zips" within
    #       which we need to compute sales
    #       (the function will compute this vector if it is not provided)
    #   outside: Compute substitution to outside restaurant?

    # Extract data objects
    G.mat        <- dat.m$G.mat
    G.indices    <- dat.m$G.indices
    demand.param <- dat.m$demand.param

    # Produce a list of ZIPs for which to compute CCPs
    CCP.dat  <- list()
    zips.m   <- names(dat.m$Jp.G.1.m)
    zips.pos <- sapply(zips.m, function(z) sum(dat.m$Jp.G.1.m[[z]]) > 0)
    all.zips <- zips.m[zips.pos]

    # Determine sales.zips and loop.zips
    if (is.null(sales.zips)){
        sales.zips <- all.zips
        loop.zips  <- all.zips
    } else if (is.null(loop.zips)){
        loop.zips <- lapply(sales.zips, function(z) dat.m$zip.map.1[[z]])
        loop.zips <- Reduce(c, loop.zips)
    }
    sales.zips <- intersect(sales.zips, all.zips)
    loop.zips  <- intersect(loop.zips,  all.zips)

    # Generate option values if not provided
    if (is.null(opts)){
        opts <- load.opts()
    }

    #== Block A: determine active and inactive platforms ==#
    NF <- dat.m$nplatforms - 1
    if (is.null(keep.platforms)){
        keep.platforms <- rep(1, times = NF)
    }
    active.platforms   <- which(keep.platforms == 1)
    inactive.platforms <- which(keep.platforms == 0)


    #== Block B: compute the sales & derivatives for each ZIP ==#
    ## Sales within each zip code by each restaurant in range
    Sales.mats <- compute.zip.sales(dat.m, Rhos = Rhos, more.outputs = FALSE,
                                    opts = opts,
                                    loop.zips = loop.zips,
                                    scale.down = scale.down,
                                    keep.platform = keep.platforms)

    Sales.derivs <- compute.zip.derivatives(dat.m, Rhos = Rhos, level = TRUE,
                                            outside = outside,
                                            opts = opts,
                                            loop.zips = loop.zips,
                                            keep.platforms = keep.platforms)

    if (outside){
        Outside.derivs <- Sales.derivs$S.out
        Sales.derivs <- Sales.derivs$S.derivs
    }

    #== Block C: compute restaurants' sales ==#
    # For each ZIP, sum up over ZIPs in range
    Sales <- list()
    Deriv <- list()
    if (outside){
        Outside <- list()
    }

    # Loop over ZIPs in which restaurants are located
    for (zip in sales.zips){

        if (is.null(dat.m$J.G.1.m[[zip]])){
            next
        }
        if (sum(dat.m$J.G.1.m[[zip]]) == 0){
            next
        }

        ## Initialize outputs
        Sales[[zip]] <- list()
        Deriv[[zip]] <- list()
        if (outside){
            Outside[[zip]] <- list()
        }

        # Loop over ZIPs in which the restaurant makes sales
        Z.z  <- dat.m$zip.map.1[[zip]]
        Z.z0 <- unique(sub('[ci]$', '', Z.z))
        for (z.z0 in Z.z0){
            init.zip <- (length(Sales[[zip]]) == 0)

            # Sales
            sales.in.z.z0  <- Sales.mats[[z.z0]][, , zip]
            derivs.in.z.z0 <- Sales.derivs[[z.z0]][[zip]]
            if (init.zip){
                Sales[[zip]] <- sales.in.z.z0
                Deriv[[zip]] <- derivs.in.z.z0
                if (outside){
                    Outside[[zip]] <- Outside.derivs[[z.z0]][[zip]]
                }
            } else {
                Sales[[zip]] <- Sales[[zip]] + sales.in.z.z0
            }

            # Derivatives
            if (!init.zip){
                for (g in 1:dat.m$nportfolios){
                    Deriv[[zip]][[g]] <- Deriv[[zip]][[g]] + Sales.derivs[[z.z0]][[zip]][[g]]
                    if (outside){
                        Outside[[zip]][[g]] <- Outside[[zip]][[g]] + Outside.derivs[[z.z0]][[zip]][[g]]
                    }
                }
            }
        }
    }
    # Package and return outputs
    outputs <- list(Sales = Sales, Deriv = Deriv)
    if (outside){
        outputs$Outside <- Outside
    }
    return(outputs)
}

