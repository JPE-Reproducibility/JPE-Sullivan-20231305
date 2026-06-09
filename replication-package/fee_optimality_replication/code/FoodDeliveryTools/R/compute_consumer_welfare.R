compute.consumer.welfare <- function(dat.m, C, J.G, Rhos, demand.param = NULL,
                                     no.logit = TRUE, keep.platforms = NULL){
    # Compute measures of consumer welfare under fees p.c,
    # restaurant location decisions J.G, and prices Rhos

    NF <- dat.m$nplatforms - 1
    if (is.null(keep.platforms)){
        keep.platforms <- rep(1, times = NF)
    }
    keep.G <- determine.portfolios(dat.m$G.mat, keep.platforms)

    # Extract data objects
    G.mat        <- dat.m$G.mat
    G.indices    <- dat.m$G.indices
    if (is.null(demand.param)){
        demand.param <- dat.m$demand.param
    }

    # Initialize outputs
    EU <- list()
    EU.dollarized <- list()
    EU.dollarized.sum <- c()

    # Determine price sensitivity specification
    het.extra <- ('alpha.young' %in% names(demand.param))
    alpha <- demand.param$alpha

    zips0 <- names(dat.m$buy.zip)
    for (z0 in zips0){
        # Extract data for the zip
        buy.z   <- dat.m$buy.zip[[z0]]
        weights <- buy.z$count

        if (nrow(buy.z) == 0){
            next
        }

        z.c <- paste0(z0, 'c')
        z.i <- paste0(z0, 'i')
        nearby.zips <- unique(c(dat.m$zip.map.1[[z.c]], dat.m$zip.map.1[[z.i]]))
        Z.z <- intersect(nearby.zips, names(J.G))
        J.G.z <- lapply(Z.z, function(z.z) J.G[[z.z]])
        names(J.G.z) <- Z.z
        nresto <- sum(Reduce(c, J.G.z))

        if (nresto == 0){
            next
        }

        if (het.extra){
            a.y <- demand.param$alpha.young*buy.z$young
            a.m <- demand.param$alpha.married*buy.z$married
            a.h <- demand.param$alpha.highinc*buy.z$high_income
            alpha.i <- alpha + a.y + a.m + a.h
        } else {
            alpha.i <- rep(alpha, times = nrow(buy.z))
        }

        EU[[z0]] <- compute.expected.utility.v2(p.c.z = C,
                                                J.G.z = J.G.z,
                                                G.mat = G.mat,
                                                buy.z = buy.z,
                                                Rhos  = Rhos,
                                                demand.param = demand.param,
                                                G.indices = G.indices,
                                                use.expected.delta = no.logit)
        ## Dollarized
        EU.dollarized[[z0]]   <- EU[[z0]]/alpha.i*dat.m$T.i
        EU.dollarized.sum[z0] <- sum(EU.dollarized[[z0]]*weights)
    }

    ## Total expected utility in dollar terms
    total.EU.dollar <- sum(EU.dollarized.sum)

    out <- list(EU              = EU,
                EU.dollar       = EU.dollarized,
                EU.dollar.sum   = EU.dollarized.sum,
                total.EU.dollar = total.EU.dollar)
    return(out)
}
