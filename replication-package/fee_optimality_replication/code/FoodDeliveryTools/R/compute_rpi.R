
compute.rpi <- function(dat.m, C, R, J.G, kappa, Rhos, opts, keep.platforms = NULL){
    # Load in fees and restaurant counts
    if (is.null(keep.platforms)){
        keep.platforms <- rep(1, times = dat.m$nplatforms - 1)
    }
    for (z in names(dat.m$fees)){
        dat.m$fees[[z]] <- C
    }
    for (z in names(dat.m$comm)){
        dat.m$comm[[z]] <- R
    }
    for (z in names(J.G)){
        dat.m$J.G.1.m[[z]] <- J.G[[z]]
    }
    # Extract fixed costs
    K <- kappa$K
    # Compute profits
    profits <- compute.market.profits(dat.m, num.param, opts, Rhos = Rhos,
                                      keep.platforms = keep.platforms)
    zips0 <- names(dat.m$fees)
    G.mat <- dat.m$G.mat
    profit.tot <- profits.by.zip(profits, J.G, K, zips0,
                                 keep.platforms, G.mat)
    rpi <- c(chain = profit.tot$pi.Jc.sum,
             indep = profit.tot$pi.Ji.sum)
    rpi['total'] <- rpi['chain'] + rpi['indep']

    # Now, a version with RC profits
    rc.names <- c(chain = 'chain_RC', indep = 'indep_RC', all = 'total_RC')
    pi.RC <- compute.RC.profits(profits, J.G, dat.m, kappa)
    rpi[rc.names] <- pi.RC[names(rc.names)]

    # Now, a version with logit profits
    rpi['total_logit'] <- compute.logit.profits(profits, J.G, dat.m, kappa)

    return(rpi)
}
