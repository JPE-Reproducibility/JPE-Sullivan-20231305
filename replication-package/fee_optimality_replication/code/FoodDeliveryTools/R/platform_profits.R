platform.profits <- function(FP, C, R, kappa, dat.m, pMC.df,
                             opts, num.param,
                             more.outputs = FALSE,
                             by.demo = FALSE,
                             keep.platforms = NULL){
    # Compute each food delivery platform's profits under the
    # portfolio choice model with geography.
    # Use the alternative timing assumption on price setting.
    #
    #
    # Inputs
    #   p.r: Commissions charged to restaurants
    #   dat.m: Data for a particular market
    #   resto.param: Restaurant variable profits parameters
    #   kappa: Restaurant fixed cost parameters
    #   pMC.df: The platform's marginal costs organized as a data.frame
    #   return.FP: return the find.fixed.point() object?

    if ('tax.rate' %in% names(opts)){
        tax.rate <- opts$tax.rate
    } else {
        tax.rate <- 0.0
    }

    # Preliminaries
    nG <- dat.m$nportfolios
    nF <- dat.m$nplatforms

    if (is.null(keep.platforms)){
        keep.platforms <- rep(1, times = nF - 1)
    }
    active.platforms <- which(keep.platforms == 1)
    keep.G <- determine.portfolios(dat.m$G.mat, keep.platforms)

    # Load in fees
    for (z in names(dat.m$fees)){
        dat.m$fees[[z]] <- C
    }
    for (z in names(dat.m$comm)){
        dat.m$comm[[z]] <- R
    }

    # Compute restaurant profits
    r.profit.c <- 0
    r.profit.i <- 0
    r.profit.tot <- 0
    r.profit.f <- rep(0, times = dat.m$nplatforms - 1)
    r.profit.uber <- 0

    zips <- names(FP$profits)

    K <- kappa$K
    for (z in zips){
        # Total profits
        J.G.z <- FP$J.G.1[[z]][keep.G]
        pi.z  <- FP$profits[[z]][keep.G]
        ## Adjust for fixed costs
        pi.z <- pi.z - K[[z]][keep.G]
        tot.pi.z <- sum(pi.z*J.G.z)
        r.profit.tot <- r.profit.tot + tot.pi.z

        # Profits by restaurant type
        if (grepl('c$', z)){
            r.profit.c <- r.profit.c + tot.pi.z
        } else if (grepl('i$', z)){
            r.profit.i <- r.profit.i + tot.pi.z
        }

        # Profits by platform membership
        for (f in active.platforms){
            idx.f <- dat.m$G.mat[, f + 1] == 1 &
                     (1:nrow(dat.m$G.mat) %in% keep.G)
            J.G.z <- FP$J.G.1[[z]][idx.f]
            pi.z  <- FP$profits[[z]][idx.f]
            ## Adjust for fixed costs
            pi.z  <- pi.z - K[[z]][idx.f]
            tot.pi.z <- sum(pi.z*J.G.z)
            r.profit.f[f] <- r.profit.f[f] + tot.pi.z
        }
        ## On both Uber and Postmates
        if (all(c(2, 4) %in% active.platforms)){
            idx.f <- sort(unique(which(dat.m$G.mat[, 3] == 1 |
                                           dat.m$G.mat[, 5] == 1)))
            idx.f <- intersect(idx.f, keep.G)
            J.G.z <- FP$J.G.1[[z]][idx.f]
            pi.z  <- FP$profits[[z]][idx.f]
            ## Adjust for fixed costs
            pi.z <- pi.z - K[[z]][idx.f]
            tot.pi.z <- sum(pi.z*J.G.z)
            r.profit.uber <- r.profit.uber + tot.pi.z
        }
    }

    Rhos       <- FP$Rhos
    Sales.tots <- FP$Sales.tots

    profits <- list()
    Rho.avg <- list()
    S.r     <- list() # sum_z \bar rho_{fz} s_{fz}
    if (more.outputs){
        revenues <- list()
        Tax      <- list()
    }

    # Columns of marginal costs in the pMC.df data.frame
    pMC.cols <- paste0('mc', 1:(nF - 1))

    # Loop over ZIPs in which consumers order food
    zips <- names(Sales.tots)

    Cp <- c(offline = 0, C)
    Rp <- c(offline = 0, R)
    for (z0 in zips){
        # Extract platform marginal costs
        idx.z <- which(pMC.df$zip == z0)
        if (length(idx.z) != 1){
            next
        }

        mc.pl.z <- as.numeric(pMC.df[idx.z, pMC.cols])
        names(mc.pl.z) <- c('dd', 'uber', 'gh', 'pm')
        mc.pl.z <- c(offline = 0, mc.pl.z)

        Z.z <- dimnames(Sales.tots[[z0]])[[3]]
        nZ <- length(Z.z)

        # Initialize outputs
        Markups  <- list()
        Rho.Mats <- list()
        Profits  <- list()
        if (more.outputs){
            Tax.per.sales <- list()
            Revenues      <- list()
        }

        for (z.z in Z.z){
            markups <- matrix(0, nrow = nG, ncol = nF)
            ## For computing sales-weighted rho averages
            Rho.mat <- matrix(0, nrow = nG, ncol = nF)
            if (more.outputs){
                tax.per.sale <- matrix(0, nrow = nG, ncol = nF)
                revenues.mat <- matrix(0, nrow = nG, ncol = nF)
            }

            for (g in keep.G){
                idx.g  <- dat.m$G.indices[[g]]
                Rho.zg <- as.numeric(Rhos[[z.z]][[g]])
                rev.g <- Cp[idx.g] + Rp[idx.g]*Rho.zg*(1 - tax.rate)

                markups[g, idx.g] <- rev.g - mc.pl.z[idx.g]
                Rho.mat[g, idx.g] <- Rho.zg

                if (more.outputs){
                    tax.per.sale[g, idx.g] <- Rp[idx.g]*Rho.zg*tax.rate
                    revenues.mat[g, idx.g] <- rev.g
                }
            }
            Markups[[z.z]]  <- markups
            Rho.Mats[[z.z]] <- Rho.mat
            if (more.outputs){
                Tax.per.sales[[z.z]] <- tax.per.sale
                Revenues[[z.z]]      <- revenues.mat
            }
            # Integrate across platform subsets
            Profits[[z.z]] <- colSums(markups*Sales.tots[[z0]][, , z.z])
        }

        # Integrate across ZIPs
        profits[[z0]] <- Reduce('+', Profits)

        ## Compute sales-weighted average Rhos
        sales.sum <- apply(Sales.tots[[z0]], MARGIN = 2, FUN = sum)
        sales.Rho.sum <- list()
        for (z.z in names(Rho.Mats)){
            sales.Rho.sum[[z.z]] <- Rho.Mats[[z.z]]*Sales.tots[[z0]][, , z.z]
        }
        Rho.avg[[z0]]  <- colSums(Reduce('+', sales.Rho.sum))/sales.sum
        Rho.avg[[z0]][is.nan(Rho.avg[[z0]])] <- 0

        S.r[[z0]] <- Rho.avg[[z0]]*sales.sum*(1 - tax.rate)
        if (more.outputs){
            ## Compare platform revenues and taxes
            rev.tot <- list()
            tax.tot <- list()
            for (z.z in names(Revenues)){
                rev.tot[[z.z]] <- Revenues[[z.z]]*Sales.tots[[z0]][, , z.z]
                tax.tot[[z.z]] <- Tax.per.sales[[z.z]]*Sales.tots[[z0]][, , z.z]
            }
            rev.tot <- Reduce('+', rev.tot)
            revenues[[z0]] <- colSums(rev.tot)

            tax.tot <- Reduce('+', tax.tot)
            Tax[[z0]] <- colSums(tax.tot)
        }
    }

    # Integrate across zip codes
    if (length(profits) > 1){
        pl.profits <- colSums(Reduce(rbind, profits))
        if (more.outputs){
            pl.revenues <- colSums(Reduce(rbind, revenues))
        }
    } else {
        pl.profits  <- profits[[1]]
        if (more.outputs){
            pl.revenues <- revenues[[1]]
        }
    }
    pl.profits <- pl.profits[2:dat.m$nplatforms]
    if (more.outputs){
        Tax.tot <- sum(Reduce('+', Tax))
        pl.revenues <- pl.revenues[2:dat.m$nplatforms]
    }

    S.r <- Reduce('+', S.r)
    S.r <- S.r[2:length(S.r)]

    if (more.outputs){
        output <- list(pl.profits    = pl.profits,
                       pl.revenues   = pl.revenues,
                       pl.sales      = FP$Sales.platform,
                       S.r           = S.r,
                       Rho.avg       = Rho.avg,
                       Tax.tot       = Tax.tot,
                       p.c           = FP$p.c,
                       J.G           = FP$J.G.1,
                       profits       = FP$profits,
                       r.profit.c    = r.profit.c,
                       r.profit.i    = r.profit.i,
                       r.profit.tot  = r.profit.tot,
                       r.profit.f    = r.profit.f,
                       r.profit.uber = r.profit.uber)
        if (by.demo){
            output$by.demo <- FP$by.demo
        }
    } else {
        output <- pl.profits
    }

    return(output)
}
