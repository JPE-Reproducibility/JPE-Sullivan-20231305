compute.S.mat.v2 <- function(J.G, G.mat, p.c, demand.param, buy.z,
                             G.indices, fg.indices,
                             more.outputs = FALSE, Rhos = NULL,
                             scale.down = FALSE,
                             T.i = 17, by.demo = FALSE,
                             keep.platforms = NULL){
    # Compute the sales received by a restaurant in range of a zip code with
    # distribution `J.G` of nearby restaurants across portfolios
    #
    # Inputs
    #   J.G: restaurant counts (among nearby ZIPs only)
    #
    # Outputs
    #   S.mat: this matrix gives the sales that a restaurant in a particular
    #       ZIP receives on a particular platform
    #       when it is a member of a particular platform portfolio
    #   S.tot: this matrix gives the total sales accounted for by each pair
    #       of a (i) restaurant type, where type is defined by portfolio,
    #       (ii) platform, and (iii) ZIP
    #   T.i: Number of potential transactions
    #   by.demo: Compute welfare and sales by demographic group?

    if (is.null(Rhos)){
        stop('compute.S.mat.v2 now requires prices Rhos as input')
    }

    if (is.null(keep.platforms)){
        NF <- ncol(G.mat) - 1
        keep.platforms <- rep(1, times = NF)
    }
    keep.G <- determine.portfolios(G.mat, keep.platforms)
    inactive.platforms <- which(keep.platforms == 0)

    # Preliminaries
    nportfolios <- length(J.G[[1]])
    nZ          <- length(J.G)
    nplatforms  <- ncol(G.mat)
    nbuy        <- nrow(buy.z)
    weights     <- buy.z$count
    sweight     <- sum(weights)

    Z.z <- names(J.G)

    # Include the option to make no purchase?
    incl.np <- ('mu.eta' %in% names(demand.param))

    het.extra <- ('alpha.young' %in% names(demand.param))

    alpha <- demand.param$alpha
    if (het.extra){
        a.y <- demand.param$alpha.young*buy.z$young
        a.m <- demand.param$alpha.married*buy.z$married
        a.h <- demand.param$alpha.highinc*buy.z$high_income
        alpha.i <- alpha + a.y + a.m + a.h
    } else {
        alpha.i <- rep(alpha, times = nrow(buy.z))
    }

    alpha.vals <- unique(alpha.i)
    nvals      <- length(alpha.vals)

    IDX.alpha <- list()
    for (k in 1:nvals){
        alpha.k <- alpha.vals[k]
        IDX.alpha[[k]] <- which(alpha.i == alpha.k)
    }

    # Smoothing parameter
    sigma.eps <- load.sigma.eps()

    #== Generate objects used in computing sales ==#
    deltas <- compute.deltas(p.c, demand.param, buy.z, incl.0 = TRUE)
    if (length(inactive.platforms) > 0){
        # set very low deltas to dropped platforms to ensure sales of zero
        for (k in inactive.platforms){
            deltas[, k + 1] <- -100
        }
    }

    # Compute an inclusive value for each nZIP (nearby ZIP)
    V.G  <- list()
    eV.0 <- list()
    for (z.z in Z.z){
        Rhos.z <- Rhos[[z.z]]
        is.chain <- chain.stat(z.z)
        V.G[[z.z]] <- compute.inclusive.values(deltas, demand.param,
                                               G.mat, G.indices,
                                               Rhos.z = Rhos.z, buy.z = buy.z,
                                               is.chain = is.chain)
        eV.0[[z.z]] <- exp(V.G[[z.z]]/demand.param$gamma)
    }

    # Determine intra-portfolio platform choice probabilities
    Mus <- list()
    large.number <- 1e200

    ## Set separate Mus for each ZIP
    for (g in 1:nportfolios){
        idx.g <- G.indices[[g]]
        g.size <- length(idx.g)
        Mus[[g]] <- list()

        if (!(g %in% keep.G)){
            # If the portfolio is irrelevant, assign all intraportfolio sales
            # to the first-party ordering channel
            for (z.z in Z.z){
                Mus[[g]][[z.z]] <- matrix(0, nrow = nrow(deltas), ncol = g.size)
                Mus[[g]][[z.z]][, 1] <- 1
            }
        } else if (g.size > 1){
            exp.delta.g <- exp(deltas[, idx.g]/sigma.eps)
            exp.delta.g[is.infinite(exp.delta.g)]    <- large.number
            exp.delta.g[exp.delta.g >= large.number] <- large.number

            # Adjust for rhos
            for (z.z in Z.z){
                Rhos.z <- Rhos[[z.z]]

                exp.delta.g.z <- matrix(0, nrow = nrow(exp.delta.g),
                                        ncol = ncol(exp.delta.g))
                for (k in 1:nvals){
                    alpha.k <- alpha.vals[k]
                    idx.k   <- IDX.alpha[[k]]
                    Rho.mat <- diag(exp(-alpha.k*as.numeric(Rhos.z[[g]])/sigma.eps))
                    exp.delta.g.z[idx.k, ] <- exp.delta.g[idx.k, ]%*%Rho.mat
                }

                mu.g <- exp.delta.g.z/rowSums(exp.delta.g.z)
                Mus[[g]][[z.z]] <- mu.g
            }
        } else {
            for (z.z in Z.z){
                Mus[[g]][[z.z]] <- matrix(1, nrow = nrow(deltas), ncol = 1)
            }
        }
    }
    Eta <- compute.Eta(demand.param, buy.z)
    #== Finish generating objects used in computing sales ==#

    eV <- list()
    eV.sum <- list() # sum within a ZIP
    for (z.z in Z.z){
        eV[[z.z]] <- t(t(eV.0[[z.z]])*J.G[[z.z]])
        ## Set cells that are sufficiently low to zero
        eV[[z.z]][eV[[z.z]] < 1e-20] <- 0
        eV.sum[[z.z]] <- .rowSums(eV[[z.z]], m = nbuy, n = nportfolios)
    }
    ## Aggregate eV.sum across ZIPs
    eV.totsum <- Reduce('+', eV.sum)

    ## Compute probabilities of purchasing from G/ZIP pairs
    G.pr.base <- lapply(eV, function(x) x/eV.totsum)
    G.probs   <- lapply(G.pr.base, function(x) x*weights/sweight)
    names(G.pr.base) <- names(G.probs) <- names(eV)

    if (incl.np){
        gamma <- demand.param$gamma
        V.bar <- Eta + gamma*log(eV.totsum)
        eV.bar <- exp(V.bar/gamma)
        Lambda <- eV.bar/(1 + eV.bar)
    } else {
        Lambda <- rep(1, times = nrow(buy.z))
    }

    if (more.outputs){
        if (incl.np){
            EU.indiv <- -gamma*log(1 - Lambda)/alpha.i
            EU <- sum(EU.indiv*weights)*T.i
        } else {
            EU.indiv <- gamma*log(eV.totsum)/alpha.i
            EU <- sum(EU.indiv*weights)
        }
    }

    #== Initialize outputs ==#
    # Create array at (f, G, z) level that provides the sales on f
    # for a restaurant in z that belongs to G
    S.tot <- array(0, dim = c(nportfolios, nplatforms, nZ))
    dimnames(S.tot)[[3]] <- Z.z
    S.mat <- S.tot

    if (by.demo){
        # Output objects
        S.tot.young     <- array(0, dim = c(nportfolios, nplatforms, nZ))
        S.tot.unmarried <- array(0, dim = c(nportfolios, nplatforms, nZ))
        S.tot.lowinc    <- array(0, dim = c(nportfolios, nplatforms, nZ))

        dimnames(S.tot.young)[[3]]     <- Z.z
        dimnames(S.tot.unmarried)[[3]] <- Z.z
        dimnames(S.tot.lowinc)[[3]]    <- Z.z

        # Weights
        weights.y <- weights
        weights.y[which(buy.z$young != 1)] <- 0
        ## Add small amount so that 0/0 = 0
        sweight.y <- sum(weights.y) + 1e-100

        weights.u <- weights
        weights.u[which(buy.z$married == 0)] <- 0
        sweight.u <- sum(weights.u) + 1e-100

        weights.l <- weights
        weights.l[which(buy.z$high_income == 1)] <- 0
        sweight.l <- sum(weights.l) + 1e-100

        # G probabilities (these include weights)
        G.probs.y <- lapply(G.pr.base, function(x) x*weights.y/sweight.y)
        G.probs.u <- lapply(G.pr.base, function(x) x*weights.u/sweight.u)
        G.probs.l <- lapply(G.pr.base, function(x) x*weights.l/sweight.l)
        names(G.probs.y) <- names(G.probs.u) <- names(G.probs.l) <- names(G.pr.base)

        EU.y <- sum(EU.indiv*weights.y)
        EU.u <- sum(EU.indiv*weights.u)
        EU.l <- sum(EU.indiv*weights.l)

        if (incl.np){
            EU.y <- EU.y*T.i
            EU.u <- EU.u*T.i
            EU.l <- EU.l*T.i
        }
    }

    #== Compute sales ==#
    for (k.z in 1:nZ){
        z.z <- Z.z[[k.z]]
        J.G.z <- J.G[[z.z]]
        no.resto <- J.G.z < 1e-100

        for (g in 1:nportfolios){
            # Determine platforms belonging to the portfolio
            idx.g <- G.indices[[g]]
            # Determine intra-portfolio platform choice probabilities
            mu.g <- Mus[[g]][[z.z]]

            if (no.resto[g]){
                # Case 1: no restaurants on the platform portfolio
                # By cancelling zeros in the numerator and denominator, we can derive an expression
                j.prob <- eV.0[[z.z]][, g]/eV.totsum
                for (f in idx.g){
                    idx.f <- fg.indices[[g]][[f]]
                    mu.fg <- mu.g[, idx.f]
                    S.mat[g, f, z.z] <- sum(Lambda*mu.fg*j.prob*weights)/sweight
                    S.tot[g, f, z.z] <- 0

                    if (by.demo){
                        S.tot.unmarried[g, f, z.z] <- 0
                        S.tot.unmarried[g, f, z.z] <- 0
                        S.tot.lowinc[g, f, z.z]    <- 0
                    }
                }
            } else {
                # Case 2: positive number of restaurants on the platform
                g.probs <- G.probs[[z.z]][, g]
                S.tot[g, idx.g, z.z] <- base::crossprod((Lambda*g.probs), mu.g)

                if (by.demo){
                    # Young
                    g.probs.y <- G.probs.y[[z.z]][, g]
                    S.tot.young[g, idx.g, z.z] <- base::crossprod((Lambda*g.probs.y), mu.g)
                    # Unmarried
                    g.probs.u <- G.probs.u[[z.z]][, g]
                    S.tot.unmarried[g, idx.g, z.z] <- base::crossprod((Lambda*g.probs.u), mu.g)
                    # Low income
                    g.probs.l <- G.probs.l[[z.z]][, g]
                    S.tot.lowinc[g, idx.g, z.z] <- base::crossprod((Lambda*g.probs.l), mu.g)
                }
            }
        }

        # Equality of restaurants within a portfolio/type
        S.mat[!no.resto, , z.z] <- S.tot[!no.resto, , z.z]/J.G.z[!no.resto]
        if (scale.down){
            for (g in which(!no.resto)){
                if (J.G.z[g] < 1){
                    S.mat[g, , z.z] <- S.mat[g, , z.z]*J.G.z[g]
                }
            }
        }
    }

    # Check that sales add up to 1
    tot.sales <- sum(S.tot)
    if (incl.np){
        Share.np <- sum((1 - Lambda)*weights)/sweight
        tot.sales <- tot.sales + Share.np

        if (by.demo){
            Share.np.young     <- sum((1 - Lambda)*weights.y)/sweight.y
            Share.np.unmarried <- sum((1 - Lambda)*weights.u)/sweight.u
            Share.np.lowinc    <- sum((1 - Lambda)*weights.l)/sweight.l
        }
    }

    if (abs(tot.sales - 1) > 1e-4){
        warning('Total sales do not sum to one')
    }

    # Scale up by the count of potential transactions in the zip code
    if (incl.np){
        scale.factor <- sweight*T.i

        if (by.demo){
            scale.factor.y <- sweight.y*T.i
            scale.factor.u <- sweight.u*T.i
            scale.factor.l <- sweight.l*T.i
        }
    } else {
        scale.factor <- sweight

        if (by.demo){
            scale.factor.y <- sweight.y
            scale.factor.u <- sweight.u
            scale.factor.l <- sweight.l
        }
    }
    Sales.mat <- S.mat*scale.factor
    Sales.tot <- S.tot*scale.factor
    Sales.np  <- Share.np*scale.factor

    if (by.demo){
        Sales.tot.y <- S.tot.young*scale.factor.y
        Sales.np.y  <- Share.np.young*scale.factor.y

        Sales.tot.u <- S.tot.unmarried*scale.factor.u
        Sales.np.u  <- Share.np.unmarried*scale.factor.u

        Sales.tot.l <- S.tot.lowinc*scale.factor.l
        Sales.np.l  <- Share.np.lowinc*scale.factor.l

        # Fill in missing values (no units) with zeros
        if (any(is.nan(Sales.tot.y))){
            Sales.tot.y[, ,] <- 0
        }
        if (any(is.nan(Sales.tot.u))){
            Sales.tot.u[, ,] <- 0
        }
        if (any(is.nan(Sales.tot.l))){
            Sales.tot.l[, ,] <- 0
        }
    }

    if (more.outputs){
        out <- list(S.mat           = S.mat,
                    S.tot           = S.tot,
                    Sales.mat       = Sales.mat,
                    Sales.tot       = Sales.tot,
                    platform.shares = colSums(S.tot),
                    platform.sales  = colSums(Sales.tot),
                    EU              = EU)

        if (by.demo){
            demo.out <- list()
            demo.out$S.tot.young     <- S.tot.young
            demo.out$S.tot.unmarried <- S.tot.unmarried
            demo.out$S.tot.lowinc    <- S.tot.lowinc

            demo.out$Sales.tot.y <- Sales.tot.y
            demo.out$Sales.tot.u <- Sales.tot.u
            demo.out$Sales.tot.l <- Sales.tot.l

            demo.out$EU.y <- EU.y
            demo.out$EU.u <- EU.u
            demo.out$EU.l <- EU.l

            out$by.demo <- demo.out
        }
        if (incl.np){
            out$Share.np <- Share.np
            out$Sales.np <- Sales.np

            if (by.demo){
                demo.out <- out$by.demo

                demo.out$Share.np.young <- Share.np.young
                demo.out$Sales.np.y     <- Sales.np.y

                demo.out$Share.np.unmarried <- Share.np.unmarried
                demo.out$Sales.np.u         <- Sales.np.u

                demo.out$Share.np.lowinc <- Share.np.lowinc
                demo.out$Sales.np.l      <- Sales.np.l

                out$by.demo <- demo.out
            }
        }
    } else {
        out <- Sales.mat
    }
    return(out)
}
