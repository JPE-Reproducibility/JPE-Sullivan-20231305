compute.S.derivatives.v2 <- function(J.G, G.mat, p.c, demand.param, buy.z,
                                     G.indices, fg.indices,
                                     Rhos = NULL, level = TRUE, T.i = 17,
                                     keep.platforms = NULL){
    # Compute the derivatives (with respect to price) of sales received
    # by a restaurant in range of a zip code with
    # distribution `J.G` of nearby restaurants across portfolios
    #
    # Inputs
    #   level: compute derivatives of sales **levels** instead of sales **shares**?

    if (is.null(keep.platforms)){
        NF <- ncol(G.mat) - 1
        keep.platforms <- rep(1, times = NF)
    }
    keep.G <- determine.portfolios(G.mat, keep.platforms)
    inactive.platforms <- which(keep.platforms == 0)

    # Preliminaries
    alpha <- demand.param$alpha
    gamma <- demand.param$gamma

    nportfolios <- length(J.G[[1]])
    nZ          <- length(J.G)
    nplatforms  <- ncol(G.mat)
    nbuy        <- nrow(buy.z)
    weights     <- buy.z$count
    sweight     <- sum(weights)

    sigma.eps <- load.sigma.eps()

    Z.z <- names(J.G)

    # Include the option to make no purchase?
    incl.np <- ('mu.eta' %in% names(demand.param))

    # Compute price coefficients
    het.extra <- ('alpha.young' %in% names(demand.param))

    alpha <- demand.param$alpha
    if (het.extra) {
        a.y <- demand.param$alpha.young*buy.z$young
        a.m <- demand.param$alpha.married*buy.z$married
        a.h <- demand.param$alpha.highinc*buy.z$high_income
        alpha.i <- alpha + a.y + a.m + a.h
    } else {
        alpha.i <- rep(alpha, times = nrow(buy.z))
    }

    alpha.vals <- unique(alpha.i)
    nvals      <- length(alpha.vals)

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
                if (!is.null(Rhos.z)){
                    exp.delta.g.z <- matrix(0, nrow = nrow(exp.delta.g),
                                            ncol = ncol(exp.delta.g))
                    for (k in 1:nvals){
                        alpha.k <- alpha.vals[k]
                        idx.k   <- which(alpha.i == alpha.k)
                        Rho.mat <- diag(exp(-alpha.k*as.numeric(Rhos.z[[g]])/sigma.eps))
                        exp.delta.g.z[idx.k, ] <- exp.delta.g[idx.k, ]%*%Rho.mat
                    }
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

    ## Account for no-purchase option
    if (incl.np){
        eV.totsum <- eV.totsum + exp(-Eta/gamma)
    }

    # Precompute L.g ("lambdas]"), which are the probabilities of choosing
    # a particular restaurant in ZIP z.z with portfolio g.
    # These are inclusive of the outside restaurant when incl.np == TRUE
    L.g.probs <- list()
    for (z.z in Z.z){
        L.g.probs[[z.z]] <- lapply(1:nportfolios, function(g) eV.0[[z.z]][, g]/eV.totsum)
    }

    # Initialize outputs
    G.derivs <- list()

    #== Compute derivatives ==#
    for (k.z in 1:nZ){
        z.z <- Z.z[[k.z]]
        J.G.z <- J.G[[z.z]]
        no.resto <- J.G.z < 1e-100

        G.derivs[[z.z]] <- list()

        for (g in 1:nportfolios){
            # Determine platforms belonging to the portfolio
            idx.g <- G.indices[[g]]
            g.size <- length(idx.g)
            # Determine intra-portfolio platform choice probabilities
            mu.g <- Mus[[g]][[z.z]]

            # Initialize output
            g.derivs <- matrix(0, nrow = g.size, ncol = g.size)

            # Account for case of no restaurants
            if (no.resto[g]){
                G.derivs[[z.z]][[g]] <- g.derivs
                next
            }

            # Probability of choosing a particular restaurant in z.z with G
            # (*not* the inside share Lambdas)
            L.g <- L.g.probs[[z.z]][[g]]
            ## If there is less than one restaurant, scale down the probability
            ## of choosing a restaurant
            if (J.G.z[g] < 1){
                L.g <- L.g*J.G.z[g]
            }

            # Own-price derivatives
            own.derivs <- c()
            for (f in idx.g){
                idx.f <- fg.indices[[g]][[f]]
                mu.fg <- mu.g[, idx.f]
                s.deriv.own <- -alpha.i*mu.fg*L.g*((1 - mu.fg)/sigma.eps + (1/gamma)*mu.fg*(1 - L.g))
                own.derivs[idx.f] <- sum(s.deriv.own*weights)/sweight
            }
            diag(g.derivs) <- own.derivs

            # Cross-price derivatives
            if (g.size > 1){
                for (idx.f1 in 1:(g.size - 1)){
                    mu.1 <- mu.g[, idx.f1]
                    for (idx.f2 in (idx.f1 + 1):g.size){
                        mu.2 <- mu.g[, idx.f2]
                        # Compute derivative
                        s.deriv.cross <- alpha.i*mu.1*mu.2*L.g*(1/sigma.eps - (1/gamma)*(1 - L.g))
                        g.derivs[idx.f1, idx.f2] <- sum(s.deriv.cross*weights)/sweight
                        # Exploit symmetry
                        g.derivs[idx.f2, idx.f1] <- g.derivs[idx.f1, idx.f2]
                    }
                }
            }

            if (level){
                # Scale up the derivative matrix
                g.derivs <- g.derivs*sweight

                if (incl.np){
                    g.derivs <- g.derivs*T.i
                }
            }

            G.derivs[[z.z]][[g]] <- g.derivs
        }
    }

    return(G.derivs)
}
