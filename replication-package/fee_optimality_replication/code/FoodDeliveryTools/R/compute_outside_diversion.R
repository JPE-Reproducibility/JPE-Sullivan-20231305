

compute.outside.diversion <- function(J.G, G.mat, p.c, demand.param, buy.z,
                                      G.indices, fg.indices,
                                      Rhos = NULL, level = TRUE, T.i = 17,
                                      keep.platforms = NULL){
    # Compute
    #      ds_{0}/dp_{jf}
    # for restaurant j, where s_0 are the sales of the outside option.
    # Note that
    #   ds_{0}/dp_{jf} = alpha/gamma*lambda_0*lambda_j*mu_{fj}
    # Inputs
    #   level: Compute derivatives of level of sales as opposed to market shares?

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

    Z.z <- names(J.G)

    # Include the option to make no purchase?
    incl.np <- ('mu.eta' %in% names(demand.param))

    # Smoothing parameter
    sigma.eps <- load.sigma.eps()

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

    ## Compute objects appearing in expressions for derivatives
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
    Eta <- compute.Eta(demand.param, buy.z)
    if (incl.np){
        eV.totsum <- eV.totsum + exp(-Eta/gamma)
    }
    eV.totsum <- matrix(eV.totsum, ncol = 1)

    # Compute the Lambda and Mu objects in a pre-computation stage
    j.probs <- list() # Probability of choosing a particular restaurant
    Mus     <- list() # Probability of choosing a platform within a restaurant
    large.number <- 1e200

    for (g in 1:nportfolios){
        idx.g <- G.indices[[g]]
        g.size <- length(idx.g)

        Mus[[g]]     <- list()
        j.probs[[g]] <- list()

        if (g.size > 1){
            exp.delta.g <- exp(deltas[, idx.g]/sigma.eps)
            exp.delta.g[is.infinite(exp.delta.g)]    <- large.number
            exp.delta.g[exp.delta.g >= large.number] <- large.number
        }

        if (!(g %in% keep.G)){
            # If the portfolio is irrelevant, assign all intraportfolio sales
            # to the first-party ordering channel
            for (z.z in Z.z){
                Mus[[g]][[z.z]] <- matrix(0, nrow = nrow(deltas), ncol = g.size)
                Mus[[g]][[z.z]][, 1] <- 1
            }
        } else {
            for (z.z in Z.z){
                j.probs[[g]][[z.z]] <- eV.0[[z.z]][, g]/eV.totsum

                # Determine intra-portfolio platform choice probabilities
                if (g.size > 1){
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
                    # Compute choice probabilities
                    mu.g <- exp.delta.g.z/rowSums(exp.delta.g.z)

                } else {
                    mu.g <- matrix(1, nrow = nrow(deltas), ncol = 1)
                }
                Mus[[g]][[z.z]] <- mu.g
            }
        }
    }

    # Probability of selecting outside option
    L.0 <- exp(-Eta/gamma)/eV.totsum

    # Compute derivatives
    ## Initialize output
    outside.derivs <- list()

    for (k.z in 1:nZ){
        z.z <- Z.z[[k.z]]
        J.G.z <- J.G[[z.z]]
        no.resto <- J.G.z < 1e-100

        outside.derivs[[z.z]] <- list()

        ## Outer loop: over platform portfolios g. We compute the extent
        ## sales on each platform f change when a restaurant on g
        ## increases its menu price on f
        for (g in 1:nportfolios){
            # Determine platforms belonging to the portfolio
            idx.g <- G.indices[[g]]
            # Determine the number of platforms on the portfolio
            g.size <- length(idx.g)
            # Extract lambda.g and mu.g
            L.g <-  j.probs[[g]][[z.z]]
            mu.g <- Mus[[g]][[z.z]]

            # Initialize output
            # Each component of g.derivs gives the change in the platform's sales
            # when the restaurant increases its prices
            g.derivs <- c()
            ## Inner loop: we loop over the platforms upon which a restaurant on g
            ## increases its menu price
            for (f in idx.g){
                # Find the probability that a consumer selecting a restaurant
                # on portfolio g purchases from platform f
                idx.f <- fg.indices[[g]][[f]]
                # Compute the part of the expression for restaurant k
                base.term <- alpha.i/gamma*L.g*L.0*mu.g[, idx.f]
                g.derivs[idx.f] <- weighted.mean(base.term, w = weights)
            }

            if (level){
                g.derivs <- g.derivs*sum(weights)*T.i
            }

            outside.derivs[[z.z]][[g]] <- g.derivs
        }
    }


    return(outside.derivs)
}





