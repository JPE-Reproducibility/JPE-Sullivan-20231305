compute.inclusive.values <- function(deltas, demand.param, G.mat, G.indices,
                                     Rhos.z = NULL, buy.z = NULL,
                                     sigma.eps = NULL, is.chain = FALSE){
    # Compute each consumer's inclusive value for each set of platforms
    #
    # Inputs
    #   p.c: Prices charged to consumers
    #   demand.param: Parameters of the consumer choice model
    #   G.mat: Portfolio membership matrix
    #   buy: Consumer data
    #   Rhos (optional): prices charged by a restaurant with each portfolio on
    #       each platform in that portfolio

    sigma.eps <- load.sigma.eps()
    alpha <- demand.param$alpha

    het.extra  <- ('alpha.young' %in% names(demand.param))

    if (het.extra){
        a.y <- demand.param$alpha.young*buy.z$young
        a.m <- demand.param$alpha.married*buy.z$married
        a.h <- demand.param$alpha.highinc*buy.z$high_income
        alpha.i <- alpha + a.y + a.m + a.h
    }

    # Adjust for chain status
    if (is.chain){
        phi.c <- demand.param$phi.c
        phi.i <- demand.param$sigma.phi*buy.z$phi.i
        deltas <- deltas + phi.c + phi.i
    }

    # Add up exponentiated deltas across platforms in each portfolio
    ## For numerical reasons, subtract a constant off of each delta and
    ## add it back at the end
    delta.bar <- max(deltas)*0.25
    deltas.adj <- deltas - delta.bar
    exp.delta <- exp(deltas.adj/sigma.eps)

    # This may not be enough; there may still be some Infs.
    # In this case, set Inf to a very large number
    large.number <- 1e200
    exp.delta[is.infinite(exp.delta)] <- large.number
    # Ensure nothing is greater than infinity
    exp.delta[exp.delta > large.number] <- large.number

    nportfolios <- nrow(G.mat)

    # Adjust for Rhos
    if (het.extra){
        alpha.vals <- unique(alpha.i)
        nvals <- length(alpha.vals)

        Rho.Mats <- list()
        Idx      <- list()

        for (k in 1:nvals){
            alpha.k <- alpha.vals[k]
            Idx[[k]] <- which(alpha.i == alpha.k)

            rho.mat <- G.mat
            if (!is.null(Rhos.z)){
                for (g in 1:nportfolios){
                    idx.g <- G.indices[[g]]

                    # Fill in null prices
                    if (is.null(Rhos.z[[g]])){
                        Rhos.z[[g]] <- matrix(Rhos.z[[length(Rhos.z)]][idx.g], ncol = 1)
                    }

                    rho.mat[g, idx.g] <- exp(-alpha.k*Rhos.z[[g]]/sigma.eps)
                }
            }
            Rho.Mats[[k]] <- rho.mat
        }


        V.adj <- matrix(0, nrow = nrow(exp.delta), ncol = nrow(rho.mat))
        for (k in 1:nvals){
            idx.k <- Idx[[k]]
            rho.mat <- Rho.Mats[[k]]
            V.adj[idx.k, ] <- exp.delta[idx.k, ]%*%t(rho.mat)
        }

    } else {
        rho.mat <- G.mat
        if (!is.null(Rhos.z)){

            for (g in 1:nportfolios){
                idx.g <- G.indices[[g]]

                # Fill in null prices
                if (is.null(Rhos.z[[g]])){
                    Rhos.z[[g]] <- matrix(Rhos.z[[length(Rhos.z)]][idx.g], ncol = 1)
                }

                rho.mat[g, idx.g] <- exp(-alpha*Rhos.z[[g]]/sigma.eps)
            }
        }

        V.adj <- exp.delta%*%t(rho.mat)
    }

    V.adj <- sigma.eps*log(V.adj)
    V <- V.adj + delta.bar
    return(V)
}

compute.deltas <- function(p.c, demand.param, buy, incl.0 = FALSE){
    # Compute consumers' utility indices for the various platforms

    # Determine settings
    het.extra  <- ('alpha.young' %in% names(demand.param))
    lambda.inc <- nrow(demand.param$lambda) == 3

    # Extract parameters
    ## Price sensitivity parameters
    alpha <- demand.param$alpha

    if (het.extra){
        alpha.young   <- demand.param$alpha.young
        alpha.married <- demand.param$alpha.married
        alpha.highinc <- demand.param$alpha.highinc

        a.y <- alpha.young*buy$young
        a.m <- alpha.married*buy$married
        a.h <- alpha.highinc*buy$high_income
    }
    use.WT <- ('tau' %in% names(demand.param))
    if (use.WT){
        tau <- demand.param$tau
    }
    ## Other parameteres
    psi      <- demand.param$psi
    sigma.z1 <- demand.param$sigma.z1
    sigma.z2 <- demand.param$sigma.z2
    lambda   <- demand.param$lambda

    delta.base <- psi - alpha*p.c
    ndelta <- length(delta.base)
    deltas <- list()

    for (f in 1:ndelta){
        # Add zetas
        vname <- sprintf('zeta.2-%d', f)
        deltas.f <- delta.base[f] + sigma.z1*buy$zeta.1 + sigma.z2*buy[[vname]]
        # Add lambdas
        deltas[[f]] <- deltas.f + lambda[1, f]*buy$young + lambda[2, f]*buy$married
        if (lambda.inc){
            deltas[[f]] <- deltas[[f]] + lambda[3, f]*buy$high_income
        }

        if (het.extra){
            deltas[[f]] <- deltas[[f]] - (a.y + a.m + a.h)*p.c[f]
        }

        if (use.WT){
            conv.factor <- 1/60 # Convert to hours
            deltas[[f]] <- deltas[[f]] - tau*buy[, sprintf('WT_%d', f)]*conv.factor
        }
    }

    deltas <- Reduce(cbind, deltas)

    # Include zeros
    if (incl.0){
        deltas <- cbind(0, deltas)
    }
    return(deltas)
}

