compute.expected.delta.v2 <- function(deltas, demand.param, G.mat, G.indices,
                                      Rhos, buy.z = NULL){
    # Compute each consumer's inclusive value for each set of platforms minus the
    # realized logit shock, i.e., the expected delta
    #
    # Inputs
    #   p.c: Prices charged to consumers
    #   demand.param: Parameters of the consumer choice model
    #   G.mat: Portfolio membership matrix
    #   buy: Consumer data
    #   Rhos: prices charged by a restaurant with each portfolio on
    #       each platform in that portfolio

    sigma.eps <- load.sigma.eps()

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

    Phi <- demand.param$phi.c + buy.z$phi.i*demand.param$sigma.phi

    alpha.vals <- unique(alpha.i)
    nvals      <- length(alpha.vals)

    nportfolios <- nrow(G.mat)

    # Adjust for rhos
    Deltas <- list() # deltas adjusted for rhos
    Mus    <- list() # platform-specific choice probabilities

    large.number <- 1e200

    Z.z <- names(Rhos)

    # Adjust for rhos and phi
    for (g in 1:nportfolios){
        idx.g <- G.indices[[g]]
        g.size <- length(idx.g)

        Deltas[[g]] <- list()
        Mus[[g]]    <- list()

        if (g.size > 1){
            # Objects used in computing choice probabilities
            exp.delta.g <- exp(deltas[, idx.g]/sigma.eps)
            exp.delta.g[is.infinite(exp.delta.g)]    <- large.number
            exp.delta.g[exp.delta.g >= large.number] <- large.number

            for (z.z in Z.z){

                is.chain <- grepl('c$', z.z)

                Rhos.z <- Rhos[[z.z]]
                deltas.g.z <- matrix(0, nrow = nrow(deltas), ncol = g.size)

                Rhos.z.mat <- pracma::repmat(matrix(Rhos.z[[g]], nrow = 1),
                                             nrow(deltas), 1)


                exp.delta.g.z <- matrix(0, nrow = nrow(exp.delta.g),
                                        ncol = ncol(exp.delta.g))

                # Loop over price sensitivities
                for (k in 1:nvals){
                    alpha.k <- alpha.vals[k]
                    idx.k   <- which(alpha.i == alpha.k)
                    R <- Rhos.z.mat[idx.k, ]
                    D <- deltas[idx.k, idx.g]
                    deltas.g.z[idx.k, ] <- D - alpha.k*R
                    if (is.chain){
                        deltas.g.z[idx.k, ] <- deltas.g.z[idx.k, ] + Phi[idx.k]
                    }

                    # For computing choice probabilities
                    Rmat <- diag(exp(-alpha.k*as.numeric(Rhos.z[[g]])/sigma.eps))
                    exp.delta.g.z[idx.k, ] <- exp.delta.g[idx.k, ]%*%Rmat
                }

                Deltas[[g]][[z.z]] <- deltas.g.z

                mu.g <- exp.delta.g.z/rowSums(exp.delta.g.z)
                Mus[[g]][[z.z]] <- mu.g
            }
        } else {
            for (z.z in Z.z){

                is.chain <- grepl('c$', z.z)

                Rhos.z <- Rhos[[z.z]]
                Rhos.z.mat <- pracma::repmat(matrix(Rhos.z[[g]], nrow = 1),
                                             nrow(deltas), 1)

                # Loop over price sensitivities
                deltas.g.z <- matrix(0, nrow = nrow(deltas), ncol = 1)
                for (k in 1:nvals){
                    alpha.k <- alpha.vals[k]
                    idx.k   <- which(alpha.i == alpha.k)
                    R <- Rhos.z.mat[idx.k, ]
                    D <- deltas[idx.k, 1]
                    deltas.g.z[idx.k, 1] <- D - alpha.k*R

                    if (is.chain){
                        deltas.g.z[idx.k, 1] <- deltas.g.z[idx.k, 1] + Phi[idx.k]
                    }
                }

                Deltas[[g]][[z.z]] <- deltas.g.z
                Mus[[g]][[z.z]] <- matrix(1, nrow = nrow(deltas), ncol = 1)
            }
        }
    }

    # Intraportfolio choice probabilities
    V.tilde <- list()
    for (z.z in Z.z){
        V.z <- list()

        for (g in 1:nportfolios){
            deltas.g.z <- Deltas[[g]][[z.z]]

            if (g == 1){
                V.z[[g]] <- deltas.g.z
            } else {
                # Add up exponentiated deltas across platforms in each portfolio
                exp.delta <- exp(deltas.g.z/sigma.eps)

                exp.delta[is.infinite(exp.delta)]   <- large.number
                exp.delta[exp.delta > large.number] <- large.number

                # Choice probabilities
                CP <- exp.delta/rowSums(exp.delta)
                V.z[[g]] <- rowSums(deltas.g.z*CP)
            }
        }

        V.tilde[[z.z]] <- Reduce(cbind, V.z)
    }

    return(V.tilde)
}

