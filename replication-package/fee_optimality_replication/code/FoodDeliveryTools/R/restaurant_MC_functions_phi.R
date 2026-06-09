
phi.objective.NPP <- function(phi, dat, rhos, opts){

    markets <- names(dat)
    costs <- list()

    for (market in markets){
        dat.m <- dat[[market]]
        costs[[market]] <- compute.costs.phi.NPP(dat.m, rhos, phi, opts)
    }

    ## Form moment condition
    NG <- length(costs[[markets[1]]]$costs[[1]]) # number of portfolios

    IV      <- list()
    Costs.0 <- list()
    Costs.1 <- list()

    for (market in markets){
        costs.m <- costs[[market]]$costs
        Costs.0.m <- list()
        Costs.1.m <- list()

        # Instrumental variable
        IV.m <- c()

        zips.m <- names(costs.m)
        for (z in zips.m){
            costs.z <- costs.m[[z]]

            # Determine whether a commission cap is in place in the ZIP
            IV.m[z] <- 1*(max(dat[[market]]$comm[[z]]) < 0.30)

            costs.z.off <- c()
            costs.z.on  <- c()

            for (g in 1:length(costs.z)){
                if (is.null(costs.z[[g]])){
                    next
                } else {
                    costs.zg <- costs.z[[g]]
                    costs.zg.0 <- costs.zg[1]
                    costs.z.off[as.character(g)] <- costs.zg.0

                    if (g > 1){
                        costs.zg.1 <- costs.zg[2:length(costs.zg)]
                        on.names <- sprintf('%d-%d', g, 1:length(costs.zg.1))
                        costs.z.on[on.names] <- costs.zg.1
                    }
                }
            }
            costs.off.df <- data.frame(g  = names(costs.z.off),
                                       mc = costs.z.off)
            costs.off.df$zip <- z
            Costs.0.m[[z]] <- costs.off.df

            if (length(costs.z.on) > 0){
                costs.on.df <- data.frame(gf = names(costs.z.on),
                                          mc = costs.z.on)
                costs.on.df$zip <- z
                Costs.1.m[[z]]  <- costs.on.df
            }
        }

        # Data frame for marginal costs
        Costs.0.df <- do.call(dplyr::bind_rows, Costs.0.m)
        Costs.1.df <- do.call(dplyr::bind_rows, Costs.1.m)
        Costs.0.df$market <- market
        Costs.1.df$market <- market
        Costs.0[[market]] <- Costs.0.df
        Costs.1[[market]] <- Costs.1.df

        # Data frame for instrumental variables
        IV.df <- data.frame(zip = names(IV.m), IV = IV.m)
        IV[[market]] <- IV.df
    }

    Costs.0.df <- do.call(dplyr::bind_rows, Costs.0)
    Costs.1.df <- do.call(dplyr::bind_rows, Costs.1)
    IV.df <- do.call(dplyr::bind_rows, IV)

    # Merge in IV to cost data.frame
    Costs.0.df <- dplyr::left_join(Costs.0.df, IV.df, by = 'zip')
    Costs.1.df <- dplyr::left_join(Costs.1.df, IV.df, by = 'zip')

    # Compute chain indicator
    Costs.0.df$chain <- 1*(grepl('c$', Costs.0.df$zip))
    Costs.1.df$chain <- 1*(grepl('c$', Costs.1.df$zip))

    # Compute residuals
    reg.0 <- lm(mc ~ chain, data = Costs.0.df)
    Costs.0.df$omega <- reg.0$residuals
    reg.1 <- lm(mc ~ chain, data = Costs.1.df)
    Costs.1.df$omega <- reg.1$residuals

    # Compute moment condition
    fval <- mean(Costs.1.df$omega*Costs.1.df$IV)

    outputs <- list()
    outputs$fval <- fval
    outputs$Costs.1.df <- Costs.1.df
    outputs$Costs.0.df <- Costs.0.df
    outputs$IV.df      <- IV.df
    outputs$costs      <- costs

    return(outputs)
}

compute.costs.phi.NPP <- function(dat.m, rhos, phi, opts){
    # phi is the parameter that governs the non-parity penalty

    buy.m        <- dat.m$buy.m
    zip.mat.m    <- dat.m$zip.mat.m
    J.G.1.m      <- dat.m$J.G.1.m
    Jp.G.1.m     <- dat.m$Jp.G.1.m
    cap.df       <- dat.m$caps.df.1
    G.mat        <- dat.m$G.mat
    nplatforms   <- dat.m$nplatforms
    nportfolios  <- dat.m$nportfolios
    G.indices    <- dat.m$G.indices
    excl.chain   <- dat.m$excl.chain

    cap.df$cap.any <- cap.df$cap < 0.30

    zips <- names(Jp.G.1.m)

    platforms <- c('dd', 'uber', 'gh', 'pm')
    NF <- length(platforms)

    # Find commissions charged to restaurants by platform
    P.r <- list()
    for (zip in zips){
        ## Set prices based on commission caps
        idx.cap <- which(cap.df$zip == zip)
        if (length(idx.cap) > 0){
            cap <- cap.df$caps[idx.cap]
        } else {
            cap <- 0.3
        }
        # Determine if chains are exempt from the cap
        z0 <- sub('[ci]$', '', zip)
        is.chain <- grepl('c$', zip)
        if (cap < 0.3 & excl.chain[z0] & is.chain){
            cap <- 0.3
        }
        p.r <- cap*rep(1, times = nplatforms - 1)

        # Account for outside platform
        p.r <- c(offline = 0, p.r)

        # Store the result
        P.r[[zip]] <- p.r
    }

    # Set menu prices Rhos based on the data
    Rhos.0 <- list()
    for (zip in zips){
        Rhos.0[[zip]] <- list()

        # Determine which commissions apply
        comm.z <- cap.df$caps[which(cap.df$zip == zip)]

        # Specify prices accordingly
        idx.rhos <- which(rhos$r == comm.z)
        rho.z <- as.numeric(rhos[idx.rhos, c('direct', platforms)])
        names(rho.z) <- c('direct', platforms)

        for (g in 1:nportfolios){
            ng <- sum(G.mat[g, ])
            idx.g <- dat.m$G.indices[[g]]
            Rhos.0[[zip]][[g]] <- rho.z[idx.g]
        }
    }

    # Compute the sales and derivatives for each zip code
    sales.dat <- restaurant.sales(dat.m, Rhos.0, opts = opts)
    Sales <- sales.dat$Sales
    Deriv <- sales.dat$Deriv

    # Initialize outputs
    markups <- list()
    costs   <- list()
    weights <- list()

    for (zip in zips){
        # If there is only one restaurant in range of the zip code, the
        # optimal prices are not well defined. Skip the zip code.
        if (sum(Jp.G.1.m[[zip]]) == 1){
            next
        }

        # Extract zip code's sales and derivatives matrices
        S.z  <- Sales[[zip]]
        DS.z <- Deriv[[zip]]
        # Skip empty zips
        if (is.null(S.z)){
            next
        }
        if (length(S.z) == 0){
            next
        }
        # Extract prices charged to restaurants by platforms
        p.r <- P.r[[zip]]

        # Back out costs using the price charged to restaurants
        # on the first platform portfolio
        markups[[zip]] <- list()
        costs[[zip]] <- list()
        weights[[zip]] <- list()
        for (g in 1:nportfolios){
            if (J.G.1.m[[zip]][g] == 0){
                next
            }

            # Extract platform members
            idx.g <- G.indices[[g]]
            NG <- length(idx.g)

            # Extract platforms' commissions
            R <- p.r[idx.g]

            # Extract relevant matrices
            s.0 <- matrix(S.z[g, idx.g], ncol = 1)
            s.tilde <- s.0*matrix(1 - R, ncol = 1)

            if (g > 1){
                p.zg <- Rhos.0[[zip]][[g]]
                pdiff <- p.zg - p.zg[1]

                # Vector of price differences
                dvec <- pdiff*s.0
                dvec[1] <- -sum(pdiff*s.0)

            } else {
                pdiff <- 0
                dvec <- 0
            }
            dvec <- matrix(dvec, ncol = 1)

            # Compute inverse of Delta matrix
            Delta.0 <- DS.z[[g]]
            Delta.inv <- tryCatch({
                solve(Delta.0)
            },
            error = function(cond) {
                # Zip-code/portfolio pairs with no restaurants can yield a
                # singular Delta during the menu-price FOC iterations.
                # Regularise the diagonal to make it invertible; this has
                # no effect on results because no restaurants carry these
                # menu prices.
                diag(Delta.0) <- diag(Delta.0) + 1
                delta.inv <- solve(Delta.0)
                return(delta.inv)
            })

            # Compute markups
            b <- -Delta.inv%*%(s.tilde - phi*dvec) + phi/2*pdiff^2

            # Compute marginal cost
            MC.zg <- Rhos.0[[zip]][[g]]*(1 - R) - b
            # Store objects
            markups[[zip]][[g]] <- b
            costs[[zip]][[g]]   <- MC.zg
            # Store weights
            weights[[zip]][[g]] <- pracma::repmat(matrix(J.G.1.m[[zip]][g], ncol = 1), length(idx.g), 1)
        }
    }

    # Extract the offline costs for each ZIP
    costs.off   <- lapply(costs,   function(x) sapply(x, function(xg) xg[1, 1]))
    weights.off <- lapply(weights, function(x) sapply(x, function(xg) xg[1, 1]))

    # Online costs
    costs.on   <- lapply(costs,   function(x) extract.online.costs(x))
    weights.on <- lapply(weights, function(x) extract.online.costs(x))

    costs.by.zip <- lapply(costs.off, unlist)
    weights.by.zip <- lapply(weights.off, unlist)
    costs.off <- sapply(zips, function(z) sum(costs.by.zip[[z]]*weights.by.zip[[z]])/sum(weights.by.zip[[z]]))

    # Replace NaN values with the market average
    weights.mkt <- sapply(zips, function(z) sum(weights.by.zip[[z]]))
    costs.off[which(is.nan(costs.off))] <- weighted.mean(costs.off, w = weights.mkt, na.rm = TRUE)

    # Intra-ZIP standard deviation for direct orders
    cost.SD <- sapply(zips, function(z) weighted.sd(costs.by.zip[[z]], w = weights.by.zip[[z]]))
    mean.cost.off.SD <- weighted.mean(cost.SD, w = weights.mkt, na.rm = TRUE)

    # Online costs
    costs.by.zip   <- lapply(costs.on, unlist)
    weights.by.zip <- lapply(weights.on, unlist)
    costs.on <- sapply(zips, function(z) sum(costs.by.zip[[z]]*weights.by.zip[[z]], na.rm = TRUE)/sum(weights.by.zip[[z]], na.rm = TRUE))

    # Replace NaN values with the market average
    weights.mkt <- sapply(zips, function(z) sum(weights.by.zip[[z]]))
    costs.on[which(is.nan(costs.on))] <- weighted.mean(costs.on, w = weights.mkt, na.rm = TRUE)

    # Intra-ZIP standard deviation for platforms
    cost.SD <- sapply(zips, function(z) weighted.sd(costs.by.zip[[z]], w = weights.by.zip[[z]]))
    mean.cost.on.SD <- weighted.mean(cost.SD, w = weights.mkt, na.rm = TRUE)

    outputs <- list(costs.on = costs.on, costs.off = costs.off,
                    mean.cost.on.SD  = mean.cost.on.SD,
                    mean.cost.off.SD = mean.cost.off.SD,
                    mkt.weight = sum(weights.mkt),
                    costs   = costs,
                    markups = markups)

    return(outputs)
}
