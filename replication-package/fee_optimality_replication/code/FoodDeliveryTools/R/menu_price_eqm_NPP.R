menu.price.eqm.NPP <- function(dat.m, mc, num.param,
                           verbose = FALSE, menu.subset = NULL,
                           opts = NULL, return.markup = FALSE,
                           keep.platforms = NULL){
    # Compute restaurant pricing equilibrium
    #
    # Inputs
    #   dat.m: Data for market m
    #   mc: Marginal cost of the representative food item
    #       Scalar for homo costs, 2-vector for heterogeneous online/offline costs
    #   num.param: Numerical parameters
    #   menu.subset: subset of zip codes for which to compute menu pricing equilibrium

    # Adjustment to restaurant price setting
    phi <- dat.m$phi2

    NF <- dat.m$nplatforms - 1
    if (is.null(keep.platforms)){
        keep.platforms <- rep(1, times = NF)
    }
    active.platforms   <- which(keep.platforms == 1)
    inactive.platforms <- which(keep.platforms == 0)

    # Extract numerical parameters
    max.iter   <- num.param$max.iter.menu
    tol        <- num.param$tol.menu
    learn.rate <- num.param$learn.rate.menu
    if (is.null(learn.rate)){
        learn.rate <- 1.0
    }

    # Threshold for detecting divergence
    max.dist <- 30

    # Determine cost structure
    het.costs <- (length(mc) > 1)
    if (het.costs){
        by.zip <- (length(mc$on) > 1)
    } else {
        by.zip <- FALSE
    }

    if (het.costs){
        mc.off <- mc[['off']]
        mc.on <- mc[['on']]
    } else {
        mc.off <- mc
        mc.on  <- mc
    }

    # Extract data
    buy.m        <- dat.m$buy.m
    zip.mat.m    <- dat.m$zip.mat.m
    J.G.1.m      <- dat.m$J.G.1.m
    Jp.G.1.m     <- dat.m$Jp.G.1.m
    comm         <- dat.m$comm
    demand.param <- dat.m$demand.param
    G.mat        <- dat.m$G.mat
    nplatforms   <- dat.m$nplatforms
    nportfolios  <- dat.m$nportfolios
    G.indices    <- dat.m$G.indices
    excl.chain   <- dat.m$excl.chain

    if (is.null(J.G.1.m)){
        stop('dat.m$J.G.1.m must be provided')
    }

    # Determine which portfolios to include
    keep.portfolios <- determine.portfolios(G.mat, keep.platforms)

    # Determine if platforms have been abolished through low psi
    abolish <- max(dat.m$demand.param$psi) < -40

    # Obtain a list of zip codes
    zips <- names(Jp.G.1.m)
    if (is.null(menu.subset)){
        menu.subset <- zips
    }

    # Determine commissions
    comm0 <- add.offline(comm)
    if (length(inactive.platforms) > 0){
        for (j in 1:length(comm0)){
            # add 1 to account for the "offline" platform
            comm0[[j]][inactive.platforms + 1] <- Inf
        }
    }

    ## Approximate markup offline (alpha is homogeneous in the estimated
    ## spec; load.param() sets the demographic interaction terms to zero)
    alpha <- demand.param$alpha
    b <- demand.param$gamma/alpha
    ratio <- 1/6

    # Initialize the rho vector
    test.mode <- 'a' %in% zips

    # Set initial list of Rhos
    Rhos.0 <- list()
    for (zip in zips){
        R.guess <- comm0[[zip]]
        Rhos.0[[zip]] <- list()

        if (test.mode) {
            mc.off.z <- median(mc.off)
            mc.on.z  <- median(mc.on)
        } else if (by.zip){
            mc.off.z <- mc.off[zip]
            mc.on.z  <- mc.on[zip]
        } else {
            mc.off.z <- mc.off
            mc.on.z  <- mc.on
        }

        for (g in 1:nportfolios){
            ng <- sum(G.mat[g, ])

            if (g %in% keep.portfolios){
                idx.g <- dat.m$G.indices[[g]]

                if (ng == 1){
                    # Initialize Rho to mc/(1 - p.r) + gamma/alpha
                    Rhos.0[[zip]][[g]] <-  mc.off.z + b
                } else {
                    r <- R.guess[idx.g]
                    r <- r[2:length(r)]

                    pdiff <- mc.on.z/(1 - mean(r)) - mc.off.z
                    rho.0 <- mc.off.z + b + b*phi*pdiff*ratio
                    r <- R.guess[idx.g]
                    r <- r[2:length(r)]
                    rho.1 <- rep(mc.on.z, times = ng - 1)/(1 - r) + b
                    init.rho <- c(rho.0, rho.1)
                    ## Adjustment for phi2
                    Rhos.0[[zip]][[g]] <- init.rho
                }
            } else {
                # This portfolio is not available
                Rhos.0[[zip]][[g]] <- rep(Inf, times = ng)
            }
        }
    }

    # Compute total number of restaurants, which is used in computing weights
    ## Total number in each ZCTA
    nres.z <- sapply(zips, function(z) sum(J.G.1.m[[z]]))
    ## Overall total
    nres.tot <- sum(nres.z)
    ## Weights for ZCTAs
    weights.z <- sapply(menu.subset, function(z) nres.z[z])
    weights.z <- weights.z/sum(weights.z)
    names(weights.z) <- menu.subset
    # Share of platform portfolios within ZCTA
    weights.zg <- lapply(menu.subset, function(zip) J.G.1.m[[zip]]/nres.z[zip])
    names(weights.zg) <- menu.subset
    # Specify list of ZCTAs for which to compute restaurant sales
    sales.zips <- menu.subset

    ## Only allow restaurants to make online sales on exclusive portfolios?
    exclusive <- FALSE
    ## Indices of non-exclusive portfolios
    non.exclusive <- which(rowSums(G.mat) > 2)
    if (!is.null(opts)){
        if ('cf.name' %in% names(opts)){
            exclusive <- grepl('^exclusive', opts$cf.name)

            if (exclusive){
                for (z in names(Rhos.0)){
                    for (k in non.exclusive){
                        rzk <- Rhos.0[[z]][[k]]
                        Rhos.0[[z]][[k]][2:length(rzk)] <- 1000
                    }
                }
            }
        }
    }

    iter.dists <- c()
    Rhos.iter <- list()
    converged <- FALSE
    reverted  <- FALSE
    for (iter in 1:max.iter){
        # Compute the sales and derivatives for each zip code
        sales.dat <- restaurant.sales(dat.m, Rhos.0, sales.zips = sales.zips,
                                      loop.zips = NULL, scale.down = TRUE,
                                      opts = opts,
                                      keep.platforms = keep.platforms)
        Sales <- sales.dat$Sales
        Deriv <- sales.dat$Deriv

        # Compute updated rho vector for each portfolio
        Rhos.1 <- Rhos.0
        dists <- c()

        markups <- list()

        for (zip in menu.subset){
            # If there is only one restaurant in range of the zip code, the
            # optimal prices are not well defined. Skip the zip code.
            # Also skip if there are no restaurants in the ZCTA
            if (sum(Jp.G.1.m[[zip]]) <= 1 | (nres.z[zip] == 0)){
                Rhos.1[[zip]] <- Rhos.0[[zip]]
                dists[zip] <- 0
                next
            }

            if (test.mode) {
                mc.off.z <- median(mc.off)
                mc.on.z  <- median(mc.on)
            } else if (by.zip){
                mc.off.z <- mc.off[zip]
                mc.on.z  <- mc.on[zip]
            } else {
                mc.off.z <- mc.off
                mc.on.z  <- mc.on
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
            # Initialize updated menu prices for the zip code
            Rhos.1[[zip]]  <- list()
            markups[[zip]] <- list()
            # Initialize distances
            dists.z <- c()
            # Extract prices charged to restaurants by platforms
            R.z <- comm0[[zip]]

            for (g in 1:nportfolios){
                # Skip the portfolio if it includes platforms that have
                # been dropped
                if (!(g %in% keep.portfolios)){
                    dists.z[g] <- 0
                    Rhos.1[[zip]][[g]] <- Rhos.0[[zip]][[g]]
                    next
                }
                if (abolish & g > 1){
                    dists.z[g] <- 0
                    Rhos.1[[zip]][[g]] <- Rhos.0[[zip]][[g]]
                    next
                }

                if (exclusive & (g %in% non.exclusive)){
                    dists.z[g] <- 0
                    Rhos.1[[zip]][[g]] <- Rhos.0[[zip]][[g]]
                    next
                }
                # Skip the zip/portfolio pair if it includes no restaurants
                if (is.nan(weights.zg[[zip]][g])){
                    dists.z[g] <- 0
                    Rhos.1[[zip]][[g]] <- Rhos.0[[zip]][[g]]
                    next
                }
                if (weights.zg[[zip]][g] < 1e-10){
                    dists.z[g] <- 0
                    Rhos.1[[zip]][[g]] <- Rhos.0[[zip]][[g]]
                    next
                }

                # Extract platform members
                idx.g <- G.indices[[g]]
                NG <- length(idx.g)
                # Extract platforms' commission rates
                R.zg <- R.z[idx.g]
                # Determine costs
                if (length(idx.g) == 1){
                    mc.g <- mc.off.z
                } else {
                    mc.g <- c(mc.off.z, rep(mc.on.z, times = length(idx.g) - 1))
                }

                # Extract relevant matrices
                s.0     <- matrix(S.z[g, idx.g], ncol = 1)
                s.tilde <- s.0*matrix(1 - R.zg, ncol = 1)

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

                # If PM is in the portfolio and has near 0 sales, remove it
                pm.in <- G.mat[g, ncol(G.mat)] == 1
                s.0.orig <- s.0
                if (pm.in){
                    pm.idx <- which(dat.m$G.indices[[g]] == ncol(G.mat))
                    pm.small <- s.0[pm.idx]/sum(s.0) < 0.005
                    if (pm.small){
                        keep.idx <- setdiff(1:nrow(s.0), pm.idx)
                        s.0     <- s.0[keep.idx, 1]
                        s.tilde <- s.tilde[keep.idx, 1]
                        Delta.0 <- Delta.0[keep.idx, keep.idx]
                        dvec <- dvec[keep.idx, 1]
                        pdiff <- pdiff[keep.idx]
                    }
                }

                # If Postmates is in the portfolio and has near zero sales,
                # remove it
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
                dist.z.g <-  tryCatch({
                    b <- -Delta.inv%*%(s.tilde - phi*dvec) + phi/2*pdiff^2

                    # Re-introduce postmates if it was dropped
                    if (pm.in){
                        if (pm.small){
                            rho.pm <- Rhos.0[[zip]][[g]][pm.idx]
                            b.all <- matrix(0, ncol = 1, nrow = NG)
                            b.all[keep.idx, 1] <- b
                            b.all[pm.idx, 1]   <- rho.pm*(1 - R.zg[pm.idx]) - mc.g[pm.idx]
                        } else {
                            b.all <- b
                        }
                    } else {
                        b.all <- b
                    }

                    rho.1 <- (b.all + mc.g)/(1 - R.zg)
                    Rhos.1[[zip]][[g]] <- as.numeric(rho.1)
                    # Compute distances
                    rho.diff.sq <- (rho.1 - Rhos.0[[zip]][[g]])^2
                    mean(rho.diff.sq)
                }, error = function(cond){
                    return(0)
                })
                dists.z[g] <- dist.z.g
            }
            # Compute distances weighted by restaurant counts
            dists[zip] <- sum(dists.z*weights.zg[[zip]])*weights.z[zip]
        }

        # Evaluate convergence
        dist <- sum(dists)
        iter.dists[iter] <- dist
        if (verbose){
            pracma::fprintf('\tMenu price iteration %d: distance = %f\n', iter, dist)
        }

        Rhos.iter[[iter]] <- Rhos.0

        if (is.nan(dist)){
            # Break out and choose the minimum-distance Rhos
            best.iter <- which.min(iter.dists)
            Rhos.1 <- Rhos.iter[[best.iter]]
            warning(sprintf(paste0('Restaurant price algorithm (NPP) did not converge ',
                                   '(NaN distance at iteration %d; reverting to iteration %d, ',
                                   'distance = %f). Investigate problem and subsequently ',
                                   're-run analysis.'),
                            iter, best.iter, iter.dists[best.iter]))
            reverted <- TRUE
            break
        } else if (dist >= max.dist){
            # Break out and choose the minimum-distance Rhos
            best.iter <- which.min(iter.dists)
            Rhos.1 <- Rhos.iter[[best.iter]]
            warning(sprintf(paste0('Restaurant price algorithm (NPP) did not converge ',
                                   '(distance %f exceeded max.dist at iteration %d; reverting ',
                                   'to iteration %d, distance = %f). Investigate problem and ',
                                   'subsequently re-run analysis.'),
                            dist, iter, best.iter, iter.dists[best.iter]))
            reverted <- TRUE
            break
        } else if (dist < tol){
            converged <- TRUE
            break
        } else {
            for (zip in menu.subset){
                for (g in keep.portfolios){
                    Rhos.0[[zip]][[g]] <- learn.rate*Rhos.1[[zip]][[g]] + (1 - learn.rate)*Rhos.0[[zip]][[g]]
                }
            }
        }
    }

    if (iter == max.iter & !converged & !reverted){
        # Choose the minimum-distance Rhos
        best.iter <- which.min(iter.dists)
        Rhos.1 <- Rhos.iter[[best.iter]]
        warning(sprintf(paste0('Restaurant price algorithm (NPP) did not converge ',
                               '(maximum iterations %d reached; reverting to iteration %d, ',
                               'distance = %f, tolerance = %f). Investigate problem and ',
                               'subsequently re-run analysis.'),
                        max.iter, best.iter, iter.dists[best.iter], tol))
    }

    if (return.markup){
        out <- list(Rhos = Rhos.1, markups = markups)
    } else {
        out <- Rhos.1
    }
    return(out)
}

add.offline <- function(comm){
    # Add offline commissions (of zero) to `comm`
    comm0 <- list()
    for (z in names(comm)){
        comm0[[z]] <- c(offline = 0, comm[[z]])
    }
    return(comm0)
}
