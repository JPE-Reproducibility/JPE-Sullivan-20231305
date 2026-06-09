FC.GMM.v2 <- function(sigma.o, dat, EPi, demo,
                      verbose = FALSE,
                      return.K = FALSE, return.covs = TRUE,
                      sigma.rc = NULL, Mu.f = NULL,
                      delta.tol = 1e-4, delta.maxit = 1e5){
    # GMM moments for restaurant platform-adoption fixed-cost estimation.
    #
    # Returns either:
    #   K.vals  -- per-market chain/indep fixed cost vectors that match
    #              the predicted to the observed platform-portfolio shares
    #              (set return.K = TRUE), or
    #   Covs    -- a 2x2 matrix of (data, model) covariance pairs for the
    #              two moments used to identify (sigma.omega, sigma.rc):
    #              row 1: cov(share_online, log_young_pop) -- g_{omega,1}
    #                     in the paper's appendix, matching share-online
    #                     adoption to the young-consumer demand proxy.
    #              row 2: cov(avg_n,        log_young_pop) -- g_{omega,2}
    #                     in the paper's appendix, matching average number
    #                     of platforms joined to the same demand proxy.
    #              log_young_pop = log(pop * share_young_range), where
    #              pop is local ZIP population (from data/geo/geo_with_zctas.csv)
    #              and share_young_range is the Numerator-panel transaction-
    #              weighted share of under-35 unmarried consumers nearby.

    kappa <- list()
    kappa$sigma.o <- sigma.o
    if (!is.null(sigma.rc)){
        kappa$sigma.rc <- sigma.rc
    }

    nportfolios <- dat[[1]]$nportfolios
    G.vars      <- grep('^G[01]', colnames(dat[[1]]$resto.m), value = TRUE)
    markets     <- names(EPi)

    delta.update <- sigma.o

    K.vals <- list()

    for (market in markets){
        dat.m   <- dat[[market]]
        resto.m <- combine.resto.data(dat.m$resto.c.m, dat.m$resto.i.m)
        EPi.m   <- EPi[[market]]

        resto.m.sub <- resto.m[which(resto.m$zip %in% names(EPi.m)), ]
        ## Disaggregate by restaurant type
        idx.c <- grep('c$', resto.m.sub$zip)
        idx.i <- grep('i$', resto.m.sub$zip)
        agg.c <- colSums(resto.m.sub[idx.c, G.vars])
        ## Floor chain portfolio counts at 10: an empty portfolio cell would
        ## give log(0) = -Inf in the fixed-cost update below
        ## (log(model.shares) - log(data.shares)), so the contraction would
        ## diverge for never-chosen portfolios.
        agg.c[agg.c <= 10] <- 10
        agg.i <- colSums(resto.m.sub[idx.i, G.vars])
        data.shares.c <- agg.c/sum(agg.c)
        data.shares.i <- agg.i/sum(agg.i)

        zips  <- names(EPi.m)
        nres.m <- resto.m$J_total
        names(nres.m) <- resto.m$zip

        # Fill in deterministic part of fixed costs
        K.base.c <- rep(0, times = nportfolios)
        K.base.i <- rep(0, times = nportfolios)

        idx1 <- 2:nportfolios
        K <- list()
        for (z in zips){
            if (grepl('c$', z)){
                K[[z]] <- K.base.c
            } else if (grepl('i$', z)){
                K[[z]] <- K.base.i
            }
        }

        for (iter in 1:delta.maxit){
            # Compute counts implied by Ks
            kappa$K <- K
            J.1.mat <- generate.predicted.counts(EPi.m, kappa, nres.m, G.vars, Mu.f = Mu.f)
            idx.c <- grep('c$', J.1.mat$zip)
            idx.i <- grep('i$', J.1.mat$zip)
            J.1.agg.c <- colSums(J.1.mat[idx.c, G.vars])
            J.1.agg.i <- colSums(J.1.mat[idx.i, G.vars])

            model.shares.c <- J.1.agg.c/sum(J.1.agg.c)
            model.shares.i <- J.1.agg.i/sum(J.1.agg.i)

            dist.c <- sqrt(sum((model.shares.c - data.shares.c)^2))
            dist.i <- sqrt(sum((model.shares.i - data.shares.i)^2))
            dist   <- sqrt(sum((model.shares.c - data.shares.c)^2) +
                           sum((model.shares.i - data.shares.i)^2))

            # Update Ks
            update.term.c <- log(model.shares.c[idx1]) - log(data.shares.c[idx1])
            K.base.c[idx1] <- K.base.c[idx1] + delta.update*update.term.c

            update.term.i <- log(model.shares.i[idx1]) - log(data.shares.i[idx1])
            K.base.i[idx1] <- K.base.i[idx1] + delta.update*update.term.i

            K <- list()
            for (z in zips){
                if (grepl('c$', z)){
                    K[[z]] <- K.base.c
                } else if (grepl('i$', z)){
                    K[[z]] <- K.base.i
                }
            }

            if (verbose) pracma::fprintf('Iteration %d: %f (chain = %f, indep = %f)\n',
                                         iter, dist, dist.c, dist.i)

            if (dist < delta.tol){
                break
            }
        }

        K.vals[[market]] <- list(chain = K.base.c,
                                 indep = K.base.i)

        # Predict shares under the converged Ks so we can compute moment-2 covariances.
        kappa$K <- K
        demo.sim.m <- generate.predicted.counts(EPi.m, kappa, nres.m, G.vars, Mu.f = Mu.f)
        demo.sim.m$J_total <- rowSums(demo.sim.m[, G.vars])
        demo.sim.m$shr_online_model <- 1 - demo.sim.m$G0000/demo.sim.m$J_total
        demo.sim.m$avg_n_model      <- compute.average.n.platforms(demo.sim.m)

        keep.vars <- c('zip', 'shr_online_model', 'avg_n_model')
        demo[[market]] <- dplyr::left_join(demo[[market]], demo.sim.m[, keep.vars], by = 'zip')
    }

    if (return.K){
        return(K.vals)
    }

    # Pool markets and compute the two identifying covariance moments.
    demo.all <- as.data.frame(data.table::rbindlist(demo))
    demo.all$log_young_pop <- log(demo.all$pop * demo.all$share_young_range)
    idx <- which(is.finite(demo.all$log_young_pop))

    cov.dat.online <- cov.wt(demo.all[idx, c('share_online',     'log_young_pop')],
                             wt = demo.all$J_total[idx])$cov[1, 2]
    cov.sim.online <- cov.wt(demo.all[idx, c('shr_online_model', 'log_young_pop')],
                             wt = demo.all$J_total[idx])$cov[1, 2]
    cov.dat.avgn   <- cov.wt(demo.all[idx, c('avg_n',            'log_young_pop')],
                             wt = demo.all$J_total[idx])$cov[1, 2]
    cov.sim.avgn   <- cov.wt(demo.all[idx, c('avg_n_model',      'log_young_pop')],
                             wt = demo.all$J_total[idx])$cov[1, 2]

    Covs <- matrix(c(cov.dat.online, cov.sim.online,
                     cov.dat.avgn,   cov.sim.avgn),
                   ncol = 2, byrow = TRUE,
                   dimnames = list(c('online', 'avgn'), c('data', 'sim')))
    return(Covs)
}
