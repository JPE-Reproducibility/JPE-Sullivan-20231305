compute.eqm.feefirst <- function(eqm.objs.j, cf.name, eqm.opts, nsim.suffix,
                                 outdir,
                                 num.param       = NULL,
                                 fees            = NULL,
                                 C.init          = NULL,
                                 R.init          = NULL){
    # Extract data
    dat.m   <- eqm.objs.j$dat.m
    opts    <- eqm.objs.j$opts
    kappa   <- eqm.objs.j$kappa
    G.mat   <- dat.m$G.mat
    zips.m  <- names(dat.m$J.G.m)
    NF <- dat.m$nplatforms - 1
    keep.platforms <- eqm.opts$platforms

    # Apply CFs
    ## Caps
    twosided <- grepl('twosided[0-9]', cf.name)
    has.cap <- grepl('cap[0-9]', cf.name) | twosided
    if (has.cap){
        # When there is a cap, do not search for commission rates
        eqm.opts$impose.p.r <- TRUE
        cap.amt <- sub('(cap|twosided)', '', cf.name)
        cap.amt <- sub('[_a-zA-Z]+$', '', cap.amt)
        cap.amt <- as.numeric(cap.amt)/100
        R <- rep(cap.amt, times = NF)
    } else {
        # Start from observed commission
        comm.tab <- do.call(rbind, dat.m$comm)
        R <- colMeans(comm.tab)
    }
    ## Abolish
    abolish <- grepl('abolish', cf.name)
    if (abolish){
        # Lower consumer tastes for platforms
        dat.m$demand.param$psi[1:length(dat.m$demand.param$psi)] <- -50
    }
    ## Taxation
    if (grepl('tax', cf.name)){
        tax.rate <- as.numeric(sub('tax', '', cf.name))/1000
    } else {
        tax.rate <- 0.0
    }
    opts$tax.rate <- tax.rate
    ## Adjustment to fixed costs
    if (grepl('adjkappa_', cf.name)){
        kappa.adj <- sub('adjkappa_', '', cf.name)
        kappa.adj <- as.numeric(kappa.adj)/100

        zips.kappa <- names(kappa$K)
        for (zk in zips.kappa){
            kappa$K[[zk]] <- kappa$K[[zk]]*kappa.adj
        }
        # also adjust base levels for chain and independents
        for (r.type in names(kappa$K.base)){
            kappa$K.base[[r.type]] <- kappa$K.base[[r.type]]*kappa.adj
        }
    }
    if (grepl('socopt', cf.name) & eqm.opts$impose.p.r){
        stop('socopt (social optimum) and impose.p.r are not compatible')
    }

    ## More preliminaries
    opts$cf.name <- eqm.opts$cf.name
    opts$verbose <- FALSE

    # Find social optimum?
    eqm.opts$socopt <- grepl('socopt', cf.name)

    if (is.null(num.param)){
        num.param <- load.num.param()
        num.param$tol.FP   <- 2.5e-7
        num.param$tol.menu <- 1e-4
        num.param$max.iter.menu <- 30
        num.param$learn.rate.menu <- 0.25

        # Set some numerical parameters
        # Learning rates (diff for early iterations and later iterations)
        if (grepl('^sub1000_', cf.name)){
            num.param$LR.CR       <- 0.65
            num.param$LR.CR.early <- 0.65
        } else if (eqm.opts$socopt){
            num.param$LR.CR       <- 0.45
            num.param$LR.CR.early <- 0.45
        } else {
            num.param$LR.CR.early <- 0.75
            num.param$LR.CR       <- 0.45
        }
        # Uniform convergence tolerances across CFs (the tightest previously
        # used), so that exhibits never compare equilibria solved to
        # different precision.
        num.param$tol.C <- 0.01
        num.param$tol.R <- 0.005
        num.param$max.iter.CR <- 100
    }

    # List of marginal costs
    pMC.df <- eqm.objs.j$pMC.df
    zips <- pMC.df$zip
    MC <- lapply(zips, function(z) as.numeric(pMC.df[z, paste0('mc', 1:NF)]))
    names(MC) <- zips
    eqm.objs.j$MC <- MC

    # Initialize fees
    fee.df <- do.call(rbind, dat.m$fees)
    C <- colMeans(fee.df)

    # Override if C.init is provided
    if (!is.null(C.init)){
        C <- C.init
    }
    # Do the same for commissions
    if (!is.null(R.init)){
        R <- R.init
    }

    if (twosided){
        if (is.null(fees)){
            stop('fees must be provided to compute.eqm.feefirst when twosided mode is on')
        }
        C <- fees
    } else if (has.cap){
        base.cap <- min(dat.m$caps.df$caps)
        mean.rho <- weight.rhos.v2(dat.m$eqm.weighting$Sales.tots,
                                   dat.m$eqm.weighting$Rhos, dat.m)
        rho.dd <- mean.rho[, 2]
        rho.dd <- mean(rho.dd[rho.dd > 0])
        C <- C + (base.cap - cap.amt)*rho.dd
    }

    if (eqm.opts$socopt){
        # Alternative starting values
        start.R <- 0.30
        R <- rep(start.R, length(R))
        MC.avg <- colMeans(pMC.df[, grep('^mc', colnames(pMC.df))])
        avg.rho <- 22
        C <- MC.avg - start.R*avg.rho
    }

    # Adjust kappa if the platforms are abolished
    if (abolish){
        for (z in names(kappa$K)){
            kappa$K[[z]][2:dat.m$nportfolios] <- 1
        }
        num.param$tol.FP <- 0.001
    }

    if (eqm.opts$no.multi){
        # Eliminate multihoming
        cat("Eliminating multihoming\n")
        kappa.alt <- kappa
        # Remove complementarities in platform adoption fixed costs
        types <- names(kappa$K.base)
        G.sub <- dat.m$G.mat[, 2:ncol(dat.m$G.mat)]
        NF <- ncol(G.sub)
        idx.multi <- which(rowSums(dat.m$G.mat) > 2)
        large.number <- 1 # recall that this is in hundreds of thousands
        for (tau in types){
            ## Assign a large cost to multihoming
            kappa.alt$K.base[[tau]][idx.multi] <- large.number
            if (tau == 'chain'){
                zip.chain <- grep('c', names(kappa.alt$K), value = TRUE)
                for (z in zip.chain){
                    kappa.alt$K[[z]][idx.multi] <- large.number
                }
            } else {
                zip.chain <- grep('i', names(kappa.alt$K), value = TRUE)
                for (z in zip.chain){
                    kappa.alt$K[[z]][idx.multi] <- large.number
                }
            }
        }
        kappa <- kappa.alt
    } else if (eqm.opts$no.comple){
        kappa.alt <- kappa
        # Remove complementarities in platform adoption fixed costs
        types <- names(kappa$K.base)
        G.sub <- dat.m$G.mat[, 2:ncol(dat.m$G.mat)]
        NF <- ncol(G.sub)
        for (tau in types){
            # Extract singlehoming fixed costs
            kappa.single <- c()
            for (f in 1:NF){
                idx <- which(G.sub[, f] == 1 & rowSums(G.sub) == 1)
                kappa.single[f] <- kappa$K.base[[tau]][idx]
            }
            # Obtain others by adding up singlehoming
            k.prime <- c()
            for (g in 1:nrow(G.sub)){
                idx.g <- which(G.sub[g, ] == 1)
                k.prime[g] <- sum(kappa.single[idx.g])
            }
            kappa.alt$K.base[[tau]] <- k.prime
            if (tau == 'chain'){
                zip.chain <- grep('c', names(kappa.alt$K), value = TRUE)
                for (z in zip.chain){
                    kappa.alt$K[[z]] <- k.prime
                }
            } else {
                zip.chain <- grep('i', names(kappa.alt$K), value = TRUE)
                for (z in zip.chain){
                    kappa.alt$K[[z]] <- k.prime
                }
            }
        }
        kappa <- kappa.alt
    }


    # Compute fee equilibrium
    if (twosided){
        ## load in fee and commissions
        for (z in names(dat.m$fees)){
            dat.m$fees[[z]] <- fees
        }
        for (z in names(dat.m$comm)){
            dat.m$comm[[z]] <- R
        }
        ## compute downstream equilibria
        FP <- find.fixed.point(kappa, dat.m, opts, num.param,
                               more.outputs = TRUE,
                               keep.platforms = keep.platforms)
        eqm <- list(C = fees, R = R, FP = FP)
    } else if (!abolish){
        # Platforms exist
        eqm <- FOC.iteration.feefirst(dat.m, kappa, MC, opts,
                                      num.param, eqm.opts, C, R,
                                      pMC.df)
    } else {
        # No pricing equilibrium
        FP <- find.fixed.point(kappa, dat.m, opts, num.param,
                               more.outputs = TRUE,
                               keep.platforms = keep.platforms)

        NF <- dat.m$nplatforms - 1
        eqm <- list(C = rep(NA, times = NF),
                    R = rep(NA, times = NF),
                    FP = FP)
    }

    return(eqm)
}

FOC.iteration.feefirst <- function(dat.m, kappa, MC, opts, num.param,
                                   eqm.opts, C, R, pMC.df){
    # Use an FOC approach to determine the pricing equilibrium

    socopt <- eqm.opts$socopt

    # Find endogenous commissions or fix commissions at the level in R?
    impose.p.r <- eqm.opts$impose.p.r
    NF <- dat.m$nplatforms - 1
    # Numerical differentiation step sizes
    h.c <- 0.01
    h.r <- 0.00025

    # Learning rates (diff for early iterations and later iterations)
    learn.rate.early <- num.param$LR.CR.early
    learn.rate       <- num.param$LR.CR

    # Tolerances
    tol.C    <- num.param$tol.C
    tol.R    <- num.param$tol.R
    max.iter <- num.param$max.iter.CR

    C0 <- C
    R0 <- R

    keep.platforms <- eqm.opts$platforms

    pm.iterate <- TRUE
    converged  <- FALSE
    for (iter in 1:max.iter){
        if (iter < 5){
            LR <- learn.rate.early
        } else {
            LR <- learn.rate
        }
        # Ensure that the correct fees and commissions are loaded
        for (z in names(dat.m$comm)){
            dat.m$comm[[z]] <- R
        }
        for (z in names(dat.m$fees)){
            dat.m$fees[[z]] <- C
        }
        # Compute baseline equilibrium
        FP0 <- find.fixed.point(kappa, dat.m, opts, num.param,
                                keep.platforms = keep.platforms,
                                more.outputs = TRUE)
        for (zz in intersect(names(dat.m$J.G.1.m), names(FP0$J.G.1))){
            dat.m$J.G.1.m[[zz]] <- FP0$J.G.1[[zz]]
        }
        if (socopt){
            # Consumer welfare
            CW <- compute.consumer.welfare(dat.m, C, dat.m$J.G.1.m, FP0$Rhos, no.logit = FALSE)
            FP0$CW <- CW$total.EU.dollar
            # Platform profits
            pl.profit <- platform.profits(FP0, C, R, kappa, dat.m,
                                          pMC.df, opts, num.param,
                                          keep.platforms = keep.platforms)
            FP0$pl.profit <- pl.profit
        }

        # Convergence safeguard: stop iterating on Postmates' fee once its
        # market share falls below 0.5%; at negligible share its FOC is
        # ill-conditioned and the fee no longer affects the equilibrium
        # materially.
        if (keep.platforms[4] == 1 & pm.iterate){
            pm.idx <- length(FP0$Sales.platform)
            pm.shr <- FP0$Sales.platform[pm.idx]/(sum(FP0$Sales.platform[2:pm.idx]))
            if (pm.shr < 0.005){
                pm.iterate <- FALSE
            }
        }

        iter.objs <- list()
        N.iter <- ifelse(pm.iterate, NF, NF - 1)
        for (f in 1:N.iter){
            if (keep.platforms[f] == 1){
                iter.objs[[f]] <- compute.iter.objs(f, C, R, dat.m, kappa, MC,
                                                    opts, num.param, FP0,
                                                    h.c, h.r, impose.p.r,
                                                    keep.platforms, pMC.df,
                                                    socopt,
                                                    max.joint = eqm.opts$max.joint)
            } else {
                iter.objs[[f]] <- c(c.f = C[f], r.f = R[f])
            }

        }
        update.CR <- do.call(rbind, iter.objs)

        ## retroactively delete postmates if its results are non-sensical
        if (keep.platforms[4] == 1 & pm.iterate){
            pm.fee  <- update.CR[nrow(update.CR), 1]
            bad.fee <- (pm.fee >= 25 | pm.fee < 0)

            if (!impose.p.r){
                pm.comm  <- update.CR[nrow(update.CR), 2]
                bad.comm <- (pm.comm >= 1.00 | pm.comm < 0)
            } else {
                bad.comm <- FALSE
            }

            if (bad.fee | bad.comm){
                ## Turn off postmates
                pm.iterate <- FALSE
                ## Re-run the iteration
                iter.objs <- list()
                N.iter <- ifelse(pm.iterate, NF, NF - 1)
                for (f in 1:N.iter){
                    if (keep.platforms[f] == 1){
                        iter.objs[[f]] <- compute.iter.objs(f, C, R, dat.m, kappa, MC,
                                                            opts, num.param, FP0,
                                                            h.c, h.r, impose.p.r,
                                                            keep.platforms, pMC.df,
                                                            socopt,
                                                            max.joint = eqm.opts$max.joint)
                    } else {
                        iter.objs[[f]] <- c(c.f = C[f], r.f = R[f])
                    }
                }
            }
            update.CR <- do.call(rbind, iter.objs)
        }

        C1 <- update.CR[, 1]
        if (!pm.iterate){
            C1 <- c(C1, C[4])
        }
        C  <- LR*C1 + (1 - LR)*C
        if (!impose.p.r){
            R1 <- update.CR[, 2]
            if (!pm.iterate){
                R1 <- c(R1, R[4])
            }
            R  <- LR*R1 + (1 - LR)*R
        }

        # never go below zero
        C <- pmax(C, 0)
        R <- pmax(R, 0)

        print(C)
        print(R)
        idx.dist <- 1:2 # only check the largest platforms
        dist.C <- sqrt(mean((C[idx.dist] - C0[idx.dist])^2))
        dist.R <- sqrt(mean((R[idx.dist] - R0[idx.dist])^2))
        pracma::fprintf('Iteration %d: dist.C, dist.R = %f, %f\n', iter, dist.C, dist.R)
        if (dist.C < tol.C & dist.R < tol.R){
            converged <- TRUE
            break
        } else {
            C0 <- C
            R0 <- R
        }
    }

    if (!converged){
        # On max.iter exhaustion, C0/R0 hold the post-update fees while FP0
        # was computed at the pre-update fees. Recompute the fixed point at
        # the returned fees so that out$C, out$R, and out$FP describe a
        # single equilibrium. (On the converged path FP0 already corresponds
        # to C0/R0, so no recomputation is needed.)
        warning(sprintf(paste0('FOC iteration hit max.iter = %d without ',
                               'converging (dist.C = %.4f, dist.R = %.4f); ',
                               'recomputing fixed point at returned fees'),
                        max.iter, dist.C, dist.R))
        for (z in names(dat.m$comm)){
            dat.m$comm[[z]] <- R0
        }
        for (z in names(dat.m$fees)){
            dat.m$fees[[z]] <- C0
        }
        FP0 <- find.fixed.point(kappa, dat.m, opts, num.param,
                                keep.platforms = keep.platforms,
                                more.outputs = TRUE)
        for (zz in intersect(names(dat.m$J.G.1.m), names(FP0$J.G.1))){
            dat.m$J.G.1.m[[zz]] <- FP0$J.G.1[[zz]]
        }
        if (socopt){
            CW <- compute.consumer.welfare(dat.m, C0, dat.m$J.G.1.m,
                                           FP0$Rhos, no.logit = FALSE)
            FP0$CW <- CW$total.EU.dollar
            pl.profit <- platform.profits(FP0, C0, R0, kappa, dat.m,
                                          pMC.df, opts, num.param,
                                          keep.platforms = keep.platforms)
            FP0$pl.profit <- pl.profit
        }
    }

    out <- list()
    out$C  <- C0
    out$R  <- R0
    out$FP <- FP0
    return(out)
}

compute.iter.objs <- function(f, C, R, dat.m, kappa, MC, opts, num.param,
                              FP0, h.c, h.r, impose.p.r, keep.platforms,
                              pMC.df, socopt = FALSE, max.joint = FALSE){
    # Compute objects used in fee iteration
    # f: platform in question
    # C: consumer fees
    # R: commissions
    # dat.m: data
    # kappa: restaurant fixed costs
    # max.joint: maximize joint platform profits?

    if (keep.platforms[f] == 0){
        stop('compute.iter.objs cannot execute for eliminated platform')
    }

    # Determine ZIPs under consideration
    zips <- intersect(names(FP0$Sales.tots), names(MC))

    # Ensure that the correct fees and commissions are loaded
    for (z in names(dat.m$fees)){
        dat.m$fees[[z]] <- C
    }
    for (z in names(dat.m$comm)){
        dat.m$comm[[z]] <- R
    }

    tax.rate <- opts$tax.rate
    if (is.null(tax.rate)){
        tax.rate <- 0
    }


    if (max.joint){
        joint.max.platforms <- which(keep.platforms == 1)
    } else {
        joint.max.platforms <- c(2, 4)
    }

    # Baseline quantities
    S0 <- compute.sales.by.zip(FP0)
    S.f <- S0[zips, f + 1]
    bar.rho0 <- weight.rhos.v2(FP0$Sales.tots, Rhos = FP0$Rhos, dat.m)
    rho.f <- bar.rho0[zips, f + 1]

    # Perturb fees
    C1 <- C
    C1[f] <- C1[f] + h.c

    # Perturb commissions
    R1 <- R
    R1[f] <- R1[f] + h.r

    # Load perturbed fees
    for (z in names(dat.m$fees)){
        dat.m$fees[[z]] <- C1
    }

    # Compute new downstream equilibrium with perturbed fees
    FP1 <- find.fixed.point(kappa, dat.m, opts, num.param,
                            keep.platforms = keep.platforms,
                            more.outputs = TRUE)
    S1 <- compute.sales.by.zip(FP1)
    bar.rho1 <- weight.rhos.v2(FP1$Sales.tots, Rhos = FP1$Rhos, dat.m)

    # Subset to ZIPs in MC data
    S.f1   <- S1[zips, f + 1]
    rho.f1 <- bar.rho1[zips, f + 1]

    # Compute sales and fee derivatives
    DRho <- (rho.f1 - rho.f)/h.c
    DS   <- (S.f1 - S.f)/h.c # derivative
    mc.net <- sapply(zips, function(z){
        MC[[z]][f] - (1 - tax.rate)*R[f]*bar.rho0[z, f + 1]
    })

    if (socopt){
        # Compute changes in consumer surplus and restaurant profits
        ## Consumer surplus
        CW1 <- compute.consumer.welfare(dat.m, C1, FP1$J.G.1, FP1$Rhos, no.logit = FALSE)
        DCW.dc <- (CW1$total.EU.dollar - FP0$CW)/h.c
        ## Restaurant profits
        rpi0 <- compute.rpi(dat.m, C,  R, FP0$J.G.1, kappa, FP0$Rhos, opts, keep.platforms)
        rpi1 <- compute.rpi(dat.m, C1, R, FP1$J.G.1, kappa, FP1$Rhos, opts, keep.platforms)

        Drpi.dc <- (rpi1['total'] - rpi0['total'])/h.c

        ## Profits of other platforms
        pl.profit1 <- platform.profits(FP1, C1, R, kappa, dat.m,
                                       pMC.df, opts, num.param,
                                       keep.platforms = keep.platforms)
        DLambda.dc <- (pl.profit1 - FP0$pl.profit)/h.c
        other.idx <- setdiff(1:length(DLambda.dc), f)
        DLambda.dc <- sum(DLambda.dc[other.idx])
        soc.effect <- DCW.dc + Drpi.dc + DLambda.dc
    } else {
        soc.effect <- 0
    }

    # Compute new fee
    num.a <- sum(mc.net*DS) - sum((1 + (1 - tax.rate)*R[f]*DRho)*S.f)
    Margins <- list()
    if (max.joint){
        # Maximize joint profits
        other.platforms <- setdiff(joint.max.platforms, f)
        adjustment.terms <- c()
        for (g in other.platforms){
            dSg.dcf   <- (S1[zips, g + 1] - S0[zips, g + 1])/h.c
            dRhog.dcf <- (bar.rho1[zips, g + 1] - bar.rho0[zips, g + 1])/h.c
            rho.g <- bar.rho0[zips, g + 1]
            S.g   <- S0[zips, g + 1]
            MC.g <- sapply(zips, function(z) MC[[z]][g])
            margin.g <- C[g] + (1 - tax.rate)*R[g]*rho.g - MC.g

            # Store results
            g.char <- as.character(g)
            Margins[[g.char]]        <- margin.g
            ## dPi_g/dc_f = margin_g*dS_g/dc_f + (1-tax)*R_g*S_g*dRho_g/dc_f
            adjustment.terms[g.char] <- sum(margin.g*dSg.dcf) +
                (1 - tax.rate)*R[g]*sum(dRhog.dcf*S.g)
        }
        num <- num.a - sum(adjustment.terms)
    } else if ((f %in% joint.max.platforms) & !socopt){
        # Incorporate effect of c.f on g's profits
        g <- setdiff(joint.max.platforms, f)
        if (keep.platforms[g] == 1){
            dSg.dcf   <- (S1[zips, g + 1] - S0[zips, g + 1])/h.c
            dRhog.dcf <- (bar.rho1[zips, g + 1] - bar.rho0[zips, g + 1])/h.c
            rho.g <- bar.rho0[zips, g + 1]
            S.g   <- S0[zips, g + 1]
            MC.g <- sapply(zips, function(z) MC[[z]][g])

            margin.g <- C[g] + (1 - tax.rate)*R[g]*rho.g - MC.g

            # Store results
            g.char <- as.character(g)
            Margins[[g.char]] <- margin.g
            ## dPi_g/dc_f = margin_g*dS_g/dc_f + (1-tax)*R_g*S_g*dRho_g/dc_f
            num <- num.a - sum(margin.g*dSg.dcf) -
                (1 - tax.rate)*R[g]*sum(dRhog.dcf*S.g)
        } else {
            # Partner platform has been abolished
            num <- num.a
        }
    } else if (socopt){
        # Account for social implications of a fee rise
        num <- num.a - soc.effect
    } else {
        # No joint ownership
        num <- num.a
    }
    den <- sum(DS)
    c.f1 <- num/den

    if (!impose.p.r){
        # Restore fees and load in perturbed commissions
        for (z in names(dat.m$fees)){
            dat.m$fees[[z]] <- C
        }
        for (z in names(dat.m$comm)){
            dat.m$comm[[z]] <- R1
        }
        # Compute new downstream equilibrium
        FP1 <- find.fixed.point(kappa, dat.m, opts, num.param,
                                keep.platforms = keep.platforms,
                                more.outputs = TRUE)

        # Compute derivatives
        bar.rho1 <- weight.rhos.v2(FP1$Sales.tots, Rhos = FP1$Rhos, dat.m)
        S1 <- compute.sales.by.zip(FP1)

        # Subset to ZIPs in MC data
        S.f1   <- S1[zips, f + 1]
        rho.f1 <- bar.rho1[zips, f + 1]

        DRho <- (rho.f1 - rho.f)/h.r
        DS   <- (S.f1 - S.f)/h.r # derivative
        mc.net <- sapply(zips, function(z) MC[[z]][f] - C[f])

        if (socopt){
            # Compute changes in consumer surplus and restaurant profits
            ## Consumer surplus
            CW1 <- compute.consumer.welfare(dat.m, C, FP1$J.G.1, FP1$Rhos, no.logit = FALSE)
            DCW.dr <- (CW1$total.EU.dollar - FP0$CW)/h.r
            ## Restaurant profits
            rpi0 <- compute.rpi(dat.m, C, R,  FP0$J.G.1, kappa, FP0$Rhos, opts, keep.platforms)
            rpi1 <- compute.rpi(dat.m, C, R1, FP1$J.G.1, kappa, FP1$Rhos, opts, keep.platforms)

            Drpi.dr <- (rpi1['total'] - rpi0['total'])/h.r

            ## Profits of other platforms
            pl.profit1 <- platform.profits(FP1, C, R1, kappa, dat.m,
                                           pMC.df, opts, num.param,
                                           keep.platforms = keep.platforms)
            DLambda.dr <- (pl.profit1 - FP0$pl.profit)/h.r
            other.idx <- setdiff(1:length(DLambda.dr), f)
            DLambda.dr <- sum(DLambda.dr[other.idx])
            soc.effect <- DCW.dr + Drpi.dr + DLambda.dr
        } else {
            soc.effect <- 0
        }

        num.a <- sum(mc.net*DS) - (1 - tax.rate)*sum((rho.f + R[f]*DRho)*S.f)
        if (max.joint){
            other.platforms <- setdiff(joint.max.platforms, f)
            adjustment.terms <- c()
            for (g in other.platforms){
                # Extract platform's margin on g
                g.char <- as.character(g)
                margin.g <- Margins[[g.char]]

                # Compute cross-commission effect
                ## dPi_g/dr_f = margin_g*dS_g/dr_f + (1-tax)*R_g*S_g*dRho_g/dr_f
                dSg.drf   <- (S1[zips, g + 1] - S0[zips, g + 1])/h.r
                dRhog.drf <- (bar.rho1[zips, g + 1] - bar.rho0[zips, g + 1])/h.r
                S.g <- S0[zips, g + 1]
                adjustment.terms[g.char] <- sum(margin.g*dSg.drf) +
                    (1 - tax.rate)*R[g]*sum(dRhog.drf*S.g)
            }
            num <- num.a - sum(adjustment.terms)

        } else if (f %in% joint.max.platforms & !socopt){
            # Incorporate effect of r.f on g's profits
            g <- setdiff(joint.max.platforms, f)
            if (keep.platforms[g] == 1){
                # Extract platform's margin on g
                g.char <- as.character(g)
                margin.g <- Margins[[g.char]]

                # Compute cross-commission effect
                ## dPi_g/dr_f = margin_g*dS_g/dr_f + (1-tax)*R_g*S_g*dRho_g/dr_f
                dSg.drf   <- (S1[zips, g + 1] - S0[zips, g + 1])/h.r
                dRhog.drf <- (bar.rho1[zips, g + 1] - bar.rho0[zips, g + 1])/h.r
                S.g <- S0[zips, g + 1]

                # Compute pricing numerator
                num <- num.a - sum(margin.g*dSg.drf) -
                    (1 - tax.rate)*R[g]*sum(dRhog.drf*S.g)
            } else {
                # Partner platform has been abolished
                num <- num.a
            }
        } else if (socopt){
            num <- num.a - soc.effect
        } else {
            num <- num.a
        }
        den <- (1 - tax.rate)*sum(rho.f*DS)
        r.f1 <- num/den

        out <- c(c.f = c.f1, r.f = r.f1)
    } else {
        out <- c.f1
    }

    return(out)
}
