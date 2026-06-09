# Invert platforms' fee-setting first-order conditions, county by county, to
# recover platform marginal costs. Writes
# output/estimate_platform_costs/MC_H/spec*/<cbsa>_full.rds, consumed by
# describe_pMC.R (Table 5) and load.pMC.by.county() (the CF pipeline).

library(Matrix)
library(FoodDeliveryTools)
library(EconTools)
library(pracma)
library(parallel)

main <- function(){
    nsim.suffix <- '_nsim50'
    outdir <- 'output/estimate_platform_costs'
    create.dir(outdir)
    outdir <- paste0(outdir, '/MC_H')
    create.dir(outdir)
    outdir <- sprintf('%s/spec%s', outdir, nsim.suffix)
    create.dir(outdir)


    load.pMC <- FALSE
    eqm.objs <- load.all.eqm.objs.feefirst(nsim.suffix, load.pMC)

    eqm.objs.co <- eqm.objs$county
    markets <- names(eqm.objs.co)

    inpath.codes <- collect.inpaths(nsim.suffix)$inpath.cbsa.codes
    cbsa.codes <- read.dat(inpath.codes)

    for (market in markets){
        eqm.objs.m <- eqm.objs.co[[market]]
        results.m <- list()
        counties <- names(eqm.objs.m)

        for (co in counties){
            print(co)
            eqm.objs.j <- eqm.objs.m[[co]]
            results.m[[co]] <- compute.MC.H(eqm.objs.j)
        }

        # Combine county-level results into market-level matrices.
        # MC.no.h: marginal costs from the joint Uber Eats / Postmates inversion
        # (no weight on restaurant profits); Mu: the corresponding markups.
        MC.no.h <- lapply(results.m, function(x) do.call(cbind, x$MC.joint))
        MC.no.h <- do.call(rbind, MC.no.h)
        Mu <- lapply(results.m, function(x) do.call(cbind, x$Mu.joint))
        Mu <- do.call(rbind, Mu)

        # Collate outputs
        outputs <- list()
        outputs$MC.no.h <- MC.no.h
        outputs$Mu      <- Mu
        outputs$by.co   <- results.m

        # Determine output path
        cbsa.m <- cbsa.codes$cbsa[which(cbsa.codes$CBSA_name == market)]
        outpath <- sprintf('%s/%s_full.rds', outdir, cbsa.m)

        # Save outputs
        saveRDS(outputs, outpath)
    }
}

compute.MC.H <- function(eqm.objs.j){
    # Invert platforms' fee-setting first-order conditions (ignoring any weight
    # on restaurant profits) to recover marginal costs and markups, including
    # the joint Uber Eats / Postmates ownership case.

    # Extract elements
    dat.m  <- eqm.objs.j$dat.m
    opts   <- eqm.objs.j$opts
    opts$verbose <- FALSE
    kappa  <- eqm.objs.j$kappa

    num.param <- load.num.param()

    # Set tight tolerances
    num.param$tol.menu <- 0.01
    num.param$tol.FP   <- 1e-5

    # Compute the baseline equilibrium
    FP0 <- find.fixed.point(kappa, dat.m, opts, num.param, more.outputs = TRUE)

    ## Store original fixed point
    for (z in names(FP0$J.G.1)){
        dat.m$J.G.1.m[[z]] <- FP0$J.G.1[[z]]
    }
    fees <- dat.m$fees
    fzips <- intersect(names(fees), names(FP0$Sales.tots))

    # The inversion is not well behaved for very small ZIPs
    # cut ZIPs with few buyers
    min.buyer <- 250
    dat.agg <- aggregate(count ~ zip, data = dat.m$buy.m, FUN = sum)
    dat.agg <- dat.agg[which(dat.agg$count >= min.buyer/1e5), ]
    fzips <- intersect(fzips, dat.agg$zip)

    # Compute baseline consumer fees and restaurant commissions
    C.R     <- compute.C.R(fzips, dat.m)
    C       <- C.R$C
    tilde.r <- C.R$tilde.r

    # Derivatives of equilibrium objects with respect to consumer fees
    DC <- compute.dc.objs(fzips, dat.m, FP0, opts, num.param, kappa)

    # Invert FOCs for marginal costs and markups
    only.C <- invert.only.MCs(fzips, C, tilde.r, FP0, DC, dat.m)

    out <- list(MC.no.h  = only.C$MC,
                Mu.no.h  = only.C$Mu,
                MC.joint = only.C$MC.joint,
                Mu.joint = only.C$Mu.joint)
    return(out)
}

compute.C.R <- function(fzips, dat.m){
    # Compute consumer fees and restaurant commissions in baseline
    NF <- dat.m$nplatforms - 1
    C <- list()
    # Fees
    for (f in 1:NF){
        c.f <- sapply(fzips, function(z) dat.m$fees[[z]][f])
        C[[f]] <- c.f
    }
    # Commissions
    c.df <- dat.m$caps.df
    tilde.r <- sapply(fzips, function(z) c.df$caps[which(c.df$zip == z)])
    tilde.r <- matrix(tilde.r, ncol = 1)

    outputs <- list(C = C, tilde.r = tilde.r)
    return(outputs)
}

compute.sales.by.zip <- function(FP){
    S <- FP$Sales.tots
    S.by.zip <- lapply(S, function(x) apply(x, 2, sum))
    S.by.zip <- Reduce(rbind, S.by.zip)
    rownames(S.by.zip) <- names(S)
    return(S.by.zip)
}

compute.dc.objs <- function(fzips, dat.m, FP0, opts, num.param,
                            kappa, use.parallel = TRUE){
    # Compute derivatives of equilibrium objects (platform sales by ZIP and the
    # average price rho) with respect to consumer fees

    # Preliminaries
    NZ   <- length(fzips)
    NF   <- dat.m$nplatforms - 1
    fees <- dat.m$fees

    # Step size for numerical differentiation
    h.grid <- c(0.10, 0.10, 0.10, 1.00) # higher step required for smaller platform 4

    # Baseline objects
    S.by.zip0 <- compute.sales.by.zip(FP0)
    rho.bar0 <- weight.rhos.v2(FP0$Sales.tots, FP0$Rhos, dat.m)

    # Initialize outputs
    Delta <- list()
    DRho  <- list()
    Cross <- list()

    if (use.parallel){
        # Initialize cluster
        ncores <- parallel::detectCores() - 1
        cl <- parallel::makeCluster(ncores)
        parallel::clusterEvalQ(cl, library(Matrix))
        parallel::clusterEvalQ(cl, library(FoodDeliveryTools))
        parallel::clusterEvalQ(cl, library(EconTools))
        parallel::clusterEvalQ(cl, library(pracma))
    }

    # Loop over platforms
    for (f in 1:NF){
        h <- h.grid[f]

        Delta.c.f <- matrix(0, nrow = NZ, ncol = NZ)
        DRho.f    <- matrix(0, nrow = NZ, ncol = 1)

        # For Uber Eats and Postmates (common ownership)
        Delta.cross <- matrix(0, nrow = NZ, ncol = NZ)


        if (use.parallel){
            parallel::clusterExport(cl, c('compute.sales.by.zip',
                                          'S.by.zip0', 'rho.bar0',
                                          'f', 'dat.m', 'kappa', 'opts',
                                          'num.param', 'fees', 'h',
                                          'fzips'), envir = environment())
        }
        fn <- function(j){
            z <- fzips[j]
            fees1 <- fees
            fees1[[z]][f] <- fees1[[z]][f] + h
            dat.m$fees <- fees1
            FP.prime <- find.fixed.point(kappa, dat.m, opts, num.param,
                                         more.outputs = TRUE)
            dat.m$fees <- fees
            S.by.zip1 <- compute.sales.by.zip(FP.prime)
            rho.bar1 <- weight.rhos.v2(FP.prime$Sales.tots,
                                       FP.prime$Rhos, dat.m)
            # Delta^c
            S.deriv <- (S.by.zip1 - S.by.zip0)/h
            # Derivative of bar rho
            rho.deriv <- (rho.bar1 - rho.bar0)/h

            # Subsetting and storage
            Delta.c.f.j <- S.deriv[fzips, f + 1]
            DRho.f.j    <- rho.deriv[z, f + 1]

            # Joint ownership
            if (f == 2){
                f.alt <- 4
                Delta.cross.j <- S.deriv[fzips, f.alt + 1]
            } else if (f == 4){
                f.alt <- 2
                Delta.cross.j <- S.deriv[fzips, f.alt + 1]
            } else {
                Delta.cross.j <- 0
            }
            out.j <- list(Delta.c.f.j, DRho.f.j, Delta.cross.j)
            return(out.j)
        }

        if (use.parallel){
            Out <- parLapply(cl, 1:NZ, fn)
        } else {
            Out <- lapply(1:NZ, fn)
        }

        for (j in 1:NZ){
            # Subsetting and storage
            Delta.c.f[j, ]   <- Out[[j]][[1]]
            DRho.f[j]        <- Out[[j]][[2]]
            Delta.cross[j, ] <- Out[[j]][[3]]
        }

        Delta[[f]] <- Delta.c.f
        DRho[[f]]  <- DRho.f
        Cross[[f]] <- Delta.cross
    }

    outputs <- list()
    outputs$Delta.c <- Delta
    outputs$DRho    <- DRho
    outputs$Cross   <- Cross

    if (use.parallel){
        stopCluster(cl)
    }

    return(outputs)
}

invert.only.MCs <- function(fzips, C, tilde.r, FP0, DC, dat.m){
    # Invert FOCs to obtain MCs, ignoring weight on restaurant profits

    S.by.zip0 <- compute.sales.by.zip(FP0)
    rho.bar0  <- weight.rhos.v2(FP0$Sales.tots, FP0$Rhos, dat.m)
    rho.bar0  <- rho.bar0[fzips, ]
    NF <- length(C)

    # Intialize outputs
    MC <- list() # Marginal costs
    Mu <- list() # Markups

    for (f in 1:NF){
        c.f    <- C[[f]]
        S.f    <- S.by.zip0[fzips, f + 1]
        Delta  <- DC$Delta.c[[f]]
        DRho   <- DC$DRho[[f]]
        markup <- -qr.solve(Delta, (1 + tilde.r*DRho)*S.f)
        mc     <- c.f + tilde.r*rho.bar0[, f + 1] - markup

        rownames(mc)     <- fzips
        rownames(markup) <- fzips

        MC[[f]] <- mc
        Mu[[f]] <- markup
    }

    # Compute joint profit maximization MCs for Uber Eats and Postmates
    ## Initialize joint profit maximization CFs
    MC.joint <- MC
    Mu.joint <- Mu
    ## Indicate platforms with joint profit maximization
    f <- 2
    g <- 4
    ## Extract sales
    S.f <- S.by.zip0[fzips, f + 1]
    S.g <- S.by.zip0[fzips, g + 1]
    ## Extract sales derivatives
    Delta.ff <- DC$Delta.c[[f]]
    Delta.gg <- DC$Delta.c[[g]]
    Delta.gf <- DC$Cross[[f]]
    Delta.fg <- DC$Cross[[g]]
    ## Construct Delta matrix
    Delta.upper <- cbind(Delta.ff, Delta.gf)
    Delta.lower <- cbind(Delta.fg, Delta.gg)
    ## Extract prices
    rho.f <- rho.bar0[fzips, f + 1]
    rho.g <- rho.bar0[fzips, g + 1]
    ## Extract price derivatives
    DR.f <- DC$DRho[[f]]
    DR.g <- DC$DRho[[g]]
    ## Pool together consumer fees, commission rates, and prices
    c.pool   <- matrix(c(C[[f]], C[[g]]), ncol = 1)
    r.pool   <- matrix(c(tilde.r, tilde.r), ncol = 1)
    rho.pool <- matrix(c(rho.f, rho.g), ncol = 1)
    ## Right-hand side
    RHS.upper <- -(1 + tilde.r*DR.f)*S.f
    RHS.lower <- -(1 + tilde.r*DR.g)*S.g

    # Scale up the bottom panel to avoid numerical problems
    scale.fac <- mean(diag(Delta.ff))/mean(diag(Delta.gg))
    Delta <- rbind(Delta.upper, Delta.lower*scale.fac)
    RHS   <- rbind(RHS.upper,   RHS.lower*scale.fac)

    Mu.pool <- qr.solve(Delta, RHS)
    MC.pool <- c.pool + r.pool*rho.pool - Mu.pool
    NZ <- nrow(Delta.gf)
    MC.joint[[2]] <- matrix(MC.pool[1:NZ],            ncol = 1)
    MC.joint[[4]] <- matrix(MC.pool[(NZ + 1):(2*NZ)], ncol = 1)
    Mu.joint[[2]] <- matrix(Mu.pool[1:NZ],            ncol = 1)
    Mu.joint[[4]] <- matrix(Mu.pool[(NZ + 1):(2*NZ)], ncol = 1)

    rownames(MC.joint[[2]]) <- rownames(MC.joint[[4]]) <- fzips
    rownames(Mu.joint[[2]]) <- rownames(Mu.joint[[4]]) <- fzips

    outputs <- list(MC       = MC,
                    Mu       = Mu,
                    MC.joint = MC.joint,
                    Mu.joint = Mu.joint)
    return(outputs)
}

main()
