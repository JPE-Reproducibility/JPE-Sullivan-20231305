compute.sales <- function(eqm, dat.m, keep.platforms = c(1, 1, 1, 1),
                          exclude.off = TRUE,
                          return.S.tot = FALSE,
                          return.S.by.z = FALSE){
    # Compute sales under the fees contained in the equilibrium object "eqm"
    #
    # Options
    #   exclude.off: drop direct sales
    #   return.S.tot: return sales by ZIP/platform portfolio/platform rather
    #       than by platform alone

    if (exclude.off & return.S.tot){
        stop('The exclude.off and return.S.tot options of compute.sales are incompatible')
    }
    if (exclude.off & return.S.by.z){
        stop('The exclude.off and return.S.by.z options of compute.sales are incompatible')
    }
    if (return.S.by.z & return.S.tot){
        stop('The return.S.by.z and return.S.tot options of compute.sales are incompatible')
    }

    C    <- eqm$C
    Rhos <- eqm$FP$Rhos
    J.G  <- eqm$FP$J.G.1

    for (z in names(dat.m$fees)){
        dat.m$fees[[z]] <- C
    }
    for (z in names(J.G)){
        dat.m$J.G.1.m[[z]] <- J.G[[z]]
    }

    S <- compute.zip.sales(dat.m, more.outputs = TRUE, Rhos = Rhos,
                           keep.platforms = keep.platforms)

    if (return.S.tot){
        S <- S$Sales.tots

        # Aggregate across the ZIPs in which the orders were placed
        consumer.zips <- names(S)
        resto.zips <- lapply(consumer.zips, function(z) dimnames(S[[z]])[[3]])
        resto.zips <- sort(unique(do.call(c, resto.zips)))

        S.by.zip <- list()
        for (rz in resto.zips){
            S.rz <- matrix(0, nrow = nrow(S[[1]]), ncol = ncol(S[[1]]))
            for (cz in consumer.zips){
                if (rz %in% dimnames(S[[cz]])[[3]]){
                    S.rz <- S.rz + S[[cz]][, , rz]
                }
            }
            S.by.zip[[rz]] <- S.rz
        }
        output <- S.by.zip
    } else if (return.S.by.z){
        S <- S$Sales.tots
        S.by.zip <- list()
        for (z in names(S)){
            S.by.zip[[z]] <- apply(S[[z]], 2, sum)
        }
        output <- S.by.zip
    } else {
        S <- S$Sales.platform
        if (exclude.off){
            S <- S[2:(dat.m$nplatforms)]
        }
        output <- S
    }

    return(output)
}

compute.sales.total <- function(C, R, dat.m, kappa, opts, num.param, keep.platforms = NULL,
                                return.FP = FALSE){
    # Compute sales, allowing for a response in J and Rho
    for (z in names(dat.m$fees)){
        dat.m$fees[[z]] <- C
    }
    for (z in names(dat.m$comm)){
        dat.m$comm[[z]] <- R
    }
    Sales <- find.fixed.point(kappa, dat.m, opts, num.param, more.outputs = TRUE, keep.platforms = keep.platforms)
    S <- compute.pl.sales(Sales$Sales.tots)

    if (return.FP){
        output <- list(S = S, FP = Sales)
    } else {
        output <- S
    }
    return(output)
}

compute.market.power <- function(eqm.objs.j, eqm, keep.platforms = c(1, 1, 1, 1), f.incl = 1:4){
    # Inverse semi-elasticity of demand
    f.incl <- intersect(which(keep.platforms == 1), f.incl)
    Deriv <- compute.fee.deriv(eqm.objs.j, eqm, keep.platforms, f.incl = f.incl)
    S0 <- compute.sales(eqm, eqm.objs.j$dat.m, keep.platforms)
    mu <- -S0[f.incl]/Deriv
    return(mu)
}

compute.fee.deriv <- function(eqm.objs.j, eqm, keep.platforms = c(1, 1, 1, 1), f.incl = 1:4){
    # Compute sales derivatives with respect to fees
    C <- eqm$C
    dat.m <- eqm.objs.j$dat.m
    S0 <- compute.sales(eqm, dat.m, keep.platforms = keep.platforms)

    h <- 1e-2
    Deriv <- c()
    f.incl <- intersect(which(keep.platforms == 1), f.incl)
    for (f in f.incl){
        C1 <- C
        C1[f] <- C1[f] + h
        eqm1 <- eqm
        eqm1$C <- C1
        S1 <- compute.sales(eqm1, dat.m, keep.platforms = keep.platforms)
        Deriv[f] <- (S1[f] - S0[f])/h
    }

    return(Deriv)
}

compute.total.power <- function(eqm.objs.j, eqm, keep.platforms = NULL){

    # Preliminaries
    ## Data
    dat.m <- eqm.objs.j$dat.m
    NF <- dat.m$nplatforms - 1
    kappa <- eqm.objs.j$kappa

    ## Options
    opts <- eqm.objs.j$opts
    opts$verbose <- FALSE
    ## Numerical parameters
    num.param <- load.num.param()
    num.param$tol.FP <- 1e-4

    # Compute sales
    C <- eqm$C
    R <- eqm$R
    S0 <- compute.sales.total(C, R, dat.m, kappa, opts, num.param, keep.platforms = keep.platforms,
                              return.FP = TRUE)
    FP0 <- S0$FP
    S0  <- S0$S

    # Baseline commission revenue
    all.profit <- platform.profits(FP0, eqm$C, eqm$R, kappa, dat.m,
                                   eqm.objs.j$pMC.df, opts, num.param, more.outputs = TRUE)
    revs <- all.profit$pl.revenues
    revs.R <- revs - FP0$Sales.platform[2:5]*eqm$C

    # Compute derivatives
    h <- 1e-2
    Deriv <- c()
    Deriv.rev <- c()
    for (f in 1:NF){
        C1 <- C
        C1[f] <- C1[f] + h
        S1 <- compute.sales.total(C1, R, dat.m, kappa, opts, num.param,
                                  keep.platforms = keep.platforms, return.FP = TRUE)
        FP1 <- S1$FP
        S1  <- S1$S
        Deriv[f] <- (S1[f] - S0[f])/h

        # Commission revenue
        all.profit <- platform.profits(FP1, C1, eqm$R, kappa, dat.m,
                                       eqm.objs.j$pMC.df, opts, num.param, more.outputs = TRUE)
        revs <- all.profit$pl.revenues
        revs.R1 <- revs - FP1$Sales.platform[2:5]*C1
        Deriv.rev[f] <- (revs.R1[f] - revs.R[f])/h
    }

    # Inverse semi-elasticity
    mu <- -S0/Deriv

    outputs <- list(mu = mu, Deriv.S = Deriv, Deriv.rev = Deriv.rev)
    return(outputs)
}

compute.diversion <- function(eqm.objs.j, eqm, keep.platforms = c(1, 1, 1, 1), f.incl = 1:4){
    ## Diversion ratios
    dat.m <- eqm.objs.j$dat.m
    NF <- dat.m$nplatforms - 1

    C <- eqm$C
    S0 <- compute.sales(eqm, dat.m, exclude.off = FALSE, keep.platforms = keep.platforms)

    h <- 1e-2

    D <- list()
    f.incl <- which(keep.platforms == 1)
    for (f in f.incl){
        C1 <- C
        C1[f] <- C1[f] + h
        eqm$C <- C1
        S1 <- compute.sales(eqm, dat.m, exclude.off = FALSE, keep.platforms = keep.platforms)

        # Compute diversion
        chg.all <- S1 - S0
        chg.f   <- S1[f + 1] - S0[f + 1]
        D[[f]] <- -chg.all/chg.f
    }
    D <- do.call(rbind, D)
    return(D)
}
