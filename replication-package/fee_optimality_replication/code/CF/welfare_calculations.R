welfare.calculations <- function(dat.m, market, opts, FC.est, pMC.df, eqm.BL, eqm.cap){

    # Extract results
    C0 <- eqm.BL$C
    C1 <- eqm.cap$C

    # Extract results
    R0 <- eqm.BL$R
    R1 <- eqm.cap$R

    J.G0 <- eqm.BL$FP$J.G.1
    J.G1 <- eqm.cap$FP$J.G.1

    Rhos0 <- eqm.BL$FP$Rhos
    Rhos1 <- eqm.cap$FP$Rhos

    zips.m <- names(J.G0)
    kappa <- construct.kappa(FC.est, market, zips.m)
    K <- kappa$K

    # Consumer welfare
    no.logit <- FALSE
    CW.0 <- compute.consumer.welfare(dat.m, C0, J.G0, Rhos0, no.logit = no.logit)
    CW.p <- compute.consumer.welfare(dat.m, C1, J.G0, Rhos1, no.logit = no.logit)
    CW.1 <- compute.consumer.welfare(dat.m, C1, J.G1, Rhos1, no.logit = no.logit)

    chg.partial <- CW.p$total.EU.dollar - CW.0$total.EU.dollar
    chg.full    <- CW.1$total.EU.dollar - CW.0$total.EU.dollar

    # Platform profits
    BL.profit <- platform.profits(eqm.BL$FP, eqm.BL$C, eqm.BL$R, kappa, dat.m,
                                  pMC.df, opts, num.param, more.outputs = TRUE)
    cap.profit <- platform.profits(eqm.cap$FP, eqm.cap$C, eqm.cap$R, kappa, dat.m,
                                   pMC.df, opts, num.param, more.outputs = TRUE)
    chg.pp <- sum(cap.profit$pl.profits) - sum(BL.profit$pl.profits)

    # Restaurant profits
    BL.rp  <- compute.rpi(dat.m, C0, R0, J.G0, kappa, Rhos0, opts)
    cap.rp <- compute.rpi(dat.m, C1, R1, J.G1, kappa, Rhos1, opts)

    chg.Jc <- cap.rp['chain'] - BL.rp['chain']
    chg.Ji <- cap.rp['indep'] - BL.rp['indep']
    chg.J  <- cap.rp['total'] - BL.rp['total']

    S0  <- sum(eqm.BL$FP$Sales.platform[2:5])
    Rev <- sum(BL.profit$pl.revenues)

    # Collate outputs
    names(chg.Jc) <- names(chg.Ji) <- names(chg.J) <- NULL
    out <- c(consumer.full = chg.full,
             consumer.part = chg.partial,
             chain = chg.Jc,
             indep = chg.Ji,
             restaurant = chg.J,
             platform = chg.pp,
             S0 = S0,
             Rev = Rev)
    return(out)
}

welfare.for.CF <- function(dat.m, market, FC.est, eqm, opts, pMC.df, kappa = NULL){

    C   <- eqm$C
    R   <- eqm$R
    J.G <- eqm$FP$J.G.1

    Rhos <- eqm$FP$Rhos

    zips.m <- names(J.G)
    if (is.null(kappa)){
        kappa <- construct.kappa(FC.est, market, zips.m)
    }
    K <- kappa$K

    # Consumer welfare
    no.logit <- FALSE
    CW <- compute.consumer.welfare(dat.m, C, J.G, Rhos, no.logit = no.logit)

    # Platform profits
    profit.objs <- platform.profits(eqm$FP, C, R, kappa, dat.m,
                                    pMC.df, opts, num.param, more.outputs = TRUE)
    p.profit <- profit.objs$pl.profits

    # Restaurant profits
    r.profit <- compute.rpi(dat.m, C, R, J.G, kappa, Rhos, opts)

    # Sales and revenue
    S   <- sum(eqm$FP$Sales.platform[2:5])
    Rev <- sum(profit.objs$pl.revenues)

    out <- list(p.profit = p.profit,
                r.profit = r.profit,
                CW = CW,
                S = S,
                Rev = Rev)
    return(out)
}
