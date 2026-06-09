library(Matrix)
library(EconTools)
library(FoodDeliveryTools)
library(wesanderson)

source('code/CF/welfare_calculations.R')

main <- function(){
    # Specify cap levels to study
    cap.levels <- 15:40

    # Specify paths
    nsim.suffix <- '_nsim50'
    base.dir <- sprintf('output/CF_feefirst/spec%s', nsim.suffix)
    outdir <- sprintf('output/CF_feefirst/analysis%s', nsim.suffix)
    outpath.all <- sprintf('%s/twosided_all_markets.pdf', outdir)

    ## Fees, share of orders on platforms,
    outpath.fee   <- sprintf('%s/fee_by_cap_level-twosided.pdf',    outdir)
    outpath.J     <- sprintf('%s/Jshr_by_cap_level-twosided.pdf',   outdir)
    outpath.sales <- sprintf('%s/Sratio_by_cap_level-twosided.pdf', outdir)
    outpath.compare.benefits <- sprintf('%s/compare_variety_benefits.pdf', outdir)
    # Load cbsa codes
    inpaths    <- collect.inpaths(nsim.suffix)
    cbsa.codes <- read.dat(inpaths$inpath.cbsa.codes)

    # Load fixed cost estimates
    FC.est <- readRDS(inpaths$inpath.FC.est)

    # Load eqm objects
    load.pMC <- TRUE
    eqm.objs <- load.all.eqm.objs.feefirst(nsim.suffix, load.pMC)

    eqm.objs.co <- eqm.objs$county
    markets <- names(eqm.objs.co)

    # Load two-sided results
    BL.pattern <- 'cap30'
    BL <- load.eqm.results(BL.pattern, base.dir, cbsa.codes, suffix = '_take2')
    assert.complete.names(names(BL), markets,
                          what = sprintf('cap30_take2 baseline results in %s', base.dir),
                          hint = 'Run code/CF/run_all_markets_cap.R first.')
    Cap <- list()
    for (k in 1:length(cap.levels)){
        lvl <- cap.levels[k]
        cap.pattern <- sprintf('twosided%d', lvl)
        Cap[[k]] <- load.eqm.results(cap.pattern, base.dir, cbsa.codes, suffix = '')
        assert.complete.names(names(Cap[[k]]), markets,
                              what = sprintf('twosided%d results in %s', lvl, base.dir),
                              hint = 'Run code/CF/two_sided_cap.R (full 15:40 grid) first.')
    }

    # Load one-sided results
    OS  <- list()
    for (k in 1:length(cap.levels)){
        lvl <- cap.levels[k]
        cap.pattern <- sprintf('cap%d', lvl)
        OS[[k]]  <- load.eqm.results(cap.pattern, base.dir, cbsa.codes, suffix = '_take2')
        assert.complete.names(names(OS[[k]]), markets,
                              what = sprintf('cap%d_take2 results in %s', lvl, base.dir),
                              hint = 'Run code/CF/run_all_markets_cap.R first.')
    }

    # Compute changes for the level (completeness of `markets` is asserted
    # at load time above)
    Results <- list()

    for (market in markets){
        eqm.objs.m <- eqm.objs.co[[market]]
        counties <- names(eqm.objs.m)

        Results.BL  <- list()
        Results.cap <- list()

        for (co in counties){

            eqm.objs.j <- eqm.objs.m[[co]]

            dat.m  <- eqm.objs.j$dat.m
            opts   <- eqm.objs.j$opts
            pMC.df <- eqm.objs.j$pMC.df

            eqm.BL  <- BL[[market]][[co]]

            # Process baseline equilibrium
            Results.BL[[co]] <- welfare.for.CF(dat.m, market, FC.est, eqm.BL, opts, pMC.df)

            # Process commission cap results
            Results.cap[[co]] <- list()
            for (k in 1:length(cap.levels)){
                eqm <- Cap[[k]][[market]][[co]]
                Results.cap[[co]][[k]] <- welfare.for.CF(dat.m, market, FC.est, eqm, opts, pMC.df)
            }
        }

        # Store results
        Results[[market]] = list(BL = Results.BL, cap = Results.cap)
    }


    # Produce plot
    w.dat <- list()
    for (market in names(Results)){
        Results.m <- Results[[market]]
        code.m <- cbsa.codes$cbsa[which(cbsa.codes$CBSA_name == market)]
        outpath <- sprintf('%s/twosided_%s.pdf', outdir, code.m)
        w.dat[[market]] <- make.vary.cap.plot(Results.m, cap.levels, outpath,
                                              breakeven.line = TRUE)
    }

    # Aggregate plot
    add.vars <- c('CW', 'PP', 'RP', 'tot', 'S')
    w.dat.sub <- list()
    for (market in markets){
        w.dat.sub[[market]] <- w.dat[[market]][, add.vars]
    }
    all.markets <- Reduce('+', lapply(w.dat.sub, as.matrix))
    all.markets <- as.data.frame(all.markets)
    all.markets$cap <- w.dat[[1]]$cap
    vary.cap.data.to.plot(all.markets, outpath.all, breakeven.line = TRUE)

    # Effects on various observable outcomes
    Fees <- list()
    S0   <- list()
    S1   <- list()
    J0   <- list()
    J1   <- list()
    J.sh <- list()

    idx.1 <- 2:(eqm.objs.co[[1]][[1]]$dat.m$nplatforms)
    for (market in markets){
        eqm.objs.m <- eqm.objs.co[[market]]
        counties <- names(eqm.objs.m)

        Results.BL  <- list()
        Results.cap <- list()

        for (co in counties){
            eqm.objs.j <- eqm.objs.m[[co]]

            dat.m  <- eqm.objs.j$dat.m
            opts   <- eqm.objs.j$opts
            pMC.df <- eqm.objs.j$pMC.df

            # Process commission cap results
            Fees[[co]] <- c()
            S1[[co]]   <- c()
            S0[[co]]   <- c()
            J.sh[[co]] <- c()
            for (k in 1:length(cap.levels)){
                eqm <- Cap[[k]][[market]][[co]]
                S1.k <- eqm$FP$Sales.platform[idx.1]
                Fees[[co]][k] <- weighted.mean(eqm$C, S1.k)
                S1[[co]][k]   <- sum(S1.k)
                S0[[co]][k]   <- eqm$FP$Sales.platform[1]
                J.k <- do.call(rbind, eqm$FP$J.G.1)
                J.k <- colSums(J.k)
                J0[[co]][k] <- J.k[1]
                J1[[co]][k] <- sum(J.k) - J.k[1]
            }
        }
    }

    S1 <- Reduce('+', S1)
    S0 <- Reduce('+', S0)
    Fees <- Reduce('+', Fees)/length(Fees)
    J0   <- Reduce('+', J0)
    J1   <- Reduce('+', J1)

    J.shr <- J1/(J0 + J1)


    H <- 4
    W <- 4
    pdf(outpath.fee, height = H, width = W)
    plot(cap.levels, Fees, type = 'l', axes = FALSE,
         xlab = 'Regulated commission level (%)',
         ylab = 'Mean consumer fee ($)',
         lwd = 2, xlim = c(15, 40), ylim = c(3, 10))
    grid()
    axis(1)
    axis(2)
    abline(v = 30, lty = 2, col = 'grey40')
    dev.off()

    ylim <- c(floor(min(J.shr)*10)/10, ceiling(max(J.shr)*10)/10)
    pdf(outpath.J, height = H, width = W)
    plot(cap.levels, J.shr, type = 'l', axes = FALSE,
         xlab = 'Regulated commission level (%)',
         ylab = 'Share of restaurants online',
         lwd = 2, ylim = ylim, xlim = c(15, 40))
    grid()
    axis(1)
    axis(2)
    abline(v = 30, lty = 2, col = 'grey40')
    dev.off()

    y <- S1/(S0 + S1)
    ylim <- c(floor(min(y)*100)/100, ceiling(max(y)*100)/100)
    pdf(outpath.sales, height = H, width = W)
    plot(cap.levels, y, type = 'l', axes = FALSE,
         xlab = 'Regulated commission level (%)',
         ylab = 'Share of orders on platforms',
         lwd = 2, xlim = c(15, 40), ylim = ylim)
    grid()
    axis(1)
    axis(2)
    abline(v = 30, lty = 2, col = 'grey40')
    dev.off()

    # Compare consumer benefits from increased restaurant variety when fees are
    # low versus when fees are high

    # what exactly to compute? some ideas
    # - Consumer welfare impact of restaurants attracted to platforms
    #   by commission cap when (i) fees increase and (ii) fees are fixed

    k.base <- which(cap.levels == 30)
    k.star <- which(cap.levels == 15)

    # Initialize outputs
    welfare <- list()

    for (market in markets){
        eqm.objs.m <- eqm.objs.co[[market]]
        counties <- names(eqm.objs.m)

        for (co in counties){

            eqm.objs.j <- eqm.objs.m[[co]]
            dat.m  <- eqm.objs.j$dat.m
            opts   <- eqm.objs.j$opts

            eqm.BL  <- OS[[k.base]][[market]][[co]]
            eqm.cap <- OS[[k.star]][[market]][[co]]


            W.base  <- compute.consumer.welfare(dat.m, eqm.BL$C,  eqm.BL$FP$J.G.1,  eqm.BL$FP$Rhos, no.logit = FALSE)
            W.justC <- compute.consumer.welfare(dat.m, eqm.cap$C, eqm.BL$FP$J.G.1,  eqm.BL$FP$Rhos, no.logit = FALSE)
            W.justJ <- compute.consumer.welfare(dat.m, eqm.BL$C,  eqm.cap$FP$J.G.1, eqm.BL$FP$Rhos, no.logit = FALSE)
            W.both  <- compute.consumer.welfare(dat.m, eqm.cap$C, eqm.cap$FP$J.G.1, eqm.BL$FP$Rhos, no.logit = FALSE)

            welfare[[co]] <- c(base  = W.base$total.EU.dollar,
                               justC = W.justC$total.EU.dollar,
                               justJ = W.justJ$total.EU.dollar,
                               both  = W.both$total.EU.dollar)
        }
    }
    welfare.mat <- do.call(rbind, welfare)
    welfare.agg <- colSums(welfare.mat)

    # Number of online sales as a normalizing factor
    S.on <- c()
    for (market in markets){
        eqm.objs.m <- eqm.objs.co[[market]]
        counties <- names(eqm.objs.m)
        for (co in counties){
            S.on[co]  <- sum(OS[[k.base]][[market]][[co]]$FP$Sales.platform[2:5])
        }
    }
    nfac <- sum(S.on)

    diff.low.C  <- welfare.agg['justJ'] - welfare.agg['base']
    diff.high.C <- welfare.agg['both'] - welfare.agg['justC']

    rdiff.low.C  <- diff.low.C/nfac
    rdiff.high.C <- diff.high.C/nfac

    # Figure version
    pdf(outpath.compare.benefits, width = 5, height = 5)
    barplot(c(rdiff.low.C, rdiff.high.C), col = 'forestgreen',
            ylab = 'Consumer benefit from restaurant uptake of platforms ($/order)',
            xlab = 'Consumer fee increase?',
            names.arg = c('No', 'Yes'), ylim = c(0, 1.2))
    abline(h = 0)
    dev.off()
    # Table version


}

main()

