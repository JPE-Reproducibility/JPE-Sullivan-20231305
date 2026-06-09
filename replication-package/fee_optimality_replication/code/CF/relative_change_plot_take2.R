library(Matrix)
library(EconTools)
library(FoodDeliveryTools)
library(wesanderson)

source('code/CF/welfare_calculations.R')

main <- function(){

    nsim.suffix <- '_nsim50'

    # Specify paths
    outdir <- sprintf('output/CF_feefirst/analysis%s', nsim.suffix)
    create.dir(outdir)
    outpath <- sprintf('%s/welfare_barplot.pdf', outdir)
    outpath.ncumla <- sprintf('%s/welfare_barplot_noncumla.pdf', outdir)
    outpath.cw  <- sprintf('%s/c_welfare_effects.pdf', outdir)
    outpath.rpi <- sprintf('%s/rpi_profit_effects.pdf', outdir)
    outpath.rpi.sub <- sprintf('%s/rpi_profit_effects_sub.pdf', outdir)
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

    # Load results
    base.dir <- sprintf('output/CF_feefirst/spec%s', nsim.suffix)
    result.files <- list.files(base.dir)

    BL.paths  <- grep('cap30_[a-z]+.*take2\\.rds', result.files, value = TRUE)
    cap.paths <- grep('cap15_[a-z]+.*take2\\.rds', result.files, value = TRUE)

    BL.paths  <- sprintf('%s/%s', base.dir, BL.paths)
    cap.paths <- sprintf('%s/%s', base.dir, cap.paths)

    BL  <- lapply(BL.paths, readRDS)
    cap <- lapply(cap.paths, readRDS)

    ## Generate names for CF objects
    BL.codes  <- sub('.*cap30_', '', sub('_take2.rds', '', BL.paths))
    cap.codes <- sub('.*cap15_', '', sub('_take2.rds', '', cap.paths))

    BL.m  <- sapply(BL.codes,  function(x) cbsa.codes$CBSA_name[cbsa.codes$cbsa == x])
    cap.m <- sapply(cap.codes, function(x) cbsa.codes$CBSA_name[cbsa.codes$cbsa == x])

    names(BL)  <- BL.m
    names(cap) <- cap.m

    Results <- list()

    for (market in markets){
        eqm.objs.m <- eqm.objs.co[[market]]
        counties <- names(eqm.objs.m)

        results <- list()
        for (co in counties){

            eqm.objs.j <- eqm.objs.m[[co]]

            dat.m  <- eqm.objs.j$dat.m
            opts   <- eqm.objs.j$opts
            pMC.df <- eqm.objs.j$pMC.df

            eqm.BL  <- BL[[market]][[co]]
            eqm.cap <- cap[[market]][[co]]

            welfare.co <- welfare.calculations(dat.m, market, opts, FC.est,
                                               pMC.df, eqm.BL, eqm.cap)
            results[[co]] <- welfare.co
        }
        chg <- do.call(rbind, results)
        Results[[market]] <- chg
    }

    # Make a plot
    Results.all <- do.call(rbind, Results)
    Results.sum <- colSums(Results.all)
    w.terms <- c('consumer.full', 'restaurant', 'platform')
    Results.tot <- sum(Results.sum[w.terms])

    rel.chg <- c(Results.sum[w.terms], total = Results.tot)
    rel.chg <- rel.chg/Results.sum['S0']

    # Plot dimensions (width and height)
    W <- 9
    H <- 7

    ## Colours
    cols <- wesanderson::wes_palette("Cavalcanti1", n = 4)
    ## Cumulative changes
    Y <- cumsum(rel.chg[1:3])
    Y <- c(0, Y)

    # Half of rectangle width
    rw <- 0.25

    add.amount <- 0.1

    labels <- c('Consumers', 'Restaurants', 'Platforms')
    X <- 1:4
    xlim <- c(1 - rw, 4 + rw)
    labels <- c(labels, 'Total')

    Y.for.lim <- Y
    ylim <- c(min(Y.for.lim, na.rm = TRUE) - add.amount,
              max(Y.for.lim, na.rm = TRUE) + add.amount)
    ylab <- 'Welfare change ($/platform order in baseline)'

    pdf(outpath, width = W, height = H)
    par(mar = c(4, 5, 1, 1))
    plot(x = X, axes = FALSE, col = 'white',
         xlim = xlim, ylim = ylim, ylab = ylab,
         xlab = '', cex.lab = 1.9)
    grid()
    abline(h = 0)

    lims1 <- sort(c(Y[1], Y[2]))
    lims2 <- sort(c(Y[2], Y[3]))
    lims3 <- sort(c(Y[3], Y[4]))

    col.NE <- 'lightgoldenrod1'

    rect(1 - rw, lims1[1], 1 + rw, lims1[2], col = cols[1])
    rect(2 - rw, lims2[1], 2 + rw, lims2[2], col = cols[2])
    rect(3 - rw, lims3[1], 3 + rw, lims3[2], col = cols[4])
    rect(4 - rw, lims3[1], 4 + rw, 0, col = 'grey80')

    if (lims1[2] > 0){
        segments(x0 = 1, x1 = 2, y0 = lims1[2], lty = 2)
    } else {
        segments(x0 = 1, x1 = 2, y0 = lims1[1], lty = 2)
    }
    segments(x0 = 2, x1 = 3, y0 = lims3[2], lty = 2)

    axis(2, cex.axis = 1.9)
    axis(1, at = X, labels = labels, col = 'white', cex.axis = 2)
    dev.off()

    # Non-cumulative version
    Y.for.lim <- c(rel.chg, sum(rel.chg))
    ylim <- c(min(Y.for.lim, na.rm = TRUE) - 0.5,
              max(Y.for.lim, na.rm = TRUE) + add.amount)

    pdf(file = outpath.ncumla, width = W, height = H)
    par(mar = c(4, 5, 1, 1))
    plot(x = X, axes = FALSE, col = 'white',
         xlim = xlim, ylim = ylim, ylab = ylab,
         xlab = '', cex.lab = 1.9)
    grid()
    abline(h = 0)

    lims1 <- sort(c(rel.chg[1], rel.chg[2]))
    lims2 <- sort(c(rel.chg[2], rel.chg[3]))
    lims3 <- sort(c(rel.chg[3], rel.chg[4]))
    rect(1 - rw, rel.chg[1], 1 + rw, 0, col = cols[1])
    rect(2 - rw, rel.chg[2], 2 + rw, 0, col = cols[2])
    rect(3 - rw, rel.chg[3], 3 + rw, 0, col = cols[4])
    rect(4 - rw, sum(rel.chg), 4 + rw, 0, col = 'grey80')

    axis(2, cex.axis = 1.9)
    axis(1, at = X, labels = labels, col = 'white', cex.axis = 2)
    dev.off()

    ## Consumer welfare decomposition
    cw.effect <- compute.cw.values(eqm.objs.co, FC.est, BL, cap)
    ## Restaurant profit decomposition
    rpi.effect <- compute.rpi.values(eqm.objs.co, FC.est, BL, cap)

    # Plot decompositions
    cols <- wesanderson::wes_palette("Cavalcanti1", n = 4)
    xlab <- 'Effect on consumer welfare ($/baseline platform order)'
    W <- 6.6
    H <- 3
    ## Consumers
    labels <- rev(c('Consumer fee increase only',
                    '... plus restaurant adoption response',
                    '... plus restaurant price response'))
    y <- rev(cw.effect)
    xlim <- range(y)
    xlim[1] <- floor(xlim[1]/0.50)*0.50
    xlim[2] <- max(ceiling(xlim[2]/0.50)*0.50, 0)

    pdf(outpath.cw, width = W, height = H)
    par(mar = c(5, 16, 0.2, 2.85))
    barplot(y, horiz = TRUE, col = cols[1],
            xlab = xlab, xlim = xlim,
            names.arg = labels, las = 2, axes = FALSE)
    axis(1)
    grid()
    barplot(y, horiz = TRUE, col = cols[1],
            xlab = '', add = TRUE, axes = FALSE,
            names.arg = rep('', times = length(labels)))
    dev.off()
    
    ## Table version for draft
    write.dat(data.frame(val = y, var = labels), 
              file = sub('pdf$', 'csv', outpath.cw))
    
    ## Restaurants
    labels <- rev(c('Commission reduction only',
                    'Consumer fee increase only',
                    'Combined commission and fee effects',
                    '... plus restaurant adoption response',
                    '... plus restaurant price response'))
    xlab <- 'Effect on restaurant profits ($/baseline platform order)'
    y <- rev(rpi.effect)
    xlim <- range(y)
    xlim[1] <- min(floor(xlim[1]/0.50)*0.50, 0)
    xlim[2] <- ceiling(xlim[2]/0.50)*0.50

    pdf(outpath.rpi, width = W, height = H)
    par(mar = c(5, 16, 0.2, 2.85))
    barplot(y, horiz = TRUE, col = cols[2],
            xlab = xlab, xlim = xlim,
            names.arg = labels, las = 2, axes = FALSE)
    axis(1)
    grid()
    barplot(y, horiz = TRUE, col = cols[2],
            xlab = '', add = TRUE, axes = FALSE,
            names.arg = rep('', times = length(labels)))
    dev.off()

    ### Limited version of restaurant plot
    labels <- rev(c('Commission reduction only',
                    '... plus consumer fee response',
                    '... plus restaurant adoption response',
                    '... plus restaurant price response'))
    xlab <- 'Effect on restaurant profits ($/baseline platform order)'
    idx.keep <- c('R', 'RC', 'J', 'all')
    y <- rev(rpi.effect[idx.keep])
    xlim <- range(y)
    xlim[1] <- min(floor(xlim[1]/0.50)*0.50, 0)
    xlim[2] <- ceiling(xlim[2]/0.50)*0.50

    pdf(outpath.rpi.sub, width = W, height = H)
    par(mar = c(5, 16, 0.2, 2.85))
    barplot(y, horiz = TRUE, col = cols[2],
            xlab = xlab, xlim = xlim,
            names.arg = labels, las = 2, axes = FALSE)
    axis(1)
    grid()
    barplot(y, horiz = TRUE, col = cols[2],
            xlab = '', add = TRUE, axes = FALSE,
            names.arg = rep('', times = length(labels)))
    dev.off()
    
    # Table versions (to reference in writing the draft)
    write.dat(data.frame(val = y, var = labels), 
              file = sub('pdf$', 'csv', outpath.rpi.sub))
    
}

compute.cw.values <- function(eqm.objs.co, FC.est, BL, cap){
    # Compute welfare values

    # Initialize outputs
    BL.rp  <- c()
    cap.rp <- c()
    C.rp   <- c()
    CJ.rp  <- c()

    S0 <- c()

    markets <- names(eqm.objs.co)

    no.logit <- FALSE

    for (market in markets){
        eqm.objs.m <- eqm.objs.co[[market]]
        counties <- names(eqm.objs.m)

        for (co in counties){
            eqm.objs.j <- eqm.objs.m[[co]]

            eqm.BL  <- BL[[market]][[co]]
            eqm.cap <- cap[[market]][[co]]

            dat.m <- eqm.objs.j$dat.m
            kappa <- eqm.objs.j$kappa
            opts  <- eqm.objs.j$opts

            # Restaurant profits
            compute.consumer.welfare(dat.m, eqm.BL$C, eqm.BL$FP$J.G.1, eqm.BL$FP$Rhos, no.logit = no.logit)
            BL.rp[co] <- compute.consumer.welfare(dat.m, eqm.BL$C, eqm.BL$FP$J.G.1,
                                                  eqm.BL$FP$Rhos, no.logit = no.logit)$total.EU.dollar
            cap.rp[co] <- compute.consumer.welfare(dat.m, eqm.cap$C, eqm.cap$FP$J.G.1,
                                                   eqm.cap$FP$Rhos, no.logit = no.logit)$total.EU.dollar
            ## Only fee
            C.rp[co] <- compute.consumer.welfare(dat.m, eqm.cap$C, eqm.BL$FP$J.G.1,
                                                 eqm.BL$FP$Rhos, no.logit = no.logit)$total.EU.dollar
            ## Fee and restaurant
            CJ.rp[co] <- compute.consumer.welfare(dat.m, eqm.cap$C, eqm.cap$FP$J.G.1,
                                                  eqm.BL$FP$Rhos, no.logit = no.logit)$total.EU.dollar

            # Baseline sales on platforms
            sales <- eqm.BL$FP$Sales.platform
            S0[co] <- sum(sales) - sales[1]
        }
    }

    chg.C   <- sum(C.rp   - BL.rp)
    chg.CJ  <- sum(CJ.rp  - BL.rp)
    chg.all <- sum(cap.rp - BL.rp)

    output <- c(C = chg.C, CJ = chg.CJ, all = chg.all)
    output <- output/sum(S0)

    return(output)
}

compute.rpi.values <- function(eqm.objs.co, FC.est, BL, cap){
    # Compute welfare values

    # Initialize outputs
    BL.rp  <- c()
    cap.rp <- c()
    OC.rp  <- c()
    OR.rp  <- c()
    CR.rp  <- c()
    J.rp   <- c()
    S0     <- c()

    markets <- names(eqm.objs.co)

    for (market in markets){
        eqm.objs.m <- eqm.objs.co[[market]]
        counties <- names(eqm.objs.m)

        for (co in counties){
            eqm.objs.j <- eqm.objs.m[[co]]

            eqm.BL  <- BL[[market]][[co]]
            eqm.cap <- cap[[market]][[co]]

            dat.m <- eqm.objs.j$dat.m
            kappa <- eqm.objs.j$kappa
            opts  <- eqm.objs.j$opts

            # Restaurant profits
            BL.rp[co]  <- compute.rpi(dat.m, eqm.BL$C,  eqm.BL$R,  eqm.BL$FP$J.G.1,  kappa,
                                      eqm.BL$FP$Rhos, opts)['total']
            cap.rp[co] <- compute.rpi(dat.m, eqm.cap$C, eqm.cap$R, eqm.cap$FP$J.G.1, kappa,
                                      eqm.cap$FP$Rhos, opts)['total']
            ## Only fee
            OC.rp[co] <- compute.rpi(dat.m, eqm.cap$C, eqm.BL$R,  eqm.BL$FP$J.G.1, kappa,
                                     eqm.BL$FP$Rhos, opts)['total']
            ## Only commission
            OR.rp[co] <- compute.rpi(dat.m, eqm.BL$C, eqm.cap$R,  eqm.BL$FP$J.G.1, kappa,
                                     eqm.BL$FP$Rhos, opts)['total']
            ## Both platform price changes
            CR.rp[co] <-  compute.rpi(dat.m, eqm.cap$C, eqm.cap$R,  eqm.BL$FP$J.G.1, kappa,
                                      eqm.BL$FP$Rhos, opts)['total']
            ## ... and J
            J.rp[co] <-  compute.rpi(dat.m, eqm.cap$C, eqm.cap$R, eqm.cap$FP$J.G.1, kappa,
                                     eqm.BL$FP$Rhos, opts)['total']

            # Baseline sales on platforms
            sales <- eqm.BL$FP$Sales.platform
            S0[co] <- sum(sales) - sales[1]
        }
    }

    chg.R   <- sum(OR.rp  - BL.rp)
    chg.C   <- sum(OC.rp  - BL.rp)
    chg.RC  <- sum(CR.rp  - BL.rp)
    chg.J   <- sum(J.rp   - BL.rp)
    chg.all <- sum(cap.rp - BL.rp)

    output <- c(R = chg.R, C = chg.C, RC = chg.RC, J = chg.J, all = chg.all)
    output <- output/sum(S0)

    return(output)
}

main()
