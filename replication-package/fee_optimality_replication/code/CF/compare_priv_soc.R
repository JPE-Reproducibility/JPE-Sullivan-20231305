# Compare the privately and socially optimal platform fees
library(Matrix)
library(FoodDeliveryTools)
library(EconTools)
library(wesanderson)

source('code/CF/welfare_calculations.R')

main <- function(){

    nsim.suffix <- '_nsim50'

    # Specify paths
    outdir <- sprintf('output/CF_feefirst/analysis%s', nsim.suffix)
    outpath         <- sprintf('%s/markups_soc_priv.pdf', outdir)
    outpath.markups <- sprintf('%s/markups_IQR.pdf', outdir)

    outpath.diff.c     <- sprintf('%s/compare_priv_soc_consumer_fee.csv', outdir)
    outpath.diff.r     <- sprintf('%s/compare_priv_soc_commission.csv',   outdir)
    outpath.diff.m     <- sprintf('%s/compare_priv_soc_markup.csv',       outdir)
    outpath.diff.combo <- sprintf('%s/compare_priv_soc_CR.csv',           outdir)

    outpath.S0      <- sprintf('%s/S_direct_soc_priv.pdf', outdir)
    outpath.S1      <- sprintf('%s/S_platform_soc_priv.pdf', outdir)
    outpath.barplot <- sprintf('%s/welfare_soc_priv.pdf', outdir)
    outpath.obs.chg <- sprintf('%s/soc_priv_observables.pdf', outdir)

    outpath.fees <- sprintf('%s/fees_IQR.pdf', outdir)
    outpath.comm <- sprintf('%s/comm_IQR.pdf', outdir)

    outpath.soc.opt.tab <- sprintf('%s/compare_soc_priv_fees_table.csv', outdir)

    outpath.competition.tab <- sprintf('%s/compare_fees_monopolization.csv', outdir)

    outpath.mono.C <- sprintf('%s/monopolization_fee_effects.pdf', outdir)
    outpath.mono.R <- sprintf('%s/monopolization_comm_effects.pdf', outdir)

    outpath.HHI.reg <- sprintf('%s/reg_Cdiff_competition.csv', outdir)

    CF.dir <- 'output/CF_feefirst/spec_nsim50'
    CF.files <- list.files(CF.dir)

    # Determine competitive and socially optimal equilibria
    priv   <- grep('^baseline_[a-z]+\\.rds', CF.files, value = TRUE)
    socopt <- grep('^socopt', CF.files, value = TRUE)
    ## Monopoly versions
    priv.M   <- grep('^sub1000_baseline_[a-z]+\\.rds', CF.files, value = TRUE)
    socopt.M <- grep('^sub1000_socopt', CF.files, value = TRUE)

    priv     <- sprintf('%s/%s', CF.dir, priv)
    socopt   <- sprintf('%s/%s', CF.dir, socopt)
    priv.M   <- sprintf('%s/%s', CF.dir, priv.M)
    socopt.M <- sprintf('%s/%s', CF.dir, socopt.M)

    eqm.objs <- load.all.eqm.objs.feefirst(nsim.suffix, TRUE)
    eqm.objs <- eqm.objs$county

    ## flatten the list
    markets <- names(eqm.objs)
    eqm.objs.co <- list()
    for (market in names(eqm.objs)){
        counties <- names(eqm.objs[[market]])
        for (co in counties){
            eqm.objs.co[[co]] <- eqm.objs[[market]][[co]]
        }
    }

    # load results
    Eqm.priv <- list()
    Eqm.soc  <- list()
    ## "M" is for monopoly
    M.priv <- list()
    M.soc  <- list()

    for (j in 1:length(priv)){
        Eqm <- readRDS(priv[j])
        counties <- names(Eqm)
        for (co in counties){
            Eqm.priv[[co]] <- Eqm[[co]]
        }
    }
    for (j in 1:length(socopt)){
        Eqm <- readRDS(socopt[j])
        counties <- names(Eqm)
        for (co in counties){
            Eqm.soc[[co]] <- Eqm[[co]]
        }
    }
    for (j in 1:length(priv.M)){
        Eqm <- readRDS(priv.M[j])
        counties <- names(Eqm)
        for (co in counties){
            M.priv[[co]] <- Eqm[[co]]
        }
    }
    for (j in 1:length(socopt.M)){
        Eqm <- readRDS(socopt.M[j])
        counties <- names(Eqm)
        for (co in counties){
            M.soc[[co]] <- Eqm[[co]]
        }
    }

    ## Every equilibrium set must cover the full county set; a missing or
    ## stale market file is a hard error, not silent sample selection
    counties.all <- names(eqm.objs.co)
    nms <- list('baseline (priv)'       = names(Eqm.priv),
                'socially optimal'      = names(Eqm.soc),
                'monopoly baseline'     = names(M.priv),
                'monopoly soc. optimal' = names(M.soc))
    for (k in names(nms)){
        assert.complete.names(nms[[k]], counties.all,
                              what = sprintf('%s equilibria in %s (counties)', k, CF.dir),
                              hint = 'Re-run the CF solvers for this spec.')
    }
    common.names <- sort(counties.all)
    ## Subsetting
    Eqm.priv <- lapply(common.names, function(cn) Eqm.priv[[cn]])
    Eqm.soc  <- lapply(common.names, function(cn) Eqm.soc[[cn]])
    M.priv   <- lapply(common.names, function(cn) M.priv[[cn]])
    M.soc    <- lapply(common.names, function(cn) M.soc[[cn]])
    eqm.objs.co <- lapply(common.names, function(cn) eqm.objs.co[[cn]])
    names(Eqm.priv) <- names(Eqm.soc) <- names(M.priv) <-
        names(M.soc) <- names(eqm.objs.co) <- common.names

    compute.ratios(Eqm.priv, Eqm.soc, M.priv, M.soc, eqm.objs.co, outdir)

    outputs   <- process.eqm(Eqm.soc, Eqm.priv, eqm.objs.co, monopoly = FALSE)
    outputs.M <- process.eqm(M.soc,   M.priv,   eqm.objs.co, monopoly = TRUE)

    # Share of restaurants online
    J.shr.soc  <- sum(outputs$J.soc)/sum(outputs$J.tot)
    J.shr.priv <- sum(outputs$J.priv)/sum(outputs$J.tot)
    nlist.soc <- sum(outputs$nlisting.soc)
    nlist.priv <- sum(outputs$nlisting.priv)

    # Online prices
    mean.p.soc  <- weighted.mean(outputs$price.soc,  outputs$S.on.soc)
    mean.p.priv <- weighted.mean(outputs$price.priv, outputs$S.on.priv)


    produce.sales.plots(outputs$S.on.priv,  outputs$S.on.soc,
                        outputs$S.off.priv, outputs$S.off.soc,
                        outpath.S0, outpath.S1)

    # Welfare gains from optimal pricing
    chg.C <- sum(outputs$CW.soc - outputs$CW.priv)
    chg.R <- sum(outputs$rp.soc - outputs$rp.priv)
    chg.P <- sum(outputs$mkp.soc*outputs$S.on.soc - outputs$mkp.priv*outputs$S.on.priv)
    chgs <- c(chg.C, chg.R, chg.P)
    chgs.rel <- chgs/sum(outputs$S.on.priv)
    chgs.rel <- c(chgs.rel, sum(chgs.rel))
    produce.welfare.change.plot(chgs.rel, outpath.barplot)

    ## Table version
    outpath.tabversion <- sub('.pdf', '.csv', outpath.barplot)
    tab <- data.frame(var = c('Consumer welfare',
                              'Restaurant profits',
                              'Platform profits',
                              'Total welfare'),
                      val = sprintf('%0.2f', chgs.rel))
    write.dat(tab, outpath.tabversion)

    # Changes in observables
    rel.chg.p <- (mean.p.soc/mean.p.priv - 1)*100
    rel.chg.J <- (J.shr.soc/J.shr.priv - 1)*100
    rel.chg.list <- (nlist.soc/nlist.priv - 1)*100
    labels <- c('# restaurant listings',
                '# restaurants online',
                'Online restaurant price')
    rel.chgs <- c(rel.chg.list, rel.chg.J, rel.chg.p)
    xlim <- c(floor(min(rel.chgs)/10)*10, ceiling(max(rel.chgs)/10)*10)

    pdf(outpath.obs.chg, width = 6, height = 3.8)
    par(mar = c(5.5, 10.5, 1, 3))
    barplot(rel.chgs, horiz = TRUE, names.arg = labels, las = 1,
            col = 'royalblue', xlim = xlim,
            xlab = 'Change from privately to socially optimal fees (%)')
    abline(v = 0)
    grid()
    barplot(rel.chgs, horiz = TRUE,
            names.arg = rep('', times = length(labels)),
            col = 'royalblue', add = TRUE)
    dev.off()

    # Table version
    outpath.tabversion <- sub('.pdf', '.csv', outpath.obs.chg)

    ## Additionally compute the share of sales made on online platforms
    S1.priv <- rowSums(outputs$S.f.priv)
    S0.priv <- outputs$S.off.priv
    S1.soc  <- rowSums(outputs$S.f.soc)
    S0.soc  <- outputs$S.off.soc

    ratio.priv <- sum(S1.priv)/sum(S0.priv + S1.priv)
    ratio.soc  <- sum(S1.soc)/sum(S0.soc + S1.soc)
    rel.chg.S0    <- (sum(S0.soc)/sum(S0.priv) - 1)*100
    rel.chg.S1    <- (sum(S1.soc)/sum(S1.priv) - 1)*100
    rel.chg.S     <- (sum(c(S0.soc, S1.soc))/sum(c(S0.priv, S1.priv)) - 1)*100
    rel.chg.ratio <- (ratio.soc/ratio.priv - 1)*100

    rel.chgs1 <- c(rel.chg.S, rel.chg.S1, rel.chg.S0, rel.chgs)
    tab <- data.frame(var = c('Restaurant prices',
                              'Share of restaurants online',
                              'Number of restaurant listings',
                              'First-party orders',
                              'Platform orders',
                              'Total orders'),
                      val = sprintf('%0.1f', rev(rel.chgs1)))
    tab$lineflag <- c(0, 0, 0, 1, 0, 0)
    write.dat(tab, outpath.tabversion)

    # Distribution of gaps between socially and privately optimal fees
    diff.C <- outputs$C.priv[, 1] - outputs$C.soc[, 1]
    diff.R <- outputs$R.priv[, 1] - outputs$R.soc[, 1]

    fees.priv <- outputs$C.priv[, 1]
    comm.priv <- outputs$R.priv[, 1]
    fees.soc  <- outputs$C.soc[, 1]
    comm.soc  <- outputs$R.soc[, 1]
    wgts <- outputs$w

    idx <- which(comm.soc > 0 & comm.priv > 0)
    produce.distribution.plot(fees.priv, fees.soc, wgts, outpath.fees,
                              'Consumer fee ($)')
    produce.distribution.plot(comm.priv[idx], comm.soc[idx], wgts[idx], outpath.comm,
                              'Restaurant commission rate', yfac = 0.1)
    produce.distribution.plot(outputs$mkp.priv, outputs$mkp.soc, wgts, outpath.markups,
                              'Aggregate markup ($/order)')

    ## weighted fees
    C.priv <- c()
    C.soc  <- c()
    R.priv <- c()
    R.soc  <- c()

    ## Standard deviations
    SD.C.priv <- c()
    SD.C.soc  <- c()
    SD.C.diff <- c()

    SD.R.priv <- c()
    SD.R.soc  <- c()
    SD.R.diff <- c()

    # Weights
    wgt.alt <- c()

    NF <- ncol(outputs$C.priv)
    for (f in 1:NF){
        C.priv[f] <- weighted.mean(outputs$C.priv[, f], outputs$S.f.priv[, f])
        C.soc[f]  <- weighted.mean(outputs$C.soc[, f] , outputs$S.f.priv[, f])
        R.priv[f] <- weighted.mean(outputs$R.priv[, f], outputs$S.f.priv[, f])
        R.soc[f]  <- weighted.mean(outputs$R.soc[, f] , outputs$S.f.priv[, f])

        # Standard deviations
        ## Consumer
        SD.C.priv[f] <- sqrt(weighted.mean((outputs$C.priv[, f] - C.priv[f])^2, outputs$S.f.priv[, f]))
        SD.C.soc[f]  <- sqrt(weighted.mean((outputs$C.soc[, f]  - C.soc[f])^2,  outputs$S.f.priv[, f]))
        ### Difference
        C.diff <- outputs$C.priv[, f] - outputs$C.soc[, f]
        C.diff.mean <- C.priv[f] - C.soc[f]
        SD.C.diff[f]  <- sqrt(weighted.mean((C.diff - C.diff.mean)^2,  outputs$S.f.priv[, f]))

        ## Restaurant
        SD.R.priv[f] <- sqrt(weighted.mean((outputs$R.priv[, f] - R.priv[f])^2, outputs$S.f.priv[, f]))
        SD.R.soc[f]  <- sqrt(weighted.mean((outputs$R.soc[, f]  - R.soc[f] )^2, outputs$S.f.priv[, f]))
        ### Difference
        R.diff <- outputs$R.priv[, f] - outputs$R.soc[, f]
        R.diff.mean <- R.priv[f] - R.soc[f]
        SD.R.diff[f]  <- sqrt(weighted.mean((R.diff - R.diff.mean)^2, outputs$S.f.priv[, f]))

        # Store weight for cross-platform analysis
        wgt.alt[f] <- sum(outputs$S.f.priv[, f])
    }

    # Compute platform specific aggregate markups
    counties <- names(Eqm.priv)
    opts <- load.opts()
    num.param <- load.num.param()
    markups.priv <- list()
    markups.soc  <- list()
    for (co in counties){
        pMC.df <- eqm.objs.co[[co]]$pMC.df
        kappa  <- eqm.objs.co[[co]]$kappa
        dat.m  <- eqm.objs.co[[co]]$dat.m

        Ep <- Eqm.priv[[co]]
        pp <- platform.profits(Ep$FP, Ep$C, Ep$R, kappa, dat.m, pMC.df, opts, num.param)
        markups.priv[[co]] <- pp/Eqm.priv[[co]]$FP$Sales.platform[2:5]

        Es <- Eqm.soc[[co]]
        pp <- platform.profits(Es$FP, Es$C, Es$R, kappa, dat.m, pMC.df, opts, num.param)
        markups.soc[[co]] <- pp/Eqm.soc[[co]]$FP$Sales.platform[2:5]
    }

    # Compute average aggregate markups by platform
    markups.priv <- do.call(rbind, markups.priv)
    markups.soc  <- do.call(rbind, markups.soc)

    agg.markups.priv <- c()
    agg.markups.soc  <- c()
    SD.M.priv <- c()
    SD.M.soc  <- c()
    SD.M.diff <- c()
    for (f in 1:NF){
        M.priv <- markups.priv[, f]
        M.soc  <- markups.soc[, f]
        M.diff <- M.priv - M.soc
        wgt    <- outputs$S.f.priv[, f]
        agg.markups.priv[f] <- weighted.mean(M.priv, wgt)
        agg.markups.soc[f]  <- weighted.mean(M.soc,  wgt)

        mean.diff <-  agg.markups.priv[f] - agg.markups.soc[f]
        SD.M.priv[f] <- sqrt(weighted.mean((M.priv - agg.markups.priv[f])^2, wgt))
        SD.M.soc[f]  <- sqrt(weighted.mean((M.soc  - agg.markups.soc[f])^2, wgt))
        SD.M.diff[f] <- sqrt(weighted.mean((M.diff - mean.diff)^2,          wgt))
    }

    ## Tables with platform-specific results
    platforms <- c('DD', 'Uber', 'GH', 'PM')

    tab.c <- data.frame(platform = platforms,
                        priv = C.priv, priv_SD = SD.C.priv,
                        soc = C.soc, soc_SD = SD.C.soc,
                        diff = C.priv - C.soc, diff_SD = SD.C.diff)
    tab.r <- data.frame(platform = platforms,
                        priv = R.priv, priv_SD = SD.R.priv,
                        soc = R.soc, soc_SD = SD.R.soc,
                        diff = R.priv - R.soc, diff_SD = SD.R.diff)
    tab.m <- data.frame(platform = platforms,
                        priv = agg.markups.priv, priv_SD = SD.M.priv,
                        soc = agg.markups.soc, soc_SD = SD.M.soc,
                        diff = agg.markups.priv - agg.markups.soc,
                        diff_SD = SD.M.diff)
    for (k in 2:ncol(tab.c)){
        cname <- colnames(tab.c)[k]
        if (grepl('SD', cname)){
            tab.c[, k] <- sprintf('\\scriptsize (%0.2f)', tab.c[, k])
            tab.r[, k] <- sprintf('\\scriptsize (%0.2f)', tab.r[, k]*100)
            tab.m[, k] <- sprintf('\\scriptsize (%0.2f)', tab.m[, k])
        } else {
            tab.c[, k] <- sprintf('%0.2f', tab.c[, k])
            tab.r[, k] <- sprintf('%0.2f', tab.r[, k]*100)
            tab.m[, k] <- sprintf('%0.2f', tab.m[, k])
        }
    }

    ## Table aggregating across platforms
    # Produce table summarizing all changes
    mean.c.priv <- weighted.mean(C.priv, wgt.alt)
    mean.c.soc  <- weighted.mean(C.soc,  wgt.alt)
    mean.r.priv <- weighted.mean(R.priv, wgt.alt)
    mean.r.soc  <- weighted.mean(R.soc,  wgt.alt)

    wgt <- sapply(rownames(outputs$C.priv), function(co) sum(Eqm.priv[[co]]$FP$Sales.platform[2:5]))  # county-level sales
    agg.markup.priv <- weighted.mean(outputs$mkp.priv, wgt)
    agg.markup.soc  <- weighted.mean(outputs$mkp.soc,  wgt)

    priv <- c(mean.c.priv, mean.r.priv, agg.markup.priv)
    soc  <- c(mean.c.soc,  mean.r.soc,  agg.markup.soc)
    diff <- priv - soc

    tab <- data.frame(var = c('Consumer fee (\\$)',
                              'Restaurant commission rate (\\%)',
                              'Aggregate markup (\\$)'),
                      priv = c(sprintf('%0.2f', priv[1]),
                               sprintf('%0.2f', priv[2]*100),
                               sprintf('%0.2f', priv[3])),
                      soc = c(sprintf('%0.2f', soc[1]),
                              sprintf('%0.2f', soc[2]*100),
                              sprintf('%0.2f', soc[3])),
                      diff = c(sprintf('%0.2f', diff[1]),
                               sprintf('%0.2f', diff[2]*100),
                               sprintf('%0.2f', diff[3])))
    write.dat(tab, outpath.soc.opt.tab)

    # Total standard deviations
    ## Consumer fees
    SD.C.priv.tot <- sqrt(weighted.mean(SD.C.priv^2, w = wgt.alt))
    SD.C.soc.tot  <- sqrt(weighted.mean(SD.C.soc^2,  w = wgt.alt))
    SD.C.diff.tot <- sqrt(weighted.mean(SD.C.diff^2, w = wgt.alt))
    ## Restaurant commissions
    SD.R.priv.tot <- sqrt(weighted.mean(SD.R.priv^2, w = wgt.alt))*100
    SD.R.soc.tot  <- sqrt(weighted.mean(SD.R.soc^2,  w = wgt.alt))*100
    SD.R.diff.tot <- sqrt(weighted.mean(SD.R.diff^2, w = wgt.alt))*100
    ## Markups
    SD.M.priv.tot <- sqrt(weighted.mean(SD.M.priv^2, w = wgt.alt))
    SD.M.soc.tot  <- sqrt(weighted.mean(SD.M.soc^2,  w = wgt.alt))
    SD.M.diff.tot <- sqrt(weighted.mean(SD.M.diff^2, w = wgt.alt))


    # Format standard deviations
    ## Consumer fees
    SD.C.priv.tot <-  sprintf('\\scriptsize (%0.2f)', SD.C.priv.tot)
    SD.C.soc.tot  <-  sprintf('\\scriptsize (%0.2f)', SD.C.soc.tot)
    SD.C.diff.tot <-  sprintf('\\scriptsize (%0.2f)', SD.C.diff.tot)
    ## Restaurant commissions
    SD.R.priv.tot <-  sprintf('\\scriptsize (%0.2f)', SD.R.priv.tot)
    SD.R.soc.tot  <-  sprintf('\\scriptsize (%0.2f)', SD.R.soc.tot)
    SD.R.diff.tot <-  sprintf('\\scriptsize (%0.2f)', SD.R.diff.tot)
    ## Markups
    SD.M.priv.tot <-  sprintf('\\scriptsize (%0.2f)', SD.M.priv.tot)
    SD.M.soc.tot  <-  sprintf('\\scriptsize (%0.2f)', SD.M.soc.tot)
    SD.M.diff.tot <-  sprintf('\\scriptsize (%0.2f)', SD.M.diff.tot)

    # Now add sales-weighted averages
    tab.c <- dplyr::bind_rows(tab.c, data.frame(platform = 'Total',
                                                priv    = tab$priv[1],
                                                priv_SD = SD.C.priv.tot,
                                                soc     = tab$soc[1],
                                                soc_SD  = SD.C.soc.tot,
                                                diff    = tab$diff[1],
                                                diff_SD = SD.C.diff.tot))
    tab.r <- dplyr::bind_rows(tab.r, data.frame(platform = 'Total',
                                                priv    = tab$priv[2],
                                                priv_SD = SD.R.priv.tot,
                                                soc     = tab$soc[2],
                                                soc_SD  = SD.R.soc.tot,
                                                diff    = tab$diff[2],
                                                diff_SD = SD.R.diff.tot))
    tab.m <- dplyr::bind_rows(tab.m, data.frame(platform = 'Total',
                                                priv    = tab$priv[3],
                                                priv_SD = SD.M.priv.tot,
                                                soc     = tab$soc[3],
                                                soc_SD  = SD.M.soc.tot,
                                                diff    = tab$diff[3],
                                                diff_SD = SD.M.diff.tot))
    write.dat(tab.c, outpath.diff.c)
    write.dat(tab.r, outpath.diff.r)
    write.dat(tab.m, outpath.diff.m)

    # Combined C and R
    idx <- grepl('(priv|soc|diff)', colnames(tab.c))
    colnames(tab.c)[idx] <- paste0(colnames(tab.c)[idx], '_C')
    idx <- grepl('(priv|soc|diff)', colnames(tab.r))
    colnames(tab.r)[idx] <- paste0(colnames(tab.r)[idx], '_R')
    tab.combo <- dplyr::left_join(tab.c, tab.r, by = 'platform')
    write.dat(tab.combo, outpath.diff.combo)

    ## But: I would like to incorporate other platforms
    ## sales weighting?
    sales.dd.BL <- outputs$S.f.priv[, 1]

    # Compare platform fees in competition and in monopoly
    diff.C.comp <- outputs.M$C.priv[, 1] - outputs$C.priv[, 1]
    diff.R.comp <- outputs.M$R.priv[, 1] - outputs$R.priv[, 1]

    diff.C.comp.soc <- outputs.M$C.soc[, 1] - outputs$C.soc[, 1]
    diff.R.comp.soc <- outputs.M$R.soc[, 1] - outputs$R.soc[, 1]

    qt <- c(0.10, 0.25, 0.50, 0.75, 0.90)
    dist.C <- sapply(qt, function(q) weighted.quantile(diff.C.comp, q, wgts))
    dist.R <- sapply(qt, function(q) weighted.quantile(diff.R.comp, q, wgts))

    dist.C.soc <- sapply(qt, function(q) weighted.quantile(diff.C.comp.soc, q, wgts))
    dist.R.soc <- sapply(qt, function(q) weighted.quantile(diff.R.comp.soc, q, wgts))


    yfac.C <- 1
    yfac.R <- 0.10
    combo.C <- c(dist.C, dist.C.soc)
    ylim.C <- range(combo.C)
    ylim.C[1] <-   floor(ylim.C[1]/yfac.C)*yfac.C
    ylim.C[2] <- ceiling(ylim.C[2]/yfac.C)*yfac.C

    combo.R <- c(dist.R, dist.R.soc)
    ylim.R <- range(combo.R)
    ylim.R[1] <-   floor(ylim.R[1]/yfac.R)*yfac.R
    ylim.R[2] <- ceiling(ylim.R[2]/yfac.R)*yfac.R


    colours <- wesanderson::wes_palette('Cavalcanti1', 4)[3:4]

    W <- 4.8
    H <- 4.8
    pdf(outpath.mono.C, width = W, height = H)
    plot(1:2, -100*c(1, 1), axes = FALSE, ylim = ylim.C, xlim = c(0.5, 2.5),
         xlab = '', ylab = 'Consumer fee change upon monopolization ($)')
    grid()
    abline(h = 0)
    h <- 0.25
    rect(xleft = 1 - h, xright = 1 + h,
         ybottom = dist.C[2], ytop = dist.C[4],
         col = colours[1])
    rect(xleft = 2 - h, xright = 2 + h,
         ybottom = dist.C.soc[2], ytop = dist.C.soc[4],
         col = colours[2])

    ## add whiskers
    segments(1, dist.C[4], y1 = dist.C[5])
    segments(1, dist.C[1], y1 = dist.C[2])
    segments(x0 = 1 - h, y0 = dist.C[3], x1 = 1 + h, lwd = 1.25)
    segments(x0 = 1 - h/3, y0 = dist.C[5], x1 = 1 + h/3)
    segments(x0 = 1 - h/3, y0 = dist.C[1], x1 = 1 + h/3)

    segments(2, dist.C.soc[4], y1 = dist.C.soc[5])
    segments(2, dist.C.soc[1], y1 = dist.C.soc[2])
    segments(x0 = 2 - h, y0 = dist.C.soc[3], x1 = 2 + h, lwd = 1.25)
    segments(x0 = 2 - h/3, y0 = dist.C.soc[5], x1 = 2 + h/3)
    segments(x0 = 2 - h/3, y0 = dist.C.soc[1], x1 = 2 + h/3)

    axis(1, at = 1:2, labels = c('Private', 'Social'))
    axis(2)
    dev.off()


    pdf(outpath.mono.R, width = W, height = H)
    plot(1:2, -100*c(1, 1), axes = FALSE, ylim = ylim.R, xlim = c(0.5, 2.5),
         xlab = '', ylab = 'Commission rate change upon monopolization ($)')
    grid()
    abline(h = 0)
    h <- 0.25
    rect(xleft = 1 - h, xright = 1 + h,
         ybottom = dist.R[2], ytop = dist.R[4],
         col = colours[1])
    rect(xleft = 2 - h, xright = 2 + h,
         ybottom = dist.R.soc[2], ytop = dist.R.soc[4],
         col = colours[2])

    ## add whiskers
    segments(1, dist.R[4], y1 = dist.R[5])
    segments(1, dist.R[1], y1 = dist.R[2])
    segments(x0 = 1 - h, y0 = dist.R[3], x1 = 1 + h, lwd = 1.25)
    segments(x0 = 1 - h/3, y0 = dist.R[5], x1 = 1 + h/3)
    segments(x0 = 1 - h/3, y0 = dist.R[1], x1 = 1 + h/3)

    segments(2, dist.R.soc[4], y1 = dist.R.soc[5])
    segments(2, dist.R.soc[1], y1 = dist.R.soc[2])
    segments(x0 = 2 - h, y0 = dist.R.soc[3], x1 = 2 + h, lwd = 1.25)
    segments(x0 = 2 - h/3, y0 = dist.R.soc[5], x1 = 2 + h/3)
    segments(x0 = 2 - h/3, y0 = dist.R.soc[1], x1 = 2 + h/3)

    axis(1, at = 1:2, labels = c('Private', 'Social'))
    axis(2)
    dev.off()

    # Table version
    ## use sales weights
    C.priv.fx <- weighted.mean(diff.C.comp, sales.dd.BL)
    R.priv.fx <- weighted.mean(diff.R.comp, sales.dd.BL)*100
    C.soc.fx  <- weighted.mean(diff.C.comp.soc, sales.dd.BL)
    R.soc.fx  <- weighted.mean(diff.R.comp.soc, sales.dd.BL)*100

    tab <- data.frame(var = c('Consumer fee (\\$)', 'Restaurant commission (pp)'),
                      priv = c(C.priv.fx, R.priv.fx),
                      soc  = c(C.soc.fx, R.soc.fx))
    tab$priv <- sprintf('%0.2f', tab$priv)
    tab$soc  <- sprintf('%0.2f', tab$soc)
    write.dat(tab, outpath.competition.tab)
}

compute.ratios <- function(Eqm.priv, Eqm.soc, M.priv, M.soc, eqm.objs.co, outdir){
    counties <- names(eqm.objs.co)
    num.param <- load.num.param()

    C.priv  <- c()
    C.soc   <- c()
    R.priv  <- c()
    R.soc   <- c()

    agg.markup <- c()
    agg.markup.M <- c()

    ratios.priv   <- list()
    ratios.soc    <- list()
    ratios.priv.M <- c()
    ratios.soc.M  <- c()
    S.priv <- list()
    S.soc  <- list()
    S.priv.M <- c()
    S.soc.M  <- c()

    f <- 1  # monopoly platform

    wgt <- c()
    for (co in counties){
        eqm.objs.j <- eqm.objs.co[[co]]

        kappa <- eqm.objs.j$kappa
        opts <- eqm.objs.j$opts
        pMC.df <- eqm.objs.j$pMC.df
        dat.m <- eqm.objs.j$dat.m

        priv <- Eqm.priv[[co]]
        soc  <- Eqm.soc[[co]]

        C.priv[co] <- priv$C[1]
        C.soc[co]  <- soc$C[1]
        R.priv[co] <- priv$R[1]
        R.soc[co]  <- soc$R[1]

        ## Private
        pp <- platform.profits(priv$FP, priv$C, priv$R, kappa, dat.m, pMC.df, opts, num.param, more.outputs = TRUE)
        C.rev <- priv$C*pp$pl.sales[2:5]
        R.rev <- pp$pl.revenues - C.rev
        ratios.priv[[co]] <- C.rev/R.rev
        S.priv[[co]]      <- pp$pl.sales[2:5]


        agg.markup[co] <- pp$pl.profits[f]/S.priv[[co]][f]

        ## Social
        pp <- platform.profits(soc$FP, soc$C, soc$R, kappa, dat.m, pMC.df, opts, num.param, more.outputs = TRUE)
        C.rev <- soc$C*pp$pl.sales[2]
        R.rev <- pp$pl.revenues - C.rev
        ratios.soc[[co]] <- C.rev/R.rev
        S.soc[[co]]      <- pp$pl.sales[2:5]

        # Monopoly
        priv.M <- M.priv[[co]]
        soc.M  <- M.soc[[co]]
        ## Private
        pp <- platform.profits(priv.M$FP, priv.M$C, priv.M$R, kappa, dat.m, pMC.df, opts, num.param, more.outputs = TRUE,
                               keep.platforms = c(1, 0, 0, 0))
        C.rev <- priv.M$C[f]*pp$pl.sales[f + 1]
        R.rev <- pp$pl.revenues[f] - C.rev
        ratios.priv.M[co] <- C.rev/R.rev
        S.priv.M[co] <- pp$pl.sales[f + 1]

        agg.markup.M[co] <- pp$pl.profits[f]/S.priv.M[[co]]

        ## Social
        pp <- platform.profits(soc.M$FP, soc.M$C, soc.M$R, kappa, dat.m, pMC.df, opts, num.param,
                               more.outputs = TRUE, keep.platforms = c(1, 0, 0, 0))
        C.rev <- soc.M$C[f]*pp$pl.sales[f + 1]
        R.rev <- pp$pl.revenues[f] - C.rev
        ratios.soc.M[co] <- C.rev/R.rev
        S.soc.M[co] <- pp$pl.sales[f + 1]

        wgt[co] <- sum(dat.m$buy.m$count)
    }
    ratio.mat.priv   <- do.call(rbind, ratios.priv)
    ratio.mat.soc    <- do.call(rbind, ratios.soc)

    qt <- c(0.10, 0.25, 0.50, 0.75, 0.90)
    dist.p  <- sapply(qt, function(q) weighted.quantile(ratio.mat.priv[, 1],   q, wgt))
    dist.s  <- sapply(qt, function(q) weighted.quantile(ratio.mat.soc[, 1],    q, wgt))
    dist.pM <- sapply(qt, function(q) weighted.quantile(ratios.priv.M, q, wgt))
    dist.sM <- sapply(qt, function(q) weighted.quantile(ratios.soc.M,  q, wgt))

    nbar <- 4
    ylim <- c(0, 2)

    outpath <- sprintf('%s/fee_commission_ratios.pdf', outdir)
    colours <- wes_palette('Cavalcanti1', 4)[c(3, 4, 3, 4)]
    pdf(outpath, width = 8, height = 5)
    plot(1:nbar, -100*rep(1, times = nbar), ylim = ylim, xlim = c(0.5, nbar + 0.5),
         ylab = 'Fee/commission ratio', axes = FALSE,
         xlab = '')
    grid()
    abline(h = 0, col = 'grey20')
    make.IQR.bar(1, dist.p,  colour = colours[1])
    make.IQR.bar(2, dist.s,  colour = colours[2])
    make.IQR.bar(3, dist.pM, colour = colours[3])
    make.IQR.bar(4, dist.sM, colour = colours[4])
    axis(2)
    axis(1, at = 1:4, labels = c('Private', 'Social',
                                 'Private', 'Social'))
    abline(v = 2.5, col = 'grey20', lty = 2)
    text('Competition', x = 1.5, y = ylim[2] - 0.1, cex = 1.2)
    text('Monopoly'   , x = 3.5, y = ylim[2] - 0.1, cex = 1.2)
    dev.off()


    # Gap in private vs social c and r
    gap.C <- weighted.mean(C.priv - C.soc, wgt)
    gap.R <- weighted.mean(R.priv - R.soc, wgt)
    gap.rel.C <- weighted.mean(C.priv - C.soc, wgt)/weighted.mean(C.priv, wgt)
    gap.rel.R <- weighted.mean(R.priv - R.soc, wgt)/weighted.mean(R.priv, wgt)
    ## save
    outpath.gap <- sprintf('%s/priv_soc_gaps.csv', outdir)
    gap.tab <- data.frame(var = c('C', 'R'), abs = c(gap.C, gap.R),
                          rel = c(gap.rel.C, gap.rel.R))
    write.dat(gap.tab, outpath.gap)
}

process.eqm <- function(Eqm.soc, Eqm.priv, eqm.objs.co, monopoly){

    # Indices for online platforms versus offline ordering
    idx.on <- 2:5
    idx.off <- 1

    # Initialize outputs
    C.soc  <- list()
    C.priv <- list()
    R.soc  <- list()
    R.priv <- list()

    S.off.soc <- c()
    S.on.soc  <- c()
    S.off.priv <- c()
    S.on.priv  <- c()

    S.f.soc  <- list()
    S.f.priv <- list()

    mkp.soc  <- c()
    mkp.priv <- c()

    rp.soc  <- c()
    rp.priv <- c()

    CW.soc  <- c()
    CW.priv <- c()

    # Various "observable" variables
    fees.soc   <- c()
    fees.priv  <- c()
    J.soc      <- c()
    J.priv     <- c()
    J.tot      <- c()
    price.soc  <- c()
    price.priv <- c()

    nlisting.soc  <- c()
    nlisting.priv <- c()

    # Weights
    wgts <- c()

    assert.complete.names(names(Eqm.priv), names(Eqm.soc),
                          what = 'process.eqm: baseline equilibria (counties)',
                          hint = 'Baseline and socially-optimal county sets must match.')
    assert.complete.names(names(Eqm.soc), names(Eqm.priv),
                          what = 'process.eqm: socially-optimal equilibria (counties)',
                          hint = 'Baseline and socially-optimal county sets must match.')
    common.names <- intersect(names(Eqm.priv), names(Eqm.soc))
    for (cn in common.names){
        Es <- Eqm.soc[[cn]]
        Ep <- Eqm.priv[[cn]]

        C.soc[[cn]]  <- Es$C
        C.priv[[cn]] <- Ep$C
        R.soc[[cn]]  <- Es$R
        R.priv[[cn]] <- Ep$R

        # Total platform sales
        S.off.soc[cn]  <- Es$FP$Sales.platform[idx.off]
        S.on.soc [cn]  <- sum(Es$FP$Sales.platform[idx.on])
        S.off.priv[cn] <- Ep$FP$Sales.platform[idx.off]
        S.on.priv[cn]  <- sum(Ep$FP$Sales.platform[idx.on])

        S.f.soc[[cn]]  <- Es$FP$Sales.platform[idx.on]
        S.f.priv[[cn]] <- Ep$FP$Sales.platform[idx.on]

        eqm.objs.j <- eqm.objs.co[[cn]]
        profit.soc  <- extract.p.profit(Es, eqm.objs.j)
        profit.priv <- extract.p.profit(Ep, eqm.objs.j)

        # Markups
        mkp.soc[cn]  <- sum(profit.soc$pl.profits)/S.on.soc[cn]
        mkp.priv[cn] <- sum(profit.priv$pl.profits)/S.on.priv[cn]

        # Restaurant profit
        dat.m <- eqm.objs.j$dat.m
        kappa <- eqm.objs.j$kappa
        opts  <- eqm.objs.j$opts

        if (monopoly){
            dat.m$psi[] <- -50
        }

        rp.soc[cn]  <- compute.rpi(dat.m, Es$C, Es$R, Es$FP$J.G.1, kappa, Es$FP$Rhos, opts)['total']
        rp.priv[cn] <- compute.rpi(dat.m, Ep$C, Ep$R, Ep$FP$J.G.1, kappa, Ep$FP$Rhos, opts)['total']

        # Consumer welfare (first pass)
        ## Socially optimal
        CW <- compute.consumer.welfare(dat.m, Es$C, Es$FP$J.G.1, Es$FP$Rhos, no.logit = FALSE)
        CW.soc[cn]  <- CW$total.EU.dollar
        ## Privately optimal
        CW <- compute.consumer.welfare(dat.m, Ep$C, Ep$FP$J.G.1, Ep$FP$Rhos, no.logit = FALSE)
        CW.priv[cn] <- CW$total.EU.dollar

        # Population weights
        wgts[cn] <- sum(dat.m$buy.m$count)

        ## Restaurants' uptake of platforms
        Js <- do.call(rbind, Es$FP$J.G.1)
        J.soc[cn] <- sum(Js) - sum(Js[, 'G0000'])
        Jp <- do.call(rbind, Ep$FP$J.G.1)
        J.priv[cn] <- sum(Jp) - sum(Jp[, 'G0000'])
        J.tot[cn] <- sum(Jp)

        Js.agg <- colSums(Js)
        Jp.agg <- colSums(Jp)
        nplatforms <- strsplit(sub('G', '', names(Js.agg)), '')
        nplatforms <- sapply(nplatforms, function(x) sum(as.numeric(x)))
        nlisting.s <- sum(Js.agg*nplatforms)
        nlisting.p <- sum(Jp.agg*nplatforms)

        nlisting.soc[cn]  <- nlisting.s
        nlisting.priv[cn] <- nlisting.p

        ## Prices
        rho.soc  <- weight.rhos.v2(Es$FP$Sales.tots, Es$FP$Rhos, dat.m)
        rho.priv <- weight.rhos.v2(Ep$FP$Sales.tots, Ep$FP$Rhos, dat.m)
        #### make this more sophisticated eventually
        price.soc[cn]  <- weighted.mean(colMeans(rho.soc[, idx.on]),
                                        Es$FP$Sales.platform[idx.on])
        price.priv[cn] <- weighted.mean(colMeans(rho.priv[, idx.on]),
                                        Ep$FP$Sales.platform[idx.on])
    }
    C.soc    <- do.call(rbind, C.soc)
    C.priv   <- do.call(rbind, C.priv)
    R.soc    <- do.call(rbind, R.soc)
    R.priv   <- do.call(rbind, R.priv)
    S.f.priv <- do.call(rbind, S.f.priv)
    S.f.soc  <- do.call(rbind, S.f.soc)

    # Collate outputs
    outputs <- list()
    outputs$C.soc      <- C.soc
    outputs$C.priv     <- C.priv
    outputs$R.soc      <- R.soc
    outputs$R.priv     <- R.priv
    outputs$S.off.soc  <- S.off.soc
    outputs$S.on.soc   <- S.on.soc
    outputs$S.off.priv <- S.off.priv
    outputs$S.on.priv  <- S.on.priv
    outputs$S.f.priv   <- S.f.priv
    outputs$S.f.soc    <- S.f.soc
    outputs$idx.on     <- idx.on
    outputs$idx.off    <- idx.off
    outputs$mkp.soc    <- mkp.soc
    outputs$mkp.priv   <- mkp.priv
    outputs$rp.soc     <- rp.soc
    outputs$rp.priv    <- rp.priv
    outputs$CW.soc     <- CW.soc
    outputs$CW.priv    <- CW.priv
    outputs$fees.soc   <- fees.soc
    outputs$fees.priv  <- fees.priv
    outputs$J.soc      <- J.soc
    outputs$J.priv     <- J.priv
    outputs$J.tot      <- J.tot
    outputs$price.soc  <- price.soc
    outputs$price.priv <- price.priv
    outputs$wgts       <- wgts

    outputs$nlisting.soc  <- nlisting.soc
    outputs$nlisting.priv <- nlisting.priv

    return(outputs)
}

produce.distribution.plot <- function(x.priv, x.soc, wgts, outpath, ylab, yfac = 1){
    # Produce plot describing the distribution of markups
    qt <- c(0.05, 0.25, 0.50, 0.75, 0.95)
    dist.priv <- sapply(qt, function(q) weighted.quantile(x.priv, q, wgts))
    dist.soc  <- sapply(qt, function(q) weighted.quantile(x.soc,  q, wgts))
    dist.gap  <- sapply(qt, function(q) weighted.quantile(x.priv - x.soc, q, wgts))

    combo <- c(dist.priv, dist.soc, dist.gap)
    ylim <- range(combo)
    ylim[1] <- floor(ylim[1]/yfac)*yfac
    ylim[2] <- ceiling(ylim[2]/yfac)*yfac

    colours <- c(wes_palette('Cavalcanti1', 4)[c(3, 4)], 'lightyellow')

    pdf(outpath, height = 4, width = 4)
    plot(1:3, -100*c(1, 1, 1), axes = FALSE, ylim = ylim, xlim = c(0.5, 3.5),
         xlab = '', ylab = ylab)
    grid()
    abline(h = 0)
    h <- 0.25
    rect(xleft = 1 - h, xright = 1 + h,
         ybottom = dist.priv[2], ytop = dist.priv[4],
         col = colours[1])
    rect(xleft = 2 - h, xright = 2 + h,
         ybottom = dist.soc[2], ytop = dist.soc[4],
         col = colours[2])
    rect(xleft = 3 - h, xright = 3 + h,
         ybottom = dist.gap[2], ytop = dist.gap[4],
         col = colours[3])

    ## add whiskers
    segments(1, dist.priv[4], y1 = dist.priv[5])
    segments(1, dist.priv[1], y1 = dist.priv[2])
    segments(x0 = 1 - h, y0 = dist.priv[3], x1 = 1 + h, lwd = 1.25)
    segments(x0 = 1 - h/3, y0 = dist.priv[5], x1 = 1 + h/3)
    segments(x0 = 1 - h/3, y0 = dist.priv[1], x1 = 1 + h/3)

    segments(2, dist.soc[4], y1 = dist.soc[5])
    segments(2, dist.soc[1], y1 = dist.soc[2])
    segments(x0 = 2 - h, y0 = dist.soc[3], x1 = 2 + h, lwd = 1.25)
    segments(x0 = 2 - h/3, y0 = dist.soc[5], x1 = 2 + h/3)
    segments(x0 = 2 - h/3, y0 = dist.soc[1], x1 = 2 + h/3)

    segments(3, dist.gap[4], y1 = dist.gap[5])
    segments(3, dist.gap[1], y1 = dist.gap[2])
    segments(x0 = 3 - h, y0 = dist.gap[3], x1 = 3 + h, lwd = 1.25)
    segments(x0 = 3 - h/3, y0 = dist.gap[5], x1 = 3 + h/3)
    segments(x0 = 3 - h/3, y0 = dist.gap[1], x1 = 3 + h/3)

    axis(1, at = 1:3, labels = c('Private', 'Social', 'Difference'))
    axis(2)
    dev.off()
}

produce.sales.plots <- function(S.on.priv, S.on.soc, S.off.priv, S.off.soc,
                                outpath.S0, outpath.S1){
    # Sales plot

    ## On-platform sales
    x <- log(S.on.priv*1e5)
    y <- log(S.on.soc*1e5)
    axis.lim <- range(c(x, y))
    axis.lim[1] <- floor(axis.lim[1]/2)*2
    axis.lim[2] <- ceiling(axis.lim[2]/2)*2

    pdf(outpath.S1, width = 6, height = 5)
    plot(x, y, axes = FALSE,
         xlab = 'Log platform orders (private)',
         ylab = 'Log platform orders (social)', pch = 16,
         xlim = axis.lim, ylim = axis.lim)
    abline(0, 1)
    grid()
    axis(1)
    axis(2)
    dev.off()

    x <- log(S.off.priv*1e5)
    y <- log(S.off.soc*1e5)
    axis.lim <- range(c(x, y))
    axis.lim[1] <- floor(axis.lim[1]/2)*2
    axis.lim[2] <- ceiling(axis.lim[2]/2)*2

    pdf(outpath.S0, width = 6, height = 5)
    plot(log(S.off.priv*1e5), log(S.off.soc*1e5), axes = FALSE,
         xlab = 'Log first-party orders (private)',
         ylab = 'Log first-party orders (social)', pch = 16,
         xlim = axis.lim, ylim = axis.lim)
    abline(0, 1)
    grid()
    axis(1)
    axis(2)
    dev.off()
}

produce.welfare.change.plot <- function(chgs.rel, outpath.barplot){

    # Welfare change by component upon moving to optimal pricing
    # (relative to sales in baseline)
    ymax <- ceiling(max(chgs.rel)/5)*5
    ymin <- floor(min(chgs.rel)/5)*5
    ylim <- c(ymin, ymax)

    # Graphical settings
    colours <- wesanderson::wes_palette('Cavalcanti1', 4)
    colours <- c(colours[c(1, 2, 4)], 'grey80')
    W <- 9
    H <- 7

    # Produce plot
    pdf(file = outpath.barplot, width = W, height = H)
    # half of rectangle width
    rw <- 0.25
    X <- 1:length(chgs.rel)
    ylab <- 'Welfare change ($/platform order in baseline)'
    xlim <- c(min(X) - rw, max(X) + rw)
    labels <- c('Consumers', 'Restaurants', 'Platforms', 'Total')

    par(mar = c(4, 5, 1, 1))
    plot(x = X, axes = FALSE, col = 'white',
         xlim = xlim, ylim = ylim, ylab = ylab,
         xlab = '', cex.lab = 1.9)
    grid()
    abline(h = 0)

    # Place rectangles
    rect(1 - rw, chgs.rel[1], 1 + rw, 0, col = colours[1])
    rect(2 - rw, chgs.rel[2], 2 + rw, 0, col = colours[2])
    rect(3 - rw, chgs.rel[3], 3 + rw, 0, col = colours[3])
    rect(4 - rw, chgs.rel[4], 4 + rw, 0, col = colours[4])

    axis(2, cex.axis = 1.9)
    axis(1, at = X, labels = labels, col = 'white', cex.axis = 2)
    dev.off()
}

main()
