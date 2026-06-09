# Describe the estimated restaurant marginal costs/markups
# Goal with v3: report tables with (i) markups and (ii) raw costs,
# aggregating across chains and independents

library(FoodDeliveryTools)
library(EconTools)

main <- function(){
    # Produce tables/figures that summarize the distribution of costs
    # which.restos: chain, indep, or all

    which.restos <- 'all'

    # Specify paths
    inpaths <- collect.inpaths(nsim.suffix = '_nsim50')
    inpath.cap <- 'data/fee_caps/monthly_fee_caps.csv'

    outdir <- 'output/recover_restaurant_costs/describe'
    create.dir(outdir)

    # Version with standard deviations
    outpath.combo <- sprintf('%s/combo_tab_%s.csv', outdir, which.restos)
    outpath.combo.mc <- sprintf('%s/combo_tab_mc_%s.csv', outdir, which.restos)

    # Version with SEs
    outpath.combo.SE <- sprintf('%s/combo_tab_%s_SEs.csv', outdir, which.restos)
    outpath.combo.mc.SE <- sprintf('%s/combo_tab_mc_%s_SEs.csv', outdir, which.restos)


    # Guards: fail loudly with actionable messages if prerequisites are absent
    for (p in c(inpath.cap, inpaths$inpath.dat, inpaths$inpath.rMC)){
        if (!file.exists(p)) stop(sprintf("required input missing: %s", p))
    }

    # Read data
    cap.df <- EconTools::read.dat(inpath.cap, colClasses = c('zip' = 'character'))
    cap.df$has.cap <- cap.df$cap < 0.30
    cap.df <- cap.df[which(cap.df$month == '2021-04-01'), ]

    dat <- readRDS(inpaths$inpath.dat)

    BL <- compute.mean.costs(inpaths$inpath.rMC, dat, cap.df)


    write.dat(BL$Markup.combo, outpath.combo)
    write.dat(BL$MC.combo, outpath.combo.mc)

    # Versions with standard errors. Limit to friction bootstrap files (the
    # restoMC/ dir may also hold non-friction artefacts; produce_GMM_table.R
    # filters the same way).
    boot.dir <- 'data/bootstrap/dat/restoMC'
    if (!dir.exists(boot.dir)){
        stop(sprintf("friction bootstrap directory missing: %s\n",
                     boot.dir),
             "Run code/bootstrap_restaurant_costs/bootstrap_friction.R first.")
    }
    inpaths.boot <- list.files(boot.dir, pattern = 'friction', full.names = TRUE)
    if (length(inpaths.boot) == 0){
        stop(sprintf("no friction bootstrap files found in %s\n", boot.dir),
             "Run code/bootstrap_restaurant_costs/bootstrap_friction.R first.")
    }
    boot.results <- lapply(inpaths.boot, compute.mean.costs, dat = dat, cap.df = cap.df, boot = TRUE)

    Markup.boot <- lapply(boot.results, function(br) as.matrix(apply(br$Markup.means[1:2, c('NoCap', 'Cap')], 2, as.numeric)))
    Costs.boot  <- lapply(boot.results, function(br) as.matrix(apply(br$MC.means[1:2, c('NoCap', 'Cap')], 2, as.numeric)))

    # Bootstrap SE = sample sd across replicates (consistent with sd() used in
    # produce_GMM_table.R; the prior code used a population variance that
    # could go slightly negative on near-constant cells -> sqrt = NaN).
    Costs.arr  <- simplify2array(Costs.boot)
    Markup.arr <- simplify2array(Markup.boot)
    Costs.SE  <- apply(Costs.arr,  c(1, 2), sd)
    Markup.SE <- apply(Markup.arr, c(1, 2), sd)

    markup.panel <- BL$Markup.means
    mc.panel   <- BL$MC.means

    markup.panel$NoCapSE <- sprintf('\\footnotesize (%0.2f)', Markup.SE[, 1])
    markup.panel$CapSE <- sprintf('\\footnotesize (%0.2f)', Markup.SE[, 2])

    mc.panel$NoCapSE <- sprintf('\\footnotesize (%0.2f)', Costs.SE[, 1])
    mc.panel$CapSE <- sprintf('\\footnotesize (%0.2f)', Costs.SE[, 2])

    write.dat(markup.panel, outpath.combo.SE)
    write.dat(mc.panel, outpath.combo.mc.SE)
}

compute.mean.costs <- function(inpath, dat, cap.df, boot = FALSE){
    rMC <- readRDS(inpath)
    phi <- rMC$phi
    rMC <- rMC$costs
    markets <- names(rMC)

    rhos <- list()
    for (market in names(dat)){
        dat.m <- dat[[market]]
        rhos[[market]] <- sapply(dat.m$eqm.weighting$Rhos, function(x) x[[1]])
    }
    rhos <- Reduce(c, rhos)
    rhos.i <- rhos[grep('i$', names(rhos))]
    rhos.c <- rhos[grep('c$', names(rhos))]

    alt.prices <- TRUE

    # Initialize objects
    mean.on  <- c()
    mean.off <- c()

    rho.on.c  <- c()
    rho.off.c <- c()
    rho.on.i  <- c()
    rho.off.i <- c()

    rho.on.cap.c <- c()
    rho.on.cap.i <- c()

    ## ZIP level
    markup.on.zip  <- list()
    markup.off.zip <- list()

    avg.markup.on.cap   <- c()
    avg.markup.on.nocap <- c()
    avg.markup.off.cap   <- c()
    avg.markup.off.nocap <- c()

    all.mc.on   <- list()
    all.mc.off  <- list()

    weights.on  <- list()
    weights.off <- list()

    Cap.on  <- list()
    Cap.off <- list()

    for (market in markets){
        # Extract costs
        rMC.m <- rMC[[market]]
        zips <- names(rMC.m$costs.on)

        # Extract data/equilibrium
        dat.m <- dat[[market]]
        J.G <- dat.m$J.G.1.m

        eqm.m <- dat.m$eqm.weighting
        Rhos <- eqm.m$Rhos

        if (boot){
            markups <- list()
            for (z in names(Rhos)){
                z0 <- sub('[ci]$', '', z)
                idx.z <- which(dat.m$caps.df$zip == z0)
                excl.z <- dat.m$excl.chain[z0]

                cap.z <- dat.m$caps.df$caps[idx.z]
                if (cap.z < 0.3 & excl.z > 0){
                    cap.z <- 0.3
                }


                n.costs <- length(rMC.m$costs[[z]])
                n.rhos  <- length(Rhos[[z]])
                n.min   <- min(n.costs, n.rhos)
                if (n.min == 0){
                    next
                }
                for (g in 1:n.min){
                    Rhos.zg <- Rhos[[z]][[g]]
                    costs.zg <- rMC.m$costs[[z]][[g]]
                    if (length(Rhos.zg) > 0){
                        cap.zg <- c(0, rep(cap.z, times = length(Rhos[[z]][[g]]) - 1))
                        markups[[z]][[g]] <- Rhos[[z]][[g]]*(1 - cap.zg) - rMC.m$costs[[z]][[g]]
                    } else {
                        markups[[z]][[g]] <- NULL
                    }
                }
            }
        } else {
            markups <- eqm.m$markup
        }


        weights <- sapply(zips, function(z) sum(J.G[[z]]))
        c.on <- rMC.m$costs.on
        c.off <- rMC.m$costs.off

        idx.c <- grep('c$', zips)
        idx.i <- grep('i$', zips)
        w.c <- weights[idx.c]
        w.i <- weights[idx.i]

        mean.on[market]  <- weighted.mean(c.on[c(idx.c, idx.i)],  w = c(w.c, w.i))
        mean.off[market] <- weighted.mean(c.off[c(idx.c, idx.i)], w = c(w.c, w.i))

        Zips <- names(markups)
        # Markup by ZIP
        Markup <- list()
        J      <- c()

        for (z in Zips){
            m.z <- markups[[z]]

            J.z <- c()
            markup.mat  <- list()
            for (g in 1:length(m.z)){
                if (!is.null(m.z[[g]])){
                    ng <- nrow(m.z[[g]])
                    if (length(ng) == 0){
                        next
                    }

                    idx <- dat.m$G.indices[[g]]
                    markup.mat[[g]] <- rep(0, times = 5)
                    markup.mat[[g]][idx] <- m.z[[g]][1:ng]
                    J.z[[g]] <- dat.m$J.G.1.m[[z]][g]
                }
            }
            if (length(markup.mat) == 1){
                markup.mat <- matrix(markup.mat[[1]], nrow = 1)
            } else {
                markup.mat <- Reduce(rbind, markup.mat)
            }

            J.z <- Reduce(c, J.z)

            mean.markup <- c()
            for (f in 1:ncol(markup.mat)){
                idx <- markup.mat[, f] != 0
                mean.markup[f] <- weighted.mean(markup.mat[idx, f], J.z[idx])
            }
            Markup[[z]] <- mean.markup
            J[z] <- sum(J.z)
        }

        Markup.names <- names(Markup)
        Markup <- Reduce(rbind, Markup)
        rownames(Markup) <- Markup.names

        Markup.mean <- c()
        for (k in 1:ncol(Markup)){
            idx <- !is.nan(Markup[, k])
            Markup.mean[k] <- weighted.mean(Markup[idx, k], J[idx])
        }
        ## Number of restaurants on each platform to get total online
        nresto <- Reduce('+', dat.m$J.G.1.m)
        n.f <- c()
        for (k in 1:4){
            n.f[k] <- sum(nresto[substr(names(nresto), k + 1, k + 1) == '1'])
        }
        Markup.online <- weighted.mean(Markup.mean[2:5], n.f)

        # Disaggregated
        markup.on <- Markup[, 2]
        names(markup.on) <- rownames(Markup)
        markup.on <- markup.on[!is.nan(markup.on)]
        markup.off <- Markup[, 1]
        names(markup.off) <- rownames(Markup)

        z0.on   <- sub('[ci]$', '', names(markup.on))
        z0.off  <- sub('[ci]$', '', names(markup.off))
        cap.on  <- sapply(z0.on,  function(z) cap.df$cap[which(cap.df$zip == z)] < 0.3)
        cap.off <- sapply(z0.off, function(z) cap.df$cap[which(cap.df$zip == z)] < 0.3)
        w.on  <- weights[names(markup.on)]
        w.off <- weights[names(markup.off)]

        avg.markup.on.cap[market]    <- weighted.mean(markup.on,  w.on*cap.on)
        avg.markup.on.nocap[market]  <- weighted.mean(markup.on,  w.on*(1 - cap.on))
        avg.markup.off.cap[market]   <- weighted.mean(markup.off, w.off*cap.off)
        avg.markup.off.nocap[market] <- weighted.mean(markup.off, w.off*(1 - cap.off))

        weights.on[[market]]  <- w.on
        weights.off[[market]] <- w.off

        all.mc.on[[market]]  <- rMC.m$costs.on
        all.mc.off[[market]] <- rMC.m$costs.off

        markup.on.zip[[market]]  <- markup.on
        markup.off.zip[[market]] <- markup.off

        Cap.on[[market]] <- cap.on
        Cap.off[[market]] <- cap.off
    }

    # Markup level averages
    markup.on.zip  <- Reduce(c, markup.on.zip)
    markup.off.zip <- Reduce(c, markup.off.zip)

    cap.on  <- Reduce(c, Cap.on)
    cap.off <- Reduce(c, Cap.off)
    w.on  <- Reduce(c, weights.on)
    w.off <- Reduce(c, weights.off)

    all.mc.on  <- Reduce(c, all.mc.on)
    all.mc.off <- Reduce(c, all.mc.off)

    common.zips <- intersect(names(markup.on.zip), names(markup.off.zip))
    markup.on.zip <- markup.on.zip[common.zips]
    markup.off.zip <- markup.off.zip[common.zips]

    # Compare markups across places with and without caps, and between
    # online and offline orders
    # SD across ZIP codes
    # Row: online/offline
    # Column: Cap/No cap
    cap.off <- cap.off[sub('[ic]$', '', common.zips)]
    w.off   <- w.off[common.zips]
    cap.on  <- cap.on[sub('[ic]$', '', common.zips)]
    w.on    <- w.on[common.zips]

    cap.col <- c(weighted.mean(markup.off.zip, w = cap.off*w.off),
                 weighted.mean(markup.on.zip,  w = cap.on*w.on))
    nocap.col <- c(weighted.mean(markup.off.zip, w = (1 - cap.off)*w.off),
                   weighted.mean(markup.on.zip, w = (1 - cap.on)*w.on))

    cap.sd <- c(weighted.sd(markup.off.zip, w = cap.off*w.off),
                weighted.sd(markup.on.zip,  w = cap.on*w.on))
    nocap.sd <- c(weighted.sd(markup.off.zip, w = (1 - cap.off)*w.off),
                  weighted.sd(markup.on.zip,  w = (1 - cap.on)*w.on))

    Markup.means <- data.frame(name = c('Direct', 'Platform'), NoCap = nocap.col, Cap = cap.col)
    Markup.sds   <- data.frame(name = c('Direct', 'Platform'), NoCap = nocap.sd,  Cap = cap.sd)
    Markup.combo <- Markup.means
    for (x in c('NoCap', 'Cap')){
        Markup.means[[x]] <- sprintf('%0.2f', Markup.means[[x]])
        Markup.sds[[x]]   <- sprintf('%0.2f', Markup.sds[[x]])
        Markup.combo[[x]] <- paste0(Markup.means[[x]], '$\\pm$', Markup.sds[[x]])
    }

    # Produce a similar table with marginal cost estimates
    cap.off <- cap.off[sub('[ic]$', '', common.zips)]
    w.off   <- w.off[common.zips]
    cap.on  <- cap.on[sub('[ic]$', '', common.zips)]
    w.on    <- w.on[common.zips]

    mc.off <- all.mc.off[names(w.off)]
    mc.on  <- all.mc.on[names(w.on)]

    cap.col <- c(weighted.mean(mc.off, w = cap.off*w.off),
                 weighted.mean(mc.on,  w = cap.on*w.on))
    nocap.col <- c(weighted.mean(mc.off, w = (1 - cap.off)*w.off),
                   weighted.mean(mc.on, w = (1 - cap.on)*w.on))

    cap.sd <- c(weighted.sd(mc.off, w = cap.off*w.off),
                weighted.sd(mc.on,  w = cap.on*w.on))
    nocap.sd <- c(weighted.sd(mc.off, w = (1 - cap.off)*w.off),
                  weighted.sd(mc.on,  w = (1 - cap.on)*w.on))

    MC.means <- data.frame(name = c('Direct', 'Platform'), NoCap = nocap.col, Cap = cap.col)
    MC.sds   <- data.frame(name = c('Direct', 'Platform'), NoCap = nocap.sd,  Cap = cap.sd)
    MC.combo <- MC.means
    for (x in c('NoCap', 'Cap')){
        MC.means[[x]] <- sprintf('%0.2f', MC.means[[x]])
        MC.sds[[x]]   <- sprintf('%0.2f', MC.sds[[x]])
        MC.combo[[x]] <- paste0(MC.means[[x]], '$\\pm$', MC.sds[[x]])
    }

    outputs <- list(Markup.means = Markup.means,
                    Markup.sds   = Markup.sds,
                    Markup.combo = Markup.combo,
                    MC.means     = MC.means,
                    MC.sds       = MC.sds,
                    MC.combo     = MC.combo)
    return(outputs)
}


main()
