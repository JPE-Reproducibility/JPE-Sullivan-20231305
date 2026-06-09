library(Matrix)
library(EconTools)
library(FoodDeliveryTools)
library(wesanderson)

source('code/CF/welfare_calculations.R')

main <- function(){

    cap.levels <- seq(from = 15, to = 40, by = 1)

    nsim.suffix <- '_nsim50'

    # Specify paths
    outdir <- sprintf('output/CF_feefirst/analysis%s', nsim.suffix)
    outpath.dat <- sprintf('%s/cap_by_county.csv',      outdir)
    outpath.reg <- sprintf('%s/cap_diff_regression.csv', outdir)
    create.dir(outdir)
    outpath.all <- sprintf('%s/vary_cap_all_markets.pdf', outdir)
    outpath.dist <- sprintf('%s/opt_cap_dist.pdf', outdir)

    outpath.reg.variety <- sprintf('%s/variety_regression.csv', outdir)
    outpath.density.effects <- sprintf('%s/density_regression.csv', outdir)

    ## Fees, share of orders on platforms,
    outpath.fee   <- sprintf('%s/fee_by_cap_level.pdf',    outdir)
    outpath.J     <- sprintf('%s/Jshr_by_cap_level.pdf',   outdir)
    outpath.sales <- sprintf('%s/Sratio_by_cap_level.pdf', outdir)

    outpath.cap15 <- sprintf('%s/effects_15pct_cap.csv', outdir)

    # Load cbsa codes
    inpaths    <- collect.inpaths(nsim.suffix)
    cbsa.codes <- read.dat(inpaths$inpath.cbsa.codes)

    geo <- load.geo(inpaths$inpath.geo)
    geo <- geo[geo$is.zcta, ]

    # Load fixed cost estimates
    FC.est <- readRDS(inpaths$inpath.FC.est)

    # Load eqm objects
    load.pMC <- TRUE
    eqm.objs <- load.all.eqm.objs.feefirst(nsim.suffix, load.pMC)

    eqm.objs.co <- eqm.objs$county
    markets <- names(eqm.objs.co)

    # Specify directory containing results
    base.dir <- sprintf('output/CF_feefirst/spec%s', nsim.suffix)

    # Load results
    suffix <- '_take2'
    BL.pattern <- 'cap30'
    BL <- load.eqm.results(BL.pattern, base.dir, cbsa.codes, suffix = suffix)
    assert.complete.names(names(BL), markets,
                          what = sprintf('cap30%s baseline results in %s', suffix, base.dir),
                          hint = 'Run code/CF/run_all_markets_cap.R first.')
    Cap <- list()
    for (k in 1:length(cap.levels)){
        lvl <- cap.levels[k]
        cap.pattern <- sprintf('cap%d', lvl)
        Cap[[k]] <- load.eqm.results(cap.pattern, base.dir, cbsa.codes, suffix = suffix)
        assert.complete.names(names(Cap[[k]]), markets,
                              what = sprintf('cap%d%s results in %s', lvl, suffix, base.dir),
                              hint = 'Run code/CF/run_all_markets_cap.R first.')
    }

    # Compute changes for the level
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

    w.dat <- list()
    for (market in markets){
        Results.m <- Results[[market]]
        code.m <- cbsa.codes$cbsa[which(cbsa.codes$CBSA_name == market)]
        outpath <- sprintf('%s/vary_cap_%s.pdf', outdir, code.m)
        w.dat[[market]] <- make.market.plot(Results.m, cap.levels, outpath)
    }

    ## Aggregate plot
    add.vars <- c('CW', 'PP', 'RP', 'tot', 'S')
    w.dat.sub <- list()
    for (market in markets){
        w.dat.sub[[market]] <- w.dat[[market]][, add.vars]
    }
    all.markets <- Reduce('+', lapply(w.dat.sub, as.matrix))
    all.markets <- as.data.frame(all.markets)
    all.markets$cap <- w.dat[[1]]$cap
    data.to.plot(all.markets, outpath.all, smooth = TRUE)

    # Save table of gains relative to 30%
    tot.rel <- all.markets$tot - all.markets$tot[which(all.markets$cap == 30)]
    tot.rel <- tot.rel/all.markets$S
    tab <- data.frame(cap = all.markets$cap, welfare = tot.rel)
    write.dat(tab, sprintf('%s/welfare_by_cap_table.csv', outdir))

    # Compute in each county the commission level maximizing platform
    # profits and total welfare
    cap.soc  <- c()
    cap.priv <- c()
    cap.cons <- c()

    # Some additional variables that are worth saving
    pop   <- c()
    J.tot <- c()
    K.bar <- c()
    Div   <- c()
    Div2  <- c()
    b.bar <- c()
    S0    <- c()
    S1    <- c()
    HHI   <- c()
    HInc  <- c()

    # Effects of lowering commission from 30%
    J.chg       <- c()
    FC.chg      <- c()
    Variety.chg <- c()
    Sales.BL    <- c()
    Sales.chg   <- c()

    for (market in markets){
        Results.m <- Results[[market]]
        counties <- names(Results.m$BL)

        for (co in counties){
            Results.co <- Results.m$cap[[co]]
            CW <- c()
            PP <- c()
            RP <- c()
            for (k in 1:length(cap.levels)){
                CW[k] <- Results.co[[k]]$CW$total.EU.dollar
                PP[k] <- sum(Results.co[[k]]$p.profit)
                RP[k] <- Results.co[[k]]$r.profit['total']
            }
            Tot <- CW + PP + RP
            k.soc  <- which.max(Tot)
            k.priv <- which.max(PP)
            k.cons <- which.max(CW)

            cap.soc[co]  <- cap.levels[k.soc]
            cap.priv[co] <- cap.levels[k.priv]
            cap.cons[co] <- cap.levels[k.cons]

            # Additional variables
            ## Requisite data objects/estimates
            dat.m <- eqm.objs.co[[market]][[co]]$dat.m
            kappa <- eqm.objs.co[[market]][[co]]$kappa
            eqm   <- BL[[market]][[co]]

            ## Demographics
            pop[co] <- sum(dat.m$buy.m$count)
            HInc[co] <- weighted.mean(dat.m$buy.m$high_income, dat.m$buy.m$count)

            J.mat <- do.call(rbind, dat.m$J.G.1.m)
            J.tot[co] <- sum(J.mat)

            K.bar[co] <- mean(c(kappa$K.base$chain[2:dat.m$nportfolios],
                              kappa$K.base$indep[2:dat.m$nportfolios]))*1e5

            sales.off <- eqm$FP$Sales.platform[1]
            sales.on  <- eqm$FP$Sales.platform[2:length(eqm$FP$Sales.platform)]
            shares.on <- sales.on/sum(sales.on)
            S0[co]    <- sales.off
            S1[co]    <- sum(sales.on)
            HHI[co]   <- sum(shares.on^2)

            # b.bar: remove a restaurant from 0000 and put it in 1111
            # what is the effect on consumer welfare per platform order
            C0    <- eqm$C
            J.G0  <- dat.m$J.G.1
            Rhos0 <- eqm$FP$Rhos
            W0 <- compute.consumer.welfare(dat.m, C0, J.G0, Rhos0, no.logit = FALSE)
            J.G1 <- J.G0
            h <- 1
            NG <- dat.m$nportfolios
            for (z in names(J.G1)){
                if (J.G1[[z]][1] >= h){
                    J.G1[[z]][1]  <- J.G1[[z]][1] - h
                    J.G1[[z]][NG] <- J.G1[[z]][NG] + h
                }
            }
            W1 <- compute.consumer.welfare(dat.m, C0, J.G1, Rhos0, no.logit = FALSE)
            b.bar[co] <- (W1$total.EU.dollar - W0$total.EU.dollar)/S0[co]*J.tot[co]

            # Change in fixed costs from a commission reduction
            k0 <- which(cap.levels == 30)
            k1 <- which(cap.levels == 29)
            J0 <- Cap[[k0]][[market]][[co]]$FP$J.G.1
            J1 <- Cap[[k1]][[market]][[co]]$FP$J.G.1
            FC0 <- sum(sapply(names(J0), function(z) sum(J0[[z]]*kappa$K[[z]])))
            FC1 <- sum(sapply(names(J0), function(z) sum(J1[[z]]*kappa$K[[z]])))
            FC.chg[co] <- (FC1 - FC0)/pop[co]

            # Change in variety benefit
            C <- Cap[[k0]][[market]][[co]]$C
            R <- Cap[[k0]][[market]][[co]]$R
            Rhos <- Cap[[k0]][[market]][[co]]$FP$Rhos
            W0 <- compute.consumer.welfare(dat.m, C, J0, Rhos, no.logit = FALSE)
            W1 <- compute.consumer.welfare(dat.m, C, J1, Rhos, no.logit = FALSE)
            Variety.chg[co] <- (W1$total.EU.dollar - W0$total.EU.dollar)/pop[co]

            # Change in sales
            ## load in fees, commissions
            for (z in names(dat.m$fees)){
                dat.m$fees[[z]] <- C
            }
            for (z in names(dat.m$comm)){
                dat.m$comm[[z]] <- R
            }
            for (z in names(J0)){
                dat.m$J.G.1.m[[z]] <- J0[[z]]
            }
            sales0 <- compute.zip.sales(dat.m, more.outputs = TRUE, Rhos = Rhos)
            sales0 <- sum(sales0$Sales.platform[2:length(sales0$Sales.platform)])
            for (z in names(J1)){
                dat.m$J.G.1.m[[z]] <- J1[[z]]
            }
            sales1 <- compute.zip.sales(dat.m, more.outputs = TRUE, Rhos = Rhos)
            sales1 <- sum(sales1$Sales.platform[2:length(sales1$Sales.platform)])
            Sales.chg[co] <- (sales1 - sales0)/pop[co]
            Sales.BL[co] <- sales1

            # Diversion ratio
            # load in fees
            for (z in names(dat.m$fees)){
                dat.m$fees[[z]] <- eqm$C
            }
            for (z in names(dat.m$comm)){
                dat.m$comm[[z]] <- eqm$R
            }
            Rhos <- eqm$FP$Rhos
            sales.dat0 <- compute.zip.sales(dat.m, Rhos = Rhos, opts = opts,
                                            more.outputs = TRUE)
            # Perturb fees
            h <- 0.01
            for (z in names(dat.m$fees)){
                dat.m$fees[[z]] <- eqm$C + h
            }
            sales.dat1 <- compute.zip.sales(dat.m, Rhos = Rhos, opts = opts,
                                            more.outputs = TRUE)
            NF <- dat.m$nplatforms
            chg.off <- sales.dat1$Sales.platform[1] - sales.dat0$Sales.platform[1]
            chg.on <- sum(sales.dat1$Sales.platform[2:NF]) - sum(sales.dat0$Sales.platform[2:NF])
            Div[co] <- -chg.off/chg.on

            # Alternative diversion measure
            ### Eliminate platforms
            for (z in names(dat.m$fees)){
                dat.m$fees[[z]] <- eqm$C + 100
            }
            sales.dat2 <- compute.zip.sales(dat.m, Rhos = Rhos, opts = opts,
                                            more.outputs = TRUE)
            S.platform <- sum(sales.dat0$Sales.platform[2:5])
            S.divert   <- sales.dat2$Sales.platform[1] - sales.dat0$Sales.platform[1]
            Div2[co] <- S.divert/S.platform
        }
    }

    # Compute some variables that explain cannibalization
    ## Density data
    inpath.dens <- 'data/ACS/processed/ACS_nearby.csv'
    dens <- read.dat(inpath.dens, colClasses = c('zcta' = 'character'))
    dens <- dens[, c('zcta', 'population')]
    colnames(dens) <- c('zip', 'pop_nearby')
    ## Initialize outputs
    nresto      <- c()
    uptake.BL   <- c()
    on.share    <- c()
    r.shr       <- c()
    on.pc       <- c()
    off.pc      <- c()
    avg.J.nearby <- c()
    avg.J.dens   <- c()
    Pop.Dens     <- c()
    Shr.Hi       <- c()
    Shr.Young    <- c()
    Shr.Married  <- c()
    Shr.YU <- c()
    for (market in markets){
        eqm.objs.m <- eqm.objs.co[[market]]
        counties <- names(eqm.objs.m)
        for (co in counties){
            eqm.objs.j <- eqm.objs.m[[co]]
            dat.m <- eqm.objs.j$dat.m
            buy.m <- dat.m$buy.m
            J.BL <-  colSums(do.call(rbind, dat.m$J.G.1.m))
            nresto[co] <- sum(do.call(c, dat.m$J.G.1.m))
            uptake.BL[co] <- 1 - J.BL[1]/nresto[co]

            S <- BL[[market]][[co]]$FP$Sales.platform
            shrs <- S[2:5]
            shrs <- shrs/sum(shrs)

            on.share[co] <- 1 - S[1]/sum(S)
            on.pc[co]    <- sum(S[2:5])/pop[co]
            off.pc[co]   <- S[1]/pop[co]
            r.shr[co]    <- sum(S)/pop[co]

            pop.nearby   <- c()
            pop.zip      <- c()
            resto.nearby <- c()
            zip.weight   <- c()
            for (z in names(dat.m$fees)){
                idx <- which(geo$zcta == z)
                pop.zip[z] <- geo$pop[idx]/1e6

                idx <- which(dens$zip == z)
                pop.nearby[z] <- dens$pop_nearby[idx]
                Z.z <- dat.m$zip.map[[z]]
                resto.nearby[z] <- sum(sapply(Z.z, function(zz) sum(dat.m$J.G.m[[zz]])))
                zip.weight[z] <- sum(dat.m$buy.zip[[z]]$count)
            }
            Pop.Dens[co] <- weighted.mean(pop.nearby/1e6, pop.zip/1e6)
            avg.J.nearby[co] <- weighted.mean(resto.nearby, zip.weight)
            avg.J.dens[co]   <- weighted.mean(resto.nearby/pop.nearby, zip.weight)

            Shr.Hi[co]      <- weighted.mean(dat.m$buy.m$high_income, dat.m$buy.m$count)
            Shr.Young[co]   <- weighted.mean(dat.m$buy.m$young, dat.m$buy.m$count)
            Shr.Married[co] <- weighted.mean(dat.m$buy.m$married, dat.m$buy.m$count)
            Shr.YU[co] <- weighted.mean(dat.m$buy.m$young*(1 - dat.m$buy.m$married), dat.m$buy.m$count)
        }
    }

    # Construct a county-specific dataset
    dat <- data.frame(county = names(Div),
                      cap.soc, cap.priv, cap.cons,
                      pop  ,
                      J.tot,
                      K.bar,
                      Div  ,
                      Div2,
                      b.bar,
                      S0   ,
                      S1   ,
                      HHI,
                      HInc,
                      FC.chg,
                      Variety.chg,
                      Sales.BL,
                      Sales.chg,
                      nresto,
                      uptake.BL,
                      on.share ,
                      r.shr,
                      on.pc,
                      off.pc,
                      avg.J.nearby,
                      avg.J.dens,
                      Pop.Dens,
                      Shr.Hi,
                      Shr.Young,
                      Shr.Married,
                      Shr.YU)
    dat$cap.cons <- dat$cap.cons/100
    dat$cap.soc  <- dat$cap.soc/100
    dat$cap.priv <- dat$cap.priv/100


    dat$saturation <- log(dat$J.tot/dat$pop)
    dat$log.b      <- log(dat$b.bar)
    dat$cap.diff   <- dat$cap.priv - dat$cap.soc
    dat$inside.shr <- dat$S1/(dat$S1 + dat$S0)
    dat$log.pop    <- log(dat$pop)
    dat$log.var    <- log(dat$Variety.chg)
    dat$log.FC     <- log(dat$FC.chg)
    dat$log.sales  <- log(dat$Sales.chg)
    dat$var.sales.diff <- log(dat$Variety.chg/dat$Sales.chg)
    dat$var.sales.ratio <- dat$Variety.chg/dat$Sales.chg

    dat$Variety.alt <-dat$Variety.chg*dat$pop/dat$S1
    dat$Sales.alt   <-dat$Sales.chg*dat$pop/dat$S1

    dat$FC.alt <- dat$FC.chg*dat$pop/dat$S1

    dat$log.Div <- log(dat$Div)
    dat$log.FC.chg <- log(dat$FC.chg)
    dat$log.Variety.chg <- log(dat$Variety.chg)

    # Regression
    regressors <- c('Div2', 'FC.chg',
                    'Variety.chg')
    tab <- run.reg('cap.soc', regressors, dat)

    write.dat(dat, outpath.dat)
    # The paper's table reports est/se and the R-squareds; drop the t-stat
    write.dat(tab[, setdiff(colnames(tab), 't')], outpath.reg)

    # What explains heterogeneity in Variety effects?
    dat$log.Variety <- log(dat$Variety.chg)
    dat$log.J.dens <- log(dat$avg.J.dens)
    dat$log.J.nearby <- log(dat$avg.J.nearby)
    dat$log.Pop.Dens <- log(dat$Pop.Dens)

    regressors <- c('log.J.nearby',  'Shr.YU')
    tab.var <- run.reg('Variety.chg', regressors, dat)
    write.dat(tab.var, outpath.reg.variety)

    reg.a <- summary(lm(Variety.chg ~ log.J.nearby, dat))
    reg.b <- summary(lm(Variety.chg ~ log.Pop.Dens, dat))
    reg.c <- summary(lm(Div2 ~ log.J.nearby, dat))
    reg.d <- summary(lm(Div2 ~ log.Pop.Dens, dat))
    reg.e <- summary(lm(cap.soc ~ log.J.nearby, dat))
    reg.f <- summary(lm(cap.soc ~ log.Pop.Dens, dat))

    proc.est <- function(x){
        x1 <- sprintf('%0.3f', x[1])
        x2 <- sprintf('\\footnotesize (%0.3f)', x[2])
        x.new <- c(x1, x2)
        return(x.new)
    }

    col.a <- c(proc.est(reg.a$coefficients[2, 1:2]),
              '', '',
              sprintf('%0.2f', reg.a$r.squared))
    col.b <- c('', '',
               proc.est(reg.b$coefficients[2, 1:2]),
               sprintf('%0.2f', reg.b$r.squared))
    col.c <- c(proc.est(reg.c$coefficients[2, 1:2]),
              '', '',
              sprintf('%0.2f', reg.c$r.squared))
    col.d <- c('', '',
               proc.est(reg.d$coefficients[2, 1:2]),
               sprintf('%0.2f', reg.d$r.squared))
    col.e <- c(proc.est(reg.e$coefficients[2, 1:2]),
               '', '',
               sprintf('%0.2f', reg.e$r.squared))
    col.f <- c('', '',
               proc.est(reg.f$coefficients[2, 1:2]),
               sprintf('%0.2f', reg.f$r.squared))

    tab <- data.frame(var = c('log(average \\# restaurants $<$ 5 miles)', '',
                              'log(population within 5 miles)', '',
                              '$R^2$'),
                      regA = col.a,
                      regB = col.b,
                      regC = col.c,
                      regD = col.d,
                      regE = col.e,
                      regF = col.f)
    write.dat(tab, outpath.density.effects)

    regressors <- c('log.J.nearby',  'log.Pop.Dens')
    tab.var <- run.reg('Variety.chg', regressors, dat)

    # What explains the cannibalization rate?
    dat$J.by.pop <- dat$nresto/dat$pop
    reg  <- summary(lm(log(Div) ~  log(r.shr) + log(on.share) + Shr.Young + Shr.Hi + Shr.Married, dat))
    reg2 <- summary(lm(log(r.shr) ~  log(avg.J.nearby), dat))

    tab.cannibal <- data.frame(var = c('log(restaurant orders per capita)',
                                       'log(online share of restaurant orders)',
                                       'Share $<$ 35yo',
                                       'Share HH inc $>$ 40k',
                                       'Share married',
                                       '$R^2$'),
                               est = c(sprintf('%0.3f', reg$coefficients[2:6, 'Estimate']),
                                       sprintf('%0.2f', reg$r.squared)),
                               se  = c(sprintf('\\small (%0.3f)',
                                             reg$coefficients[2:6, 'Std. Error']), ''))
    tab.aux <- data.frame(var = c('log(\\# restaurants $<$ 5 miles)',
                                  '$R^2$'),
                          est = sprintf('%0.2f', c(reg2$coefficients[2, 1], reg2$r.squared)),
                          se  = c(sprintf('%0.2f', reg2$coefficients[2, 2]), ''))
    write.dat(tab.cannibal, sprintf('%s/explaining_cannibal.csv', outdir))
    write.dat(tab.aux, sprintf('%s/supporting_cannibal.csv', outdir))

    # Distribution of optimal caps
    Q <- c(0.10, 0.25, 0.50, 0.75, 0.90)
    dat$cap.gap <- dat$cap.priv - dat$cap.soc
    IQR.soc <- sapply(Q, function(q) weighted.quantile(dat$cap.soc,  q, dat$pop))*100
    IQR.pri <- sapply(Q, function(q) weighted.quantile(dat$cap.priv, q, dat$pop))*100
    IQR.gap <- sapply(Q, function(q) weighted.quantile(dat$cap.gap,  q, dat$pop))*100

    minmax.soc <- c(min(dat$cap.soc),  max(dat$cap.soc))
    minmax.pri <- c(min(dat$cap.priv), max(dat$cap.priv))
    minmax.gap <- c(min(dat$cap.gap), max(dat$cap.gap))

    colours <- wes_palette('Cavalcanti1', 4)[c(3, 4, 1)]


    pdf(outpath.dist, height = 3, width = 5.8)
    par(mar = c(5, 5, 1, 1))
    plot(1:3, c(-100, -100, -100), xlim = c(0.5, 3.5), ylim = c(0.0, 0.40)*100,
         axes = FALSE, ylab = 'Commission rate (%)',
         xlab = 'Welfare component maximized')
    grid()
    h <- 0.25
    rect(xleft = 1 - h, xright = 1 + h, ybottom = IQR.pri[2], ytop = IQR.pri[4],
         col = colours[1])
    rect(xleft = 2 - h, xright = 2 + h, ybottom = IQR.soc[2], ytop = IQR.soc[4],
         col = colours[2])
    rect(xleft = 3 - h, xright = 3 + h, ybottom = IQR.gap[2], ytop = IQR.gap[4],
         col = colours[3])

    ## add whiskers
    segments(1, IQR.pri[4], y1 = IQR.pri[5])
    segments(1, IQR.pri[1], y1 = IQR.pri[2])
    segments(x0 = 1 - h,   y0 = IQR.pri[3], x1 = 1 + h, lwd = 1.25)
    segments(x0 = 1 - h/3, y0 = IQR.pri[5], x1 = 1 + h/3)
    segments(x0 = 1 - h/3, y0 = IQR.pri[1], x1 = 1 + h/3)

    segments(x0 = 2,       y0 = IQR.soc[4], y1 = IQR.soc[5])
    segments(x0 = 2,       y0 = IQR.soc[1], y1 = IQR.soc[2])
    segments(x0 = 2 - h,   y0 = IQR.soc[3], x1 = 2 + h, lwd = 1.25)
    segments(x0 = 2 - h/3, y0 = IQR.soc[5], x1 = 2 + h/3)
    segments(x0 = 2 - h/3, y0 = IQR.soc[1], x1 = 2 + h/3)

    segments(3, IQR.gap[4], y1 = IQR.gap[5])
    segments(3, IQR.gap[1], y1 = IQR.gap[2])
    segments(x0 = 3 - h, y0 = IQR.gap[3], x1 = 3 + h, lwd = 1.25)
    segments(x0 = 3 - h/3, y0 = IQR.gap[5], x1 = 3 + h/3)
    segments(x0 = 3 - h/3, y0 = IQR.gap[1], x1 = 3 + h/3)

    axis(1, at = 1:3, labels = c('Platform profit', 'Total welfare', 'Difference'))
    axis(2)

    dev.off()

    ## Table version
    Tab <- do.call(rbind, list(IQR.pri, IQR.soc, IQR.gap))
    Tab <- as.data.frame(Tab)
    colnames(Tab) <- paste0('Q', round(Q*100))
    for (k in 1:ncol(Tab)){
        Tab[, k] <- sprintf('%0.0f', Tab[, k])
    }
    Tab$var <- c('Platform-profit maximizing',
                 'Total-welfare maximizing',
                 'Difference')
    for (k in colnames(Tab)){
        Tab[, k] <- sub('^-0$', '0', Tab[, k])
    }
    outpath.dist.tab <- sub('pdf$', 'csv', outpath.dist)
    write.dat(Tab, outpath.dist.tab)

    # Fee changes
    Fees <- list()
    S0   <- list()
    S1   <- list()
    J0   <- list()
    J1   <- list()
    J.sh <- list()

    markets <- names(BL)
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

    pdf(outpath.J, height = H, width = W)
    plot(cap.levels, J.shr, type = 'l', axes = FALSE,
         xlab = 'Regulated commission level (%)',
         ylab = 'Share of restaurants online',
         lwd = 2, ylim = c(0.40, 0.65), xlim = c(15, 40))
    grid()
    axis(1)
    axis(2)
    abline(v = 30, lty = 2, col = 'grey40')
    dev.off()

    y <- S1/(S0 + S1)
    pdf(outpath.sales, height = H, width = W)
    y.smooth <- smoother(cap.levels, y)
    plot(cap.levels, y.smooth, type = 'l', axes = FALSE,
         xlab = 'Regulated commission level (%)',
         ylab = 'Share of orders on platforms',
         lwd = 2, xlim = c(15, 40), ylim = c(0.14, 0.22))
    grid()
    axis(1)
    axis(2)
    abline(v = 30, lty = 2, col = 'grey40')
    dev.off()

    # Save the numbers for 15% to be referenced in the main text
    idx.15 <- which(cap.levels == 15)
    idx.30 <- which(cap.levels == 30)
    fees.comp   <- Fees[c(idx.15, idx.30)]
    J.comp      <- J.shr[c(idx.15, idx.30)]
    shr.on.comp <- y[c(idx.15, idx.30)]
    mat <- do.call(rbind, list(fees.comp, J.comp, shr.on.comp))
    mat <- as.data.frame(mat)
    colnames(mat) <- c('comm15', 'comm30')
    mat$outcome <- c('fee', 'J', 'shr_sales_on')

    # add welfare
    wtab <- all.markets[which(all.markets$cap %in% c(15, 30)), c('CW', 'PP', 'RP', 'tot', 'S')]
    wtab$CW <- wtab$CW/wtab$S[2]
    wtab$PP <- wtab$PP/wtab$S[2]
    wtab$RP <- wtab$RP/wtab$S[2]
    wtab$tot <- wtab$tot/wtab$S[2]
    wtab <- as.data.frame(t(wtab))
    colnames(wtab) <- c('comm15', 'comm30')
    wtab$outcome <- rownames(wtab)
    mat <- dplyr::bind_rows(mat, wtab)

    mat$diff <- mat$comm15 - mat$comm30
    mat$diff.rel <- (mat$comm15/mat$comm30 - 1)*100

    write.csv(mat, outpath.cap15, row.names = FALSE, quote = FALSE)
    
}

make.market.plot <- function(Results.m, cap.levels, outpath){

    Results.cap <- Results.m$cap
    ## For each level of commission cap, extract consumer welfare
    counties <- names(Results.m$BL)

    CW <- c()
    PP <- c()
    RP <- c()
    S  <- c()
    for (k in 1:length(cap.levels)){
        CW.k <- c()
        PP.k <- c()
        RP.k <- c()
        S.k  <- c()
        for (co in counties){
            CW.k[co] <- Results.cap[[co]][[k]]$CW$total.EU.dollar
            PP.k[co] <- sum(Results.cap[[co]][[k]]$p.profit)
            RP.k[co] <- Results.cap[[co]][[k]]$r.profit['total']
            S.k[co]  <- Results.cap[[co]][[k]]$S
        }
        CW[k] <- sum(CW.k)
        PP[k] <- sum(PP.k)
        RP[k] <- sum(RP.k)
        S[k]  <- sum(S.k)
    }
    w.dat.m <- data.frame(CW = CW, PP = PP, RP = RP)
    w.dat.m$tot <- rowSums(w.dat.m)

    ## plot changes relative to a cap of 30
    w.dat.m$cap <- cap.levels
    w.dat.m$S   <- S

    data.to.plot(w.dat.m, outpath)

    return(w.dat.m)
}

data.to.plot <- function(w.dat.m, outpath, smooth = FALSE){

    # Graphical parameters
    ## Plot height and width
    H <- 6.5
    W <- 8
    ## Line widths
    LWD <- 3
    ## Colours
    colours <- RColorBrewer::brewer.pal(4, 'Dark2')
    colours[1] <- 'black'
    ## Line styles
    LTYs <- c(1, 2, 6, 4)


    if (smooth){
        w.dat.m$tot <- smoother(w.dat.m$cap, w.dat.m$tot)
        w.dat.m$RP  <- smoother(w.dat.m$cap, w.dat.m$RP)
        w.dat.m$PP  <- smoother(w.dat.m$cap, w.dat.m$PP)
        w.dat.m$CW  <- smoother(w.dat.m$cap, w.dat.m$CW)
    }

    # Relative versions of the variables
    idx.30 <- which(w.dat.m$cap == 30)
    S0 <- w.dat.m$S[idx.30]
    for (k in c('CW', 'PP', 'RP', 'tot')){
        new.var <- paste0(k, '_relchg')
        w.dat.m[, new.var] <- (w.dat.m[, k] - w.dat.m[idx.30, k])/S0
    }

    # Choose y-axis limits
    combo <- c(w.dat.m$CW_relchg, w.dat.m$PP_relchg,
               w.dat.m$RP_relchg, w.dat.m$tot_relchg)
    ylim <- c(min(combo), max(combo))
    yfac <- 0.1
    ypoints <- seq(from = floor(min(ylim)/yfac)*yfac, to = ceiling(max(ylim)/yfac)*yfac,
                   by = yfac)

    pdf(outpath, width = W, height = H)
    par(mar = c(5, 5, 2, 2))
    plot(w.dat.m$cap, w.dat.m$tot_relchg, type = 'l', ylim = ylim,
         lwd = LWD, col = colours[1], axes = FALSE, cex.lab = 1.4,
         xlab = 'Regulated commission level (%)',
         ylab = 'Welfare change ($/baseline platform orders)')
    grid()
    lines(w.dat.m$cap, w.dat.m$RP_relchg, col = colours[2], lwd = LWD, lty = LTYs[2])
    lines(w.dat.m$cap, w.dat.m$PP_relchg, col = colours[3], lwd = LWD, lty = LTYs[3])
    lines(w.dat.m$cap, w.dat.m$CW_relchg, col = colours[4], lwd = LWD, lty = LTYs[4])

    abline(h = 0)
    axis(1, cex.axis = 1.2)
    axis(2, at = ypoints, cex.axis = 1.2)
    abline(v = 30, lty = 2, col = 'grey40')

    legend(x = 'topright', legend = c('Total welfare', 'Restaurant profits',
                                      'Platform profits', 'Consumer welfare'),
           lty = LTYs, col = colours, lwd = c(LWD, LWD, LWD, LWD),
           bg = 'white', cex = 1.5)

    dev.off()
    
    # Table version for reference in the draft
    if (grepl('all_markets', outpath)){
        tab.out <- w.dat.m[, c('cap', grep('relchg', colnames(w.dat.m), value = TRUE))]
        write.dat(tab.out, file = sub('pdf$', 'csv', outpath))
    }
}

run.reg <- function(outcome, regressors, dat, weight = FALSE){

    if (weight){
        w <- dat$Sales.BL
    } else {
        w <- rep(1, times = nrow(dat))
    }

    RHS <- Reduce(function(x, y) sprintf('%s + %s', x, y), regressors)
    fmla <- as.formula(sprintf('%s ~ %s', outcome, RHS))
    reg <- lm(fmla, dat, weights = w)
    ## Pairwise explanatory power
    r2.bivar <- c()
    for (x in regressors){
        fmla <- as.formula(sprintf('%s ~ %s', outcome, x))
        r2.bivar[x] <- summary(lm(fmla, dat, weights = w))$r.squared
    }
    ## R2 when the covariate is dropped
    r2.partial <- c()
    for (x in regressors){
        regressors.mx <- setdiff(regressors, x)
        RHS <- Reduce(function(x, y) sprintf('%s + %s', x, y), regressors.mx)
        fmla <- as.formula(sprintf('%s ~ %s', outcome, RHS))
        r2.partial[x] <- summary(lm(fmla, dat, weights = w))$r.squared
    }

    # Names of variables
    vnames <- c('Intercept', regressors)
    vnames[vnames == 'saturation'] <- 'Saturation'
    vnames[vnames %in% c('log.b', 'b.bar')] <- 'Consumer benefit from restaurant adoption'
    vnames[vnames == 'Div'] <- 'Diversion ratio'
    vnames[vnames == 'Div2'] <- 'Offline business stealing'
    vnames[vnames == 'HInc'] <- 'High income share'
    vnames[vnames == 'FC.chg'] <- 'Fixed cost change'
    vnames[vnames == 'Variety.alt'] <- 'Variety change'
    vnames[vnames == 'Variety.chg'] <- 'Variety change'
    vnames[vnames == 'Sales.alt'] <- 'Sales change'

    ## Table
    tab <- as.data.frame(summary(reg)$coefficients[, 1:3])
    colnames(tab) <- c('est', 'se', 't')
    tab$var <- vnames
    tab$est <- sprintf('%0.2f', tab$est)
    tab$se  <- sprintf('\\small (%0.2f)', tab$se)
    tab$t   <- sprintf('%0.2f', tab$t)
    tab$r2bi <- c('', sprintf('%0.2f', r2.bivar))
    tab$r2part <- c('', sprintf('%0.2f', r2.partial))

    # Add R2
    r2 <- sprintf('%0.2f', summary(reg)$r.squared)
    tab <- dplyr::bind_rows(tab, data.frame(var = '$R^2$', est = r2,
                                            se  = '', t = '',
                                            r2bi = '', r2part = ''))
    tab <- tab[, c('var', 'est', 'se', 't', 'r2bi', 'r2part')]

    # remove intercept
    tab <- tab[2:nrow(tab), ]
    return(tab)
}

smoother <- function(x, y){
    y.smooth <- c()
    h <- 1.25 # bandwidth
    for (k in 1:length(x)){
        if (x[k] < 25 | x[k] > 35){
            y.smooth[k] <- y[k]
        } else {
            idx <- which(abs(x - x[k]) <= 2)
            vals <- x[idx]
            weights <- dnorm((x[k] - vals)/h)
            weights <- weights/sum(weights)
            y.smooth[k] <- sum(y[idx]*weights)
        }
    }
    return(y.smooth)
}


main()

