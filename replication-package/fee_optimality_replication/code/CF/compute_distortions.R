# This version supersedes "compute_distortions_doordash.R"
library(Matrix)
library(FoodDeliveryTools)
library(EconTools)
library(parallel)


main <- function(){

    #== Specify paths ==#
    nsim.suffix <- '_nsim50'
    inpaths <- collect.inpaths(nsim.suffix)
    # Equilibria
    CF.dir  <- sprintf('output/CF_feefirst/spec%s', nsim.suffix)
    # Outputs
    outdir <- sprintf('output/CF_feefirst/analysis%s', nsim.suffix)
    outpath.distort.tab <- sprintf('%s/distortions_table.csv', outdir)
    outpath.tab.approx  <- sprintf('%s/characterize_approx_formula.csv', outdir)
    outpath.explain     <- sprintf('%s/explain_c_distortions.csv', outdir)

    #== Loading and processing data ==#
    # CBSA codes
    cbsa.codes <- read.dat(inpaths$inpath.cbsa.codes)
    rownames(cbsa.codes) <- cbsa.codes$cbsa
    # Data objects
    load.pMC <- TRUE
    eqm.objs <- load.all.eqm.objs.feefirst(nsim.suffix, load.pMC)
    eqm.objs.co <- eqm.objs$county

    # Load competitive equilibria
    Eqm <- load.equilibria.priv.soc(CF.dir, cbsa.codes, monopoly = FALSE)
    BL  <- Eqm$BL
    soc <- Eqm$soc
    assert.complete.names(names(BL), names(eqm.objs.co),
                          what = sprintf('baseline/socopt equilibria in %s (markets)', CF.dir),
                          hint = 'Re-run the CF solvers for this spec.')

    #== Compute distortions ==#
    output <- compute.distortions(BL, soc, eqm.objs.co)

    #== Process distortions ==#
    C.priv <- list()
    C.soc  <- list()
    sales.online <- list()
    markets <- names(eqm.objs.co)
    for (market in markets){
        eqm.m <- BL[[market]]
        soc.m <- soc[[market]]
        counties <- names(eqm.m)
        for (co in counties){
            sales.online[[co]] <- eqm.m[[co]]$FP$Sales.platform[2:5]
            C.priv[[co]] <- eqm.m[[co]]$C
            C.soc[[co]]  <- soc.m[[co]]$C
        }
    }

    ## Compute the distortions for each platform
    NF <- length(output$Diffs[[1]])
    platforms <- names(output$Diffs[[1]])
    Distortions.agg     <- list()
    Wgt.f               <- c()
    compare.tot.distort <- list()
    Distort.mat     <- list()

    for (f in 1:NF){
        platform <- platforms[f]

        Diff.comp <- do.call(rbind, output$Diffs)[, f]
        C0.f <- do.call(rbind, C.priv)[, f]
        C1.f <- do.call(rbind, C.soc)[, f]

        # Extract matrix of distortions
        D.mat <- output$Distort.mat[[f]]

        # Comparison of total distortions
        tot.0 <- rowSums(D.mat[, c('market_power', 'off_bsteal', 'on_bsteal',
                                   'spence', 'displacement')])
        tot.1 <- rowSums(D.mat[, c('market_power', 'off_bsteal', 'on_bsteal',
                                   'spence', 'displacement', 'rival')])

        compare.tot.distort[[f]] <-
            data.frame(formula0 = tot.0,
                       formula1 = tot.1,
                       fee_diff = Diff.comp)
        compare.tot.distort[[f]]$platform <- platform

        # Compute weights
        wgts <- sapply(names(Diff.comp), function(co) sales.online[[co]][f])
        Wgt.f[f] <- sum(wgts)
        compare.tot.distort[[f]]$weight <- wgts

        #== Compute distortions for unadjusted displacement distortion ==#
        # Store all distortions
        Distort.mat[[platform]] <- as.data.frame(D.mat)

        # Alternative formula predictions
        Distort.mat[[platform]]$formulapred0 <- tot.0
        Distort.mat[[platform]]$formulapred1 <- tot.1

        Distort.mat[[platform]]$tot_gap     <- Diff.comp
        Distort.mat[[platform]]$other0      <- Diff.comp - Distort.mat[[platform]]$formulapred0
        Distort.mat[[platform]]$other1      <- Diff.comp - Distort.mat[[platform]]$formulapred1

        # Compute mean distortions
        mean.diff <- c()
        for (k in colnames(Distort.mat[[platform]])){
            mean.diff[k] <- weighted.mean(Distort.mat[[platform]][, k], wgts)
        }
        Distortions.agg[[platform]] <- mean.diff

        # Add some additional variables
        Distort.mat[[platform]]$county   <- names(Diff.comp)
        Distort.mat[[platform]]$platform <- platform
        Distort.mat[[platform]]$weight   <- wgts
    }

    # Produce table
    Distortions.tab <- generate.table(Distortions.agg, Wgt.f)

    # Write to file
    write.dat(Distortions.tab, outpath.distort.tab)

    # Summarize gap between sum of distortions and total fee difference
    compare.tot.distort <- do.call(dplyr::bind_rows, compare.tot.distort)

    max.lvl <- 10
    for (platform in platforms){
        idx <- which(compare.tot.distort$platform == platform &
                         abs(compare.tot.distort$formula1) <= max.lvl &
                         abs(compare.tot.distort$fee_diff) <= max.lvl)

        outpath <- sprintf('%s/compare_fee_diff_tot_%s.pdf', outdir, platform)
        pdf(outpath, height = 5, width = 5)
        plot(x = compare.tot.distort$formula1[idx],
             y = compare.tot.distort$fee_diff[idx], axes = FALSE,
             xlab = 'Fee gap predicted by distortion formula',
             ylab = 'Actual consumer fee gap',
             xlim = c(-max.lvl, max.lvl), ylim = c(-max.lvl, max.lvl))
        grid()
        axis(1)
        axis(2)
        reg <- lm(fee_diff ~ formula1, data = compare.tot.distort[idx, ],
                  weights = compare.tot.distort$weight[idx])
        abline(0, 1, lty = 2)
        text(x = -max.lvl + 2, y = max.lvl - 2, labels = bquote(R^2 == .(sprintf('%.2f', summary(reg)$r.squared))))

        dev.off()
    }

    # Add in sales weights and compute correlation
    reg.tot <- summary(lm(fee_diff ~ formula1, data = compare.tot.distort,
                           weights = compare.tot.distort$weight))
    # Save results
    reg.tab <- reg.tot$coefficients[, 1:2]
    reg.tab <- as.data.frame(reg.tab)
    colnames(reg.tab) <- c('est', 'se')
    reg.tab$est <- sprintf('%0.2f', reg.tab$est)
    reg.tab$se  <- sprintf('\\footnotesize (%0.2f)', reg.tab$se)
    reg.tab$var <- c('Constant', 'Formula-predicted consumer fee gap')
    r2.row <- data.frame(est = sprintf('%0.2f', reg.tot$r.squared),
                         se = '-', var = '$R^2$')
    n.row <- data.frame(est = sprintf('%d', nrow(compare.tot.distort)),
                        se = '-', var = '$N$')
    reg.tab <- dplyr::bind_rows(reg.tab, r2.row)
    reg.tab <- dplyr::bind_rows(reg.tab, n.row)
    write.dat(reg.tab, outpath.tab.approx)
    ## Save correlation
    cor.formula <- sqrt(reg.tot$r.squared)
    outpath.cor <- sub('.csv', '_correlation.csv', outpath.tab.approx)
    write(sprintf('Correlation = %f\n', cor.formula), outpath.cor)

    Distort.mat <- do.call(dplyr::bind_rows, Distort.mat)
    reg.decomp <- summary(lm(tot_gap ~ market_power + off_bsteal + on_bsteal + spence + displacement + rival + other1,
                             data = Distort.mat,
                          weights = Distort.mat$weight))
    # For each regressor, compute R^2k and R^2-k
    regressors <- c('market_power', 'off_bsteal', 'on_bsteal',
                    'spence', 'displacement', 'rival', 'other1')
    r2.bi <- c()
    r2.part <- c()
    for (reg in regressors){
        fmla.bi <- as.formula(sprintf('tot_gap ~ %s', reg))
        r2.bi[reg] <- summary(lm(fmla.bi, data = Distort.mat,
                                 weights = Distort.mat$weight))$r.squared
        rhs <- Reduce(function(x, y) sprintf('%s + %s', x, y), setdiff(regressors, reg))
        fmla.part <- as.formula(sprintf('tot_gap ~ %s', rhs))
        r2.part[reg] <- summary(lm(fmla.part, data = Distort.mat,
                                   weights = Distort.mat$weight))$r.squared
    }
    tab.explain <- data.frame(var = c('Market power', 'Offline business stealing',
                                      'Online business stealing',
                                      'Spence', 'Displacement', 'Rival profits', 'Other'),
                              r2bi = sprintf('%0.2f', r2.bi),
                              r2part = sprintf('%0.2f', r2.part))
    write.dat(tab.explain, outpath.explain)
}

compute.distortions <- function(BL, soc, eqm.objs.co){
    # Compute pricing distortions
    #
    # Inputs
    #   BL: profit-maximization equilibrium
    #   soc: social welfare maximizing fees
    #   eqm.objs.co: data objects

    NF <- eqm.objs.co[[1]][[1]]$dat.m$nplatforms - 1

    # Initialize outputs
    Distort <- list()
    Diffs   <- list()
    markets <- names(eqm.objs.co)

    for (f in 1:NF){
        Distort[[f]] <- list()
    }

    distort.names <-
        c('market_power', 'off_bsteal', 'on_bsteal',
          'spence', 'displacement', 'rival', 'total')

    for (market in markets){
        # Extract data for the market
        eqm.objs.m <- eqm.objs.co[[market]]

        # Extract market-specific eqiulibria
        BL.m  <- BL[[market]]
        soc.m <- soc[[market]]

        # Obtain list of counties in the market
        counties <- names(eqm.objs.m)

        for (co in counties){

            # Extract data
            eqm.objs.j <- eqm.objs.m[[co]]
            dat.m <- eqm.objs.j$dat.m
            NF <- dat.m$nplatforms - 1
            kappa <- eqm.objs.j$kappa
            opts <- eqm.objs.j$opts
            opts$verbose <- FALSE
            pMC.df <- eqm.objs.j$pMC.df
            G.mat <- dat.m$G.mat

            # Extract equilibria
            eqm.BL  <- BL.m[[co]]
            eqm.soc <- soc.m[[co]]

            # Compute commission revenue
            rho.bar.BL <- compute.average.rho(eqm.BL$FP, dat.m)
            rho.bar.so <- compute.average.rho(eqm.soc$FP, dat.m)

            RP.BL <- rho.bar.BL*eqm.BL$R
            RP.so <- rho.bar.so*eqm.soc$R

            #== Compute the distortions ==#
            # Compute market power
            ## Partial
            mu.p.BL <- compute.market.power(eqm.objs.j, eqm.BL)
            ## Total effects of fee changes
            output.total <- compute.total.power(eqm.objs.j, eqm.BL)
            ## Revenue effects
            dComm.BL <- output.total$Deriv.rev/output.total$Deriv.S
            ## Market power
            mu.t.BL <- output.total$mu
            ## Difference is explained by epsilon
            epsilon.BL <- (mu.p.BL - mu.t.BL)/RP.BL

            # Do the same for the socially optimal allocation
            mu.p.so <- compute.market.power(eqm.objs.j, eqm.soc)
            output.total <- compute.total.power(eqm.objs.j, eqm.soc)
            dComm.so <- output.total$Deriv.rev/output.total$Deriv.S
            mu.t.so <- output.total$mu
            epsilon.so <- (mu.p.so - mu.t.so)/RP.so

            # Compute mc
            pMC.df <- eqm.objs.j$pMC.df
            mc <- colMeans(pMC.df[, grep('^mc', colnames(pMC.df))])

            # Compute epsilon
            b.tilde.BL <- RP.BL[1:NF]*(1 + epsilon.BL)
            b.tilde.so <- RP.so[1:NF]*(1 + epsilon.so)
            idx <- which(is.nan(b.tilde.so))
            if (length(idx) > 0){ # 0/0 problem may arise... fix it here
                b.tilde.so[idx] <-  RP.so[1:NF][idx] + (mu.p.so[idx] - mu.t.so[idx])
            }

            #== Business stealing distortion ==#
            # These are computed at the socially optimal allocation
            eqm <- eqm.soc
            C0 <- eqm$C
            ## Baseline sales by restaurant zip
            S0 <- compute.sales(eqm, dat.m, exclude.off = FALSE,
                                return.S.tot = TRUE)
            S0.agg <- colSums(Reduce('+', S0))
            ## Baseline sales by consumer zip
            S0.by.z <- compute.sales(eqm, dat.m, exclude.off = FALSE,
                                     return.S.tot = FALSE,
                                     return.S.by.z = TRUE)

            # Produce matrices with restaurant prices, costs, and markups
            Rhos <- eqm$FP$Rhos
            price.mats <- produce.price.matrices(Rhos, G.mat)
            kappa.mats <- produce.kappa.matrices(eqm.objs.j$opts$resto.param$mc, G.mat)
            R.mat <- matrix(0, nrow = nrow(G.mat), ncol = ncol(G.mat))
            for (f in 1:NF){
                R.mat[, f + 1] <- eqm$R[f]
            }
            net.markups <- lapply(names(price.mats),
                                  function(z) price.mats[[z]]*(1 - R.mat) - kappa.mats[[z]])
            names(net.markups) <- names(price.mats)

            # Do the same for platforms
            consumer.zips <- rownames(pMC.df)
            rho.bar <- weight.rhos.v2(eqm$FP$Sales.tots, eqm$FP$Rhos, dat.m)
            fee.mat <- pracma::repmat(matrix(eqm$C, nrow = 1),
                                      n = length(consumer.zips),
                                      m = 1)
            r.mat <- pracma::repmat(matrix(eqm$R, nrow = 1),
                                    n = length(consumer.zips),
                                    m = 1)
            rownames(fee.mat) <- rownames(r.mat) <- consumer.zips
            z.int <- intersect(consumer.zips, rownames(rho.bar))
            pMC.mat <- pMC.df[, 1:NF]
            pMC.mat <- as.matrix(pMC.mat)
            p.markup <- fee.mat[z.int, ] + r.mat[z.int, ]*rho.bar[z.int, 2:5] - pMC.mat[z.int, ]

            # Initialize outputs
            Off.bsteal    <- c()
            On.bsteal     <- c()
            Rival.effects <- c()

            # Numerical differentiation step size
            h <- 1e-2

            # Loop over platforms
            for (f in 1:NF){
                # Extract and augment fee
                C1 <- C0
                C1[f] <- C1[f] + h
                eqm$C <- C1

                # Compute sales under perturbed fee
                S1 <- compute.sales(eqm, dat.m, exclude.off = FALSE,
                                    return.S.tot = TRUE)
                S1.f <- colSums(Reduce('+', S1))[f + 1]
                deriv <- lapply(names(S0), function(z) (S1[[z]] - S0[[z]])/h)
                names(deriv) <- names(S0)

                # Take out platform f
                deriv.rival <- list()
                for (z in names(S0)){
                    deriv.z <- deriv[[z]]
                    deriv.z[, f + 1] <- 0
                    deriv.rival[[z]] <- deriv.z
                }

                # Multiply derivatives with markups
                off.steal <- sapply(names(S0), function(z) sum(deriv.rival[[z]][, 1]*net.markups[[z]][, 1]))
                all.steal <- sapply(names(S0), function(z) sum(deriv.rival[[z]]*net.markups[[z]]))
                on.steal  <- all.steal - off.steal
                deriv.own <- (S1.f - S0.agg[f + 1])/h

                Off.bsteal[f] <- -sum(off.steal)/deriv.own
                On.bsteal[f]  <- -sum(on.steal)/deriv.own

                # Rival platform profit effects
                S1 <- compute.sales(eqm, dat.m, exclude.off = FALSE,
                                    return.S.tot = FALSE,
                                    return.S.by.z = TRUE)
                deriv <- list()
                for (z in names(S1)){
                    S1.z <- S1[[z]]
                    deriv[[z]] <- (S1.z - S0.by.z[[z]])/h
                    deriv[[z]] <- deriv[[z]][2:5]
                    # Remove own derivative
                    deriv[[z]][f] <- 0
                }
                rival.fx <- sapply(rownames(p.markup), function(z) sum(deriv[[z]]*p.markup[z, ]))
                Rival.effects[f]   <- -sum(rival.fx)/deriv.own
            }

            rho.bar.so <- compute.rho.bar(eqm.soc$FP, dat.m)
            kbar.so <- compute.kappa.bar(eqm.objs.j, eqm.soc)
            b.bar.so <- rho.bar.so[1:NF] - kbar.so

            # Compute distortions
            spence <- b.bar.so - b.tilde.so

            disp <- b.tilde.so - b.tilde.BL

            disp.alt <- dComm.so - dComm.BL

            for (f in 1:NF){
                distortions <- cbind(mu.p.BL[f],
                                     -Off.bsteal[f],
                                     -On.bsteal[f],
                                     spence[f],
                                     disp[f],
                                     -Rival.effects[f])
                tot <- eqm.BL$C[f] - eqm.soc$C[f]
                distortions <- cbind(distortions, tot)
                colnames(distortions) <- distort.names
                Distort[[f]][[co]] <- distortions
            }
            Diffs[[co]] <- eqm.BL$C - eqm.soc$C
        }
    }

    # Weights
    wgt <- c()
    for (market in markets){
        eqm.objs.m <- eqm.objs.co[[market]]
        counties <- intersect(names(eqm.objs.m), names(Distort[[1]]))
        for (co in counties){
            wgt[co] <- sum(eqm.objs.m[[co]]$dat.m$buy.m$count)
        }
    }

    # Matrix of consumer fee differences
    Diff.mat <- do.call(rbind, Diffs)

    # Matrices of distortions
    Distort.mat   <- list()
    for (f in 1:NF){
        Distort.mat.f <- do.call(rbind, Distort[[f]])
        colnames(Distort.mat.f) <- distort.names
        Distort.mat[[f]] <- Distort.mat.f
    }

    outputs <- list(Distort         = Distort,
                    Diffs           = Diffs,
                    Distort.mat     = Distort.mat,
                    wgt             = wgt)
    return(outputs)
}

generate.table <- function(Distortions.agg, Wgt.f){
    # Produce tables showing mean distortions
    Distortions.tab <- do.call(cbind, Distortions.agg)
    Distortions.tab <- as.data.frame(Distortions.tab)
    for (k in 1:ncol(Distortions.tab)){
        Distortions.tab[, k] <- sprintf('%0.2f', Distortions.tab[, k])
    }
    Distortions.tab <- Distortions.tab[c('market_power', 'off_bsteal', 'on_bsteal',
                                         'spence', 'displacement', 'rival', 'other1', 'total'), ]
    Distortions.tab$distortion <- c('Market power', 'Offline business stealing',
                                    'Online business stealing',
                                    'Spence', 'Displacement', 'Rival profits', 'Other', 'Total')
    Distortions.tab$lineflag <- c(0, 0, 0, 0, 0, 0, 1, 0)

    return(Distortions.tab)
}

produce.price.matrices <- function(Rhos, G.mat){
    # Produce matrices containing all restaurant prices in a market
    Z <- names(Rhos)
    price.mats <- list()
    for (z in Z){
        pmat <- matrix(0, nrow = nrow(G.mat), ncol = ncol(G.mat))
        for (ng in 1:nrow(pmat)){
            idx <- which(G.mat[ng, ] == 1)
            pmat[ng, idx] <- Rhos[[z]][[ng]]
        }
        price.mats[[z]] <- pmat
    }
    return(price.mats)
}

produce.kappa.matrices <- function(kappa, G.mat){
    Z <- names(kappa$on)
    kappa.mats <- list()
    for (z in Z){
        kmat <- matrix(0, nrow = nrow(G.mat), ncol = ncol(G.mat))
        for (ng in 1:nrow(G.mat)){
            idx <- which(G.mat[ng, ] == 1)
            kmat[ng, idx] <- kappa$on[z]
        }
        # Insert offline costs
        kmat[, 1] <- kappa$off[z]
        kappa.mats[[z]] <- kmat
    }
    return(kappa.mats)
}

produce.pMC.matrices <- function(pMC.df, G.mat){
    # Produce matrices containing all restaurant prices in a market
    Z <- rownames(pMC.df)
    price.mats <- list()
    for (z in Z){
        pmat <- matrix(0, nrow = nrow(G.mat), ncol = ncol(G.mat))
        for (ng in 1:nrow(pmat)){
            idx <- which(G.mat[ng, ] == 1)
            pmat[ng, idx] <- Rhos[[z]][[ng]]
        }
        price.mats[[z]] <- pmat
    }
    return(price.mats)
}

main()



