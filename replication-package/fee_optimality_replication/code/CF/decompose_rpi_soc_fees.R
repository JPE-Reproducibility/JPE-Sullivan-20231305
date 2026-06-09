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
    outpath.rpi <- sprintf('%s/rpi_profit_effects_soc_priv.csv', outdir)

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

    BL.paths  <- grep('^baseline', result.files, value = TRUE)
    soc.paths <- grep('^socopt', result.files, value = TRUE)

    BL.paths  <- sprintf('%s/%s', base.dir, BL.paths)
    soc.paths <- sprintf('%s/%s', base.dir, soc.paths)

    BL  <- lapply(BL.paths, readRDS)
    soc <- lapply(soc.paths, readRDS)

    ## Generate names for CF objects
    BL.codes  <- sub('.*baseline_', '', sub('.rds', '', BL.paths))
    soc.codes <- sub('.*socopt_',   '', sub('.rds', '', soc.paths))

    BL.m  <- sapply(BL.codes,  function(x) cbsa.codes$CBSA_name[cbsa.codes$cbsa == x])
    soc.m <- sapply(soc.codes, function(x) cbsa.codes$CBSA_name[cbsa.codes$cbsa == x])

    names(BL)  <- BL.m
    names(soc) <- soc.m

    # Both equilibrium sets must cover the full market set; a missing or
    # stale market file is a hard error, not silent sample selection
    assert.complete.names(BL.m, markets,
                          what = sprintf('baseline_* equilibria in %s (markets)', base.dir),
                          hint = 'Re-run the CF solvers for this spec.')
    assert.complete.names(soc.m, markets,
                          what = sprintf('socopt_* equilibria in %s (markets)', base.dir),
                          hint = 'Re-run the CF solvers for this spec.')

    # Decompose restaurant profit effects
    rpi.effect <- decompose.r.profits(eqm.objs.co, FC.est, BL, soc)

    # Table
    tab <- data.frame(var = c('Direct effect of fee changes', 'With adoption responses',
                              'With price responses', 'Total effect (all responses)'),
                      val = rpi.effect[c('RC', 'J', 'P', 'all')])
    tab$val <- sprintf('%0.2f', tab$val)
    write.dat(tab, outpath.rpi)
}

decompose.r.profits <- function(eqm.objs.co, FC.est, BL, soc){
    # Compute welfare values

    # Initialize outputs
    BL.rp  <- c()
    soc.rp <- c()
    OC.rp  <- c()
    OR.rp  <- c()
    CR.rp  <- c()
    J.rp   <- c()
    P.rp   <- c()
    S0     <- c()

    markets <- names(eqm.objs.co)

    for (market in markets){
        eqm.objs.m <- eqm.objs.co[[market]]
        counties <- names(eqm.objs.m)

        for (co in counties){
            eqm.objs.j <- eqm.objs.m[[co]]

            eqm.BL  <- BL[[market]][[co]]
            eqm.soc <- soc[[market]][[co]]

            dat.m <- eqm.objs.j$dat.m
            kappa <- eqm.objs.j$kappa
            opts  <- eqm.objs.j$opts

            # Restaurant profits
            BL.rp[co]  <- compute.rpi(dat.m, eqm.BL$C,  eqm.BL$R,  eqm.BL$FP$J.G.1,  kappa,
                                      eqm.BL$FP$Rhos, opts)['total']
            soc.rp[co] <- compute.rpi(dat.m, eqm.soc$C, eqm.soc$R, eqm.soc$FP$J.G.1, kappa,
                                      eqm.soc$FP$Rhos, opts)['total']
            ## Only fee
            OC.rp[co] <- compute.rpi(dat.m, eqm.soc$C, eqm.BL$R,  eqm.BL$FP$J.G.1, kappa,
                                     eqm.BL$FP$Rhos, opts)['total']
            ## Only commission
            OR.rp[co] <- compute.rpi(dat.m, eqm.BL$C, eqm.soc$R,  eqm.BL$FP$J.G.1, kappa,
                                     eqm.BL$FP$Rhos, opts)['total']
            ## Both platform price changes
            CR.rp[co] <-  compute.rpi(dat.m, eqm.soc$C, eqm.soc$R,  eqm.BL$FP$J.G.1, kappa,
                                      eqm.BL$FP$Rhos, opts)['total']
            ## ... and J
            J.rp[co] <-  compute.rpi(dat.m, eqm.soc$C, eqm.soc$R, eqm.soc$FP$J.G.1, kappa,
                                     eqm.BL$FP$Rhos, opts)['total']
            ## ... fees and prices
            P.rp[co] <- compute.rpi(dat.m, eqm.soc$C, eqm.soc$R, eqm.BL$FP$J.G.1, kappa,
                                    eqm.soc$FP$Rhos, opts)['total']

            # Baseline sales on platforms
            sales <- eqm.BL$FP$Sales.platform
            S0[co] <- sum(sales) - sales[1]
        }
    }

    chg.R   <- sum(OR.rp  - BL.rp)
    chg.C   <- sum(OC.rp  - BL.rp)
    chg.RC  <- sum(CR.rp  - BL.rp)
    chg.J   <- sum(J.rp   - BL.rp)
    chg.P   <- sum(P.rp   - BL.rp)
    chg.all <- sum(soc.rp - BL.rp)

    output <- c(R = chg.R, C = chg.C, RC = chg.RC, J = chg.J,
                P = chg.P, all = chg.all)
    output <- output/sum(S0)

    return(output)
}

main()
