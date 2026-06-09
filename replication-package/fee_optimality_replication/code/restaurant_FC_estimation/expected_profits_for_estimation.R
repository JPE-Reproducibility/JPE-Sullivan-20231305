# Use estimated CCPs and empirical frequencies to compute expected profits
# of joining certain platform portfolios

library(Matrix)
library(EconTools)
library(FoodDeliveryTools)

main <- function(){
    nsim.suffix <- '_nsim50' # '_nsim250'
    inpaths <- collect.inpaths(nsim.suffix)

    # Specify paths
    base.dir <- 'output/restaurant_FC_estimation'
    inpath.dat <- sprintf('data/eqm_data/eqm_data%s.rds', nsim.suffix)

    ## Paths to CCP estimates (written by estimate_CCPs.R)
    inpath.ccp   <- sprintf('%s/CCPs/CCP_outputs.rds', base.dir)
    inpath.ccp.c <- sprintf('%s/CCPs/CCP_outputs_chain.rds', base.dir)
    inpath.ccp.i <- sprintf('%s/CCPs/CCP_outputs_indep.rds', base.dir)

    indir.rMC <- 'output/recover_restaurant_costs'
    inpath.rMC <- sprintf('%s/retaurant_costs.rds', indir.rMC)

    outdir <- sprintf('%s/EPi_ccp', base.dir)
    create.dir(outdir)

    outpath <- sprintf('%s/EPi_CCPs_continuum%s.rds',
                       base.dir, nsim.suffix)

    # Load data
    dat     <- readRDS(inpath.dat)
    CCPs    <- readRDS(inpath.ccp)
    CCPs.c  <- readRDS(inpath.ccp.c)
    CCPs.i  <- readRDS(inpath.ccp.i)
    rMC.est <- readRDS(inpath.rMC)

    markets <- names(dat)

    n.multi.sim <- 5
    Jbar <- 1

    compute.CCP.profits(dat, CCPs, CCPs.c, CCPs.i,
                        rMC.est, outpath = outpath, outdir = outdir,
                        n.multi.sim = n.multi.sim, Jbar = Jbar)
}

main()
