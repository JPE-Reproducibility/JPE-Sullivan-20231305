# Restaurant fixed cost estimation via CCP approach
# _v2: use restaurant heterogeneity
library(dplyr)
library(pracma)
library(AER)

library(EconTools)
library(FoodDeliveryTools)


main <- function(){

    # Specify paths
    nsim.suffix <- '_nsim50'
    inpaths <- collect.inpaths(nsim.suffix)

    ## to outputs
    FC.dir <- 'output/restaurant_FC_estimation'
    outpath <- sprintf('%s/restaurant_cost_RC_GMM-ccp.rds', FC.dir)
    outpath.plot <- sprintf('%s/sigma_est_RC_plot.pdf', FC.dir)

    # Load data
    dat   <- readRDS(inpaths$inpath.dat)
    EPi   <- readRDS(inpaths$inpath.pi)

    # Estimate fixed costs
    minimal     <- FALSE
    sigma.start <- c(0.006, 0.003)
    estimate.restaurant.FCs.v2(dat, EPi, outpath,
                               minimal = minimal, outpath.plot = outpath.plot,
                               sigma.start = sigma.start)
}


main()


