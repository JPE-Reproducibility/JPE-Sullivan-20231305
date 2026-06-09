# Save estimates of restaurants' marginal costs
#
# NPP = "non-parity penalties"
library(Matrix)
library(EconTools)
library(FoodDeliveryTools)


main <- function(){
    
    # Create output directory
    outdir <- 'output/recover_restaurant_costs'
    create.dir(outdir)
    
    nsim.suffix <- '_nsim50'
    inpaths <- collect.inpaths(nsim.suffix)
    inpath.dat <- inpaths$inpath.dat
    dat <- readRDS(inpath.dat)
    
    outpath <- sprintf('%s/retaurant_costs_NPP.rds', outdir)

    # Specify paths to inputs
    inpath.menu <- 'output/numerator_menu_pricing/disagg_results/price_indices.csv'
    inpath.cbsa.code <- 'data/small_data/cbsa_codenames.csv'

    opts <- load.opts()

    rhos <- read.csv(inpath.menu)

    # Estimation
    phi.est <- phi.bisection.NPP(dat, rhos, opts, tol = 1e-4, interval = c(0.25, 0.40))

    # Save results
    saveRDS(phi.est, outpath)
}

main()
