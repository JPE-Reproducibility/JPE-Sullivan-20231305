# Save estimates of restaurants' marginal costs with incomplete accounting
# for commissions
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

    outpath <- sprintf('%s/retaurant_costs.rds', outdir)

    # Specify paths to inputs
    inpath.menu <- 'output/numerator_menu_pricing/disagg_results/price_indices.csv'
    inpath.cbsa.code <- 'data/small_data/cbsa_codenames.csv'

    opts <- load.opts()

    rhos <- read.csv(inpath.menu)

    # Estimation
    phi.est <- phi.bisection(dat, rhos, opts)

    # Save results
    saveRDS(phi.est, outpath)
}

main()
