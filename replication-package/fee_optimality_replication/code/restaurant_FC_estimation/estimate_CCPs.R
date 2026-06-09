# Estimate CCPs for use in the estimation of restaurants' costs
library(FoodDeliveryTools)

main <- function(){
    # Specify paths
    nsim.suffix <- '_nsim50'
    inpath.dat <- sprintf('data/eqm_data/eqm_data%s.rds', nsim.suffix)
    outdir <- 'output/restaurant_FC_estimation/CCPs'
    EconTools::create.dir(outdir)
    outpath <- sprintf('%s/CCP_outputs.rds', outdir)

    # Load the data
    dat <- readRDS(inpath.dat)

    minimal <- FALSE

    CCP.estimation(dat, outpath, minimal = minimal, outdir = outdir)
 }

main()
