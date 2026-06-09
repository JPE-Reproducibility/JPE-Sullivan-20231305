collect.inpaths <- function(nsim.suffix = '_nsim100'){

    rc.suffix <- '_RC'

    inpaths <- list()
    inpaths$inpath.dat <- sprintf('data/eqm_data/eqm_data%s.rds', nsim.suffix)
    inpaths$inpath.geo <- 'data/geo/geo_with_zctas.csv'
    inpaths$inpath.demo <- 'data/eqm_data/nearby_demo_data.rds'

    ## Paths to restaurants' marginal costs
    indir.rMC <- 'output/recover_restaurant_costs'
    inpaths$inpath.rMC <- sprintf('%s/retaurant_costs.rds', indir.rMC)
    inpaths$inpath.NPP <- sprintf('%s/retaurant_costs_NPP.rds', indir.rMC)

    ## Paths to platforms' marginal costs
    pMC.dir <- 'output/recover_platform_costs'
    inpaths$indir.pMC <- sprintf('%s/platform_costs', pMC.dir)
    inpaths$inpath.pMC.alt.time <- sprintf('%s/platform_costs_alt_timing/spec%s.rds', pMC.dir, nsim.suffix)
    inpaths$inpath.H <- sprintf('%s/hosting_costs/est%s.rds', pMC.dir, nsim.suffix)

    inpaths$inpath.cbsa.codes <- 'data/small_data/cbsa_codenames.csv'

    FC.dir <- 'output/restaurant_FC_estimation'
    inpaths$inpath.ccp    <- sprintf('%s/CCPs/CCP_outputs.rds', FC.dir)
    inpaths$inpath.pi     <- sprintf('%s/EPi_CCPs_continuum%s.rds', FC.dir, nsim.suffix)
    inpaths$inpath.FC.est <- sprintf('%s/restaurant_cost%s_GMM-ccp.rds', FC.dir, rc.suffix)
    inpaths$CF.dir     <- sprintf('output/pricing_eqm/CF%s', nsim.suffix)

    # Sub-market definitions
    inpaths$inpath.regions <- 'output/estimate_platform_costs/pricing_zones.rds'

    return(inpaths)
}
