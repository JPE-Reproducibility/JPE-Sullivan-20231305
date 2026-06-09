update.demand.param <- function(param.tab, dat){
    # Update `dat` with the demand model parameters in `param.tab`
    inpath.cbsa.id  <- 'data/est_dat/cbsa_ids.csv'
    cbsa.id         <- EconTools::read.dat(inpath.cbsa.id, sep = '|')

    demand.param <- load.param(param.tab, cbsa.id)
    for (market in names(dat)){
        dat[[market]]$demand.param     <- demand.param
        dat[[market]]$demand.param$psi <- demand.param$Psi[[market]]
        if (length(demand.param$MuEta) > 1){
            dat[[market]]$demand.param$mu.eta <- demand.param$MuEta[market]
        }
    }
    return(dat)
}

