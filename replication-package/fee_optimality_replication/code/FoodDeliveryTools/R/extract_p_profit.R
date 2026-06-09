extract.p.profit <- function(eqm, eqm.objs.j){
    # Compute platform profits given the equilibrium in eqm and the data objects
    # in eqm.objs.j
    dat.m  <- eqm.objs.j$dat.m
    opts   <- eqm.objs.j$opts
    kappa  <- eqm.objs.j$kappa
    pMC.df <- eqm.objs.j$pMC.df
    num.param <- load.num.param()

    profit.objs <- platform.profits(eqm$FP, eqm$C, eqm$R, kappa, dat.m,
                                    pMC.df, opts, num.param, more.outputs = TRUE)
    return(profit.objs)
}
