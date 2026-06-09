compute.average.rho <- function(FP, dat.m){
    rho.bar <- weight.rhos.v2(FP$Sales.tots, FP$Rhos, dat.m)
    rho.bar <- colMeans(rho.bar)
    rho.bar <- rho.bar[2:length(rho.bar)]
    return(rho.bar)
}
