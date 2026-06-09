compute.Eta <- function(demand.param, buy.z){
    # Compute tastes for restaurant ordering Eta
    mu.eta     <- demand.param$mu.eta
    sigma.eta  <- demand.param$sigma.eta

    demo.np    <- ('np.young' %in% names(demand.param))
    lambda.inc <- ('np.highinc' %in% names(demand.param))

    if (demo.np){
        np.young   <- demand.param$np.young
        np.married <- demand.param$np.married
        mean.eta <- mu.eta + np.young*buy.z$young + np.married*buy.z$married
        if (lambda.inc){
            mean.eta <- mean.eta + demand.param$np.highinc*buy.z$high_income
        }
    } else {
        mean.eta <- mu.eta
    }
    Eta <- buy.z$eta*sigma.eta + mean.eta

    idio.eta <- ('sigma.idio' %in% names(demand.param))
    if (idio.eta){
        Eta <- Eta + buy.z$eta.idio*demand.param$sigma.idio
    }

    return(Eta)
}
