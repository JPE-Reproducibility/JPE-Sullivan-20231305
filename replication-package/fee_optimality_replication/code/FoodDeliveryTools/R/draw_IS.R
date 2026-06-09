draw.IS <- function(demand.param, max.iter = 1e7, nXi = 7){
    # Draw unobservables for importance sampling
    for (k in 1:max.iter){
        Xi.trial <- rnorm(nXi)
        score <- phi(Xi.trial, demand.param)
        accept <- rbinom(1, 1, score)
        if (accept == 1){
            break
        }
    }
    if (k == max.iter){
        warning('No Xi.i accepted')
    }
    return(Xi.trial)
}


phi <- function(Xi, demand.param, base.shr = 0.025, sigma.U = 3){
    # Acceptance/rejection function for importance sampling
    # Inputs
    #   base.shr: probability of accepting a vector of zeros
    #   sigma: additional parameter that controls the likelihood of acceptance
    s1 <- demand.param$sigma.z1
    s2 <- demand.param$sigma.z2
    sE <- demand.param$sigma.eta

    b <- sigma.U*(log(base.shr) - log(1 - base.shr))

    U <- b + s1*Xi[1] + s2*max(Xi[2:5]) + sE*Xi[6]
    U <- U/sigma.U
    eU <- exp(U)
    score <- eU/(1 + eU)

    return(score)
}
