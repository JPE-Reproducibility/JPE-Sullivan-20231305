load.param <- function(est, cbsa.id = NULL){
    # Produce a list of consumer preference parameters

    theta <- est$theta_vec
    names(theta) <- est$theta_names
    demand.param <- list()
    demand.param$alpha <- theta['alpha']
    het.extra <- ('alpha_young') %in% names(theta)
    if (het.extra){
        demand.param$alpha.young   <- theta['alpha_young']
        demand.param$alpha.married <- theta['alpha_married']
        demand.param$alpha.highinc <- theta['alpha_highinc']
    } else {
        demand.param$alpha.young   <- 0
        demand.param$alpha.married <- 0
        demand.param$alpha.highinc <- 0
    }
    demand.param$gamma <- 1

    # Fixed effects
    nplatforms <- 4
    ncbsa <- length(grep('^psi', names(theta)))/nplatforms
    demand.param$Psi <- list()
    for (m in 1:ncbsa){
        psi.idx <- grep(sprintf('psi_%d-', m), names(theta))
        demand.param$Psi[[m]] <- theta[psi.idx]
    }
    if (!is.null(cbsa.id)){
        names(demand.param$Psi) <- cbsa.id$CBSA_name
    }
    # Demographic effects
    lambda.inc <- any(grepl('lambda_3', names(theta)))
    ndemo <- ifelse(lambda.inc, 3, 2)
    demand.param$lambda <- matrix(theta[grep('lambda', names(theta))],
                                  nrow = ndemo, byrow = FALSE)

    # Distribution of zeta
    demand.param$sigma.z1 <- sqrt(exp(theta['logvar_zeta1']))
    demand.param$sigma.z2 <- sqrt(exp(theta['logvar_zeta2']))

    # Distribution of phi_{i, tau}
    demand.param$phi.c <- theta['phi_chain']
    if ('logvar_phi' %in% names(theta)){
        demand.param$sigma.phi <- sqrt(exp(theta['logvar_phi']))
    } else {
        demand.param$sigma.phi <- 0
    }

    mu.eta.idx <- grep('^mu_eta', names(theta))
    no.purchase <- length(mu.eta.idx) > 0
    idio.eta    <- ('logvar_idio' %in% names(theta))

    if (no.purchase){
        # Baseline parameters
        demand.param$MuEta <- theta[mu.eta.idx]
        eta.by.m <- (length(mu.eta.idx) > 1)
        if (eta.by.m){
            names(demand.param$MuEta) <- cbsa.id$CBSA_name
        } else {
            demand.param$mu.eta <- demand.param$MuEta
        }

        demand.param$sigma.eta <- sqrt(exp(theta['logvar_eta']))
        # Demographic effects
        demo.np <- ('np_young' %in% names(theta))
        if (demo.np){
            demand.param$np.young   <- theta['np_young']
            demand.param$np.married <- theta['np_married']
            if (lambda.inc){
                demand.param$np.highinc <- theta['np_highinc']
            }
        }
        if (idio.eta){
            demand.param$sigma.idio <- sqrt(exp(theta['logvar_idio']))
        }
    }

    use.WT <- ('tau' %in% names(theta))
    if (use.WT){
        demand.param$tau <- theta['tau']
    }

    return(demand.param)
}
