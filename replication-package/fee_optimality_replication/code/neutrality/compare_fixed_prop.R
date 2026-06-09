# Compare fixed and proportional consumer fees

library(EconTools)

main <- function(){

    # Output directory
    outdir <- 'output/neutrality'

    # Price sensitivity
    alpha <- 0.6
    # Platform marginal cost
    mc <- 4

    # Organize parameters in a list
    param <- list()
    param$alpha <- alpha
    param$mc    <- mc

    eqm.analysis(outdir, param, het.kappa = TRUE)
    eqm.analysis(outdir, param, het.kappa = FALSE)
}

eqm.analysis <- function(outdir, param, het.kappa = TRUE){
    # Analysis with heterogeneous menu item marginal costs
    if (het.kappa){
        suffix <- '-hetkappa'
    } else {
        suffix <- '-homokappa'
    }
    outpath.sales   <- sprintf('%s/sales_table%s.csv',   outdir, suffix)
    outpath.welfare <- sprintf('%s/welfare_table%s.csv', outdir, suffix)

    mc    <- param$mc
    alpha <- param$alpha

    # Set costs
    kappa <- c(10, 30)
    if (!het.kappa){
        kappa <- rep(mean(kappa), times = length(kappa))
    }
    # Target market shares under social optimum
    mkt.shares <- c(0.15, 0.75)
    p <- kappa
    u <- log(mkt.shares) - log(1 - sum(mkt.shares))
    delta <- u + alpha*(p + mc)
    # Store parameters
    param$kappa <- kappa
    param$delta <- delta

    fixed.soln <- optimize(fixed.obj, param = param, interval = c(mc, 3*mc),
                           maximum = TRUE)
    rate.soln  <- optimize(rate.obj, param = param, interval = c(0, 1),
                           maximum = TRUE)

    C0 <- c(mc, 0.10)
    hybrid.soln <- optim(par = C0, hybrid.obj, param = param,
                         control = list(fnscale = -1))

    C.fixed  <- fixed.soln$maximum
    C.rate   <- rate.soln$maximum
    C.hybrid <- hybrid.soln$par

    p.fixed  <- compute.p.eqm(c(C.fixed, 0), param)
    p.rate   <- compute.p.eqm(c(0, C.rate), param)
    p.hybrid <- compute.p.eqm(C.hybrid, param)

    post.fixed  <- p.fixed + C.fixed
    post.rate   <- p.rate*(1 + C.rate)
    post.hybrid <- p.hybrid*(1 + C.hybrid[2]) + C.hybrid[1]

    sales.fixed  <- S(post.fixed,  param)
    sales.rate   <- S(post.rate,   param)
    sales.hybrid <- S(post.hybrid, param)

    # Social optimum
    p.opt <- param$kappa
    C.opt <- param$mc
    post.opt <- p.opt + C.opt
    sales.opt <- S(post.opt, param)

    # Sales table
    s.tab <- data.frame(regime = c('Fixed', 'Prop.', 'Hybrid', 'Efficient'),
                        s_low  = c(sales.fixed[1], sales.rate[1],
                                   sales.hybrid[1], sales.opt[1]),
                        s_high = c(sales.fixed[2], sales.rate[2],
                                   sales.hybrid[2], sales.opt[2]))
    s.tab$ratio <- s.tab$s_high/s.tab$s_low

    for (v in c('s_low', 's_high', 'ratio')){
        s.tab[, v] <- sprintf('%0.3f', s.tab[, v])
    }
    write.dat(s.tab, outpath.sales)

    # Profits and social welfare
    ## Consumer surplus
    u.fixed  <- delta - alpha*post.fixed
    CS.fixed <- log(1 + sum(exp(u.fixed)))
    u.rate   <- delta - alpha*post.rate
    CS.rate  <- log(1 + sum(exp(u.rate)))
    u.hybrid <- delta - alpha*post.hybrid
    CS.hybrid <- log(1 + sum(exp(u.hybrid)))
    u.opt    <- delta - alpha*post.opt
    CS.opt   <- log(1 + sum(exp(u.opt)))

    ## Profits
    ### Platforms
    PP.fixed  <- fixed.soln$objective
    PP.rate   <- rate.soln$objective
    PP.hybrid <- hybrid.soln$value
    PP.opt    <- 0
    ### Restaurants
    RP.fixed  <- sum((p.fixed  - kappa)*sales.fixed)
    RP.rate   <- sum((p.rate   - kappa)*sales.rate)
    RP.hybrid <- sum((p.hybrid - kappa)*sales.hybrid)
    RP.opt    <- 0

    w.tab <- data.frame(regime = c('Fixed', 'Prop.', 'Hybrid', 'Efficient'),
                        CS     = c(CS.fixed, CS.rate, CS.hybrid, CS.opt),
                        RP     = c(RP.fixed, RP.rate, RP.hybrid, RP.opt),
                        PP     = c(PP.fixed, PP.rate, PP.hybrid, PP.opt))
    w.tab$Tot <- w.tab$CS + w.tab$RP + w.tab$PP
    for (v in setdiff(colnames(w.tab), 'regime')){
        w.tab[, v] <- sprintf('%0.2f', w.tab[, v])
    }
    write.dat(w.tab, outpath.welfare)
}


S <- function(p, param){
    # Sales
    delta <- param$delta
    alpha <- param$alpha
    u <- delta - alpha*p
    sales <- exp(u)/(1 + sum(exp(u)))
    return(sales)
}

fixed.obj <- function(C.fixed, param){
    # Platform objective function with fixed consumer fees
    mc <- param$mc
    p <- compute.p.eqm.fixed(C.fixed, param)
    p.post <- p + C.fixed
    sales <- S(p.post, param)

    lambda <- sales[1]*(C.fixed - mc) + sales[2]*(C.fixed - mc)
    return(lambda)
}

rate.obj <- function(C.rate, param){
    # Platform objective function with proportional consumer fees
    mc <- param$mc
    p <- compute.p.eqm.rate(C.rate, param)
    p.post <- p*(1 + C.rate)
    sales <- S(p.post, param)
    lambda <- sales[1]*(p[1]*C.rate - mc) + sales[2]*(p[2]*C.rate - mc)
    return(lambda)
}

hybrid.obj <- function(x, param){
    C.fixed <- x[1]
    C.rate  <- x[2]

    mc <- param$mc
    C  <- c(C.fixed, C.rate)
    p  <- compute.p.eqm(C, param)

    p.post <- p*(1 + C.rate) + C.fixed
    sales  <- S(p.post, param)

    pi <- sales[1]*(C.fixed + p[1]*C.rate - mc) +
          sales[2]*(C.fixed + p[2]*C.rate - mc)
    return(pi)
}


compute.p.eqm <- function(C, param, R = 0){
    # Compute optimal prices when the platform uses both
    # fixed and proportional fees

    alpha <- param$alpha
    kappa <- param$kappa

    C.fixed <- C[1]
    C.rate  <- C[2]

    max.iter <- 100
    tol <- 1e-4
    learn.rate <- 0.25
    p0 <- kappa/(1 - R) + 1/(alpha*(1 + C.rate))
    for (iter in 1:max.iter){
        p0.post <- p0*(1 + C.rate) + C.fixed
        sales <- S(p0.post, param)
        ## diagonal derivatives
        D.diag <- -alpha*sales*(1 - sales)
        ## off diagonal
        D.off <- alpha*sales[1]*sales[2]
        Delta <- matrix(c(0, D.off,
                          D.off, 0), nrow = 2, ncol = 2)
        diag(Delta) <- D.diag
        p1 <- kappa/(1 - R) - (1/(1 + C.rate))*solve(Delta)%*%matrix(sales, ncol = 1)
        dist <- max(abs(p1 - p0))
        if (dist < tol){
            break
        } else{
            p0 <- learn.rate*p1 + (1 - learn.rate)*p0
        }
    }
    return(p1)
}

compute.p.eqm.fixed <- function(C.fixed, param){
    compute.p.eqm(c(C.fixed, 0), param)
}

compute.p.eqm.rate <- function(C.rate, param){
    compute.p.eqm(c(0, C.rate), param)
}

main()
