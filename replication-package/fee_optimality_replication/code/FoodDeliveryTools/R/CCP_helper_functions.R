produce.fitted.probs <- function(est, est.df){
    # Produce fitted probabilities from a regression `est` on
    # a dataset `est.df`
    fitted.probs <- predict(est, type = 'prob')
    fitted.probs <- as.data.frame(fitted.probs)
    fitted.probs$zip <- est.df$zip
    fitted.probs <- fitted.probs[!duplicated(fitted.probs$zip), ]
    return(fitted.probs)
}


compute.empirical.probs <- function(resto.m, G.vars){
    # Compute empirical probabilities
    resto.m.sub <- resto.m[which(resto.m$J_total > 0), ]
    empirical.probs <- resto.m.sub[, c('zip', G.vars)]
    for (g in G.vars){
        empirical.probs[, g] <- empirical.probs[, g]/resto.m.sub$J_total
    }
    return(empirical.probs)
}

compute.classification.error <- function(resto.m, fitted.probs, G.vars){
    # Compute empirical probabilities
    resto.m.sub <- resto.m[which(resto.m$J_total > 0), ]
    empirical.probs <- resto.m.sub[, c('zip', G.vars)]
    for (g in G.vars){
        empirical.probs[, g] <- empirical.probs[, g]/resto.m.sub$J_total
    }
    # Subset ZCTAs
    zips.incl <- intersect(fitted.probs$zip, empirical.probs$zip)
    resto.m.sub     <- resto.m.sub[which(resto.m.sub$zip %in% zips.incl), ]
    fitted.probs    <- fitted.probs[which(fitted.probs$zip %in% zips.incl), ]
    empirical.probs <- empirical.probs[which(empirical.probs$zip %in% zips.incl), ]
    # Sorting
    resto.m.sub     <- resto.m.sub[order(resto.m.sub$zip), ]
    fitted.probs    <- fitted.probs[order(fitted.probs$zip), ]
    empirical.probs <- empirical.probs[order(empirical.probs$zip), ]
    # Distance
    probs.dist <- abs(fitted.probs[, G.vars] - empirical.probs[, G.vars])
    err.rate <- sum(probs.dist*resto.m.sub[, G.vars])/sum(resto.m$J_total)

    return(err.rate)
}

plot.ccp.fit <- function(empirical.probs, fitted.probs, resto.m, G.vars, outpath.plot, J.min = 200){
    # Plot the fit of the CCPs to the empirical probabilities among
    # ZCTAs with at least `J.min` restaurants

    # Subset ZCTAs
    zips.incl <- intersect(fitted.probs$zip, empirical.probs$zip)
    zips.incl <- intersect(zips.incl, resto.m$zip[which(resto.m$J_total >= J.min)])
    fitted.probs    <- fitted.probs[which(fitted.probs$zip %in% zips.incl), ]
    empirical.probs <- empirical.probs[which(empirical.probs$zip %in% zips.incl), ]
    # Sorting
    fitted.probs    <- fitted.probs[order(fitted.probs$zip), ]
    empirical.probs <- empirical.probs[order(empirical.probs$zip), ]

    fitted.long    <- tidyr::pivot_longer(fitted.probs,    all_of(G.vars),
                                          names_to = 'portfolio', values_to = 'prob_fitted')
    empirical.long <- tidyr::pivot_longer(empirical.probs, all_of(G.vars),
                                          names_to = 'portfolio', values_to = 'prob_empirical')
    probs.long <- dplyr::left_join(fitted.long, empirical.long, by = c('zip', 'portfolio'))

    pdf(outpath.plot)
    plot(x = probs.long$prob_empirical, y = probs.long$prob_fitted, pch = 4,
         ylab = 'Fitted probabilities', xlab = 'Empirical frequencies')
    grid()
    abline(a = 0, b = 1, lty = 2)
    reg <- lm(prob_fitted ~ prob_empirical, data = probs.long)
    abline(reg)
    legend(x = 'bottomright', legend = c('45 degree', 'Regression'), lty = c(2, 1))
    dev.off()
}
