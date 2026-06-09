
process.CCPs <- function(CCPs, fitted.version = 'fitted.probs3'){
    # Place the CCPs based on a multinomial logit in the same
    # format as the empirical frequencies
    markets <- names(CCPs)
    P.ccp <- list()
    G.vars <- grep('^G[0-9]', colnames(CCPs[[1]][[fitted.version]]), value = TRUE)
    for (market in markets){
        CCPs.m <- CCPs[[market]]
        probs.m <- CCPs.m[[fitted.version]]
        P.0 <- list()
        for (zip in probs.m$zip){
            p.z <- as.numeric(probs.m[which(probs.m$zip == zip), G.vars])
            names(p.z) <- G.vars
            P.0[[zip]] <- p.z
        }
        P.ccp[[market]] <- P.0
    }
    return(P.ccp)
}
