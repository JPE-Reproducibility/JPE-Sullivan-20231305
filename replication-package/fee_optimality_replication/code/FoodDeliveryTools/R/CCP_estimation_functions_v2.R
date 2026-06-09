prepare.demo.data.v2 <- function(demo0, dat, EPi, geo, drop.na = TRUE){
    # Add several variables to the `demo` data.frames

    # Preliminaries
    markets <- names(EPi)
    nportfolios <- dat[[1]]$nportfolios
    idx.in <- 2:nportfolios

    # Prepare geographic data
    geo.vars <- c('zip', 'county', 'pop', 'municipality')
    geo.sub <- geo[, geo.vars]
    geo.sub <- EconTools::rename.var(geo.sub, 'zip', 'zip0')

    demo <- list()
    for (market in markets){
        # Extract market data
        dat.m <- dat[[market]]
        demo.m <- demo0[[market]]
        resto.c <- dat.m$resto.c.m
        resto.i <- dat.m$resto.i.m
        EPi.m <- EPi[[market]]
        ## Make sure there are no NULL entries
        not.NULL <- which(!sapply(EPi.m, is.null))
        not.NULL.zips <- names(EPi.m)[not.NULL]
        EPi.m <- lapply(not.NULL.zips, function(z) EPi.m[[z]])
        names(EPi.m) <- not.NULL.zips
        # Determine market zip codes
        zips.m <- names(EPi.m)

        # Form combined data.frame of restaurants
        resto.m <- combine.resto.data(resto.c, resto.i)

        ## Merge in share online
        resto.m$share_online <- 1 - resto.m$G0000/resto.m$J_total

        ## DoorDash shares
        dd.vars <- grep('^G1[0-1]{3}$', colnames(resto.m), value = TRUE)
        n.dd <- rowSums(resto.m[, dd.vars])
        resto.m$share_dd  <- n.dd/resto.m$J_total
        resto.m$share_dd2 <- n.dd/(n.dd + resto.m$J_total)
        resto.m$share_dd_only <- resto.m$G1000/(resto.m$G0000 + resto.m$G1000)

        ## Uber Eats shares
        uber.vars <- grep('^G[0-1]1[0-1]{2}$', colnames(resto.m), value = TRUE)
        n.uber <- rowSums(resto.m[, uber.vars])
        resto.m$share_uber  <- n.uber/resto.m$J_total
        resto.m$share_uber2 <- n.uber/(n.uber + resto.m$J_total)
        resto.m$share_uber_only <- resto.m$G0100/(resto.m$G0000 + resto.m$G0100)

        ## Comparison of DoorDash alone with DoorDash and Uber Eats
        resto.m$share_both <- resto.m$G1100/(resto.m$G1000 + resto.m$G1100)
        ## Comparison of all DoorDash to all DoorDash + Uber Eats
        resto.m$share_uber_dd <- (resto.m$G1100 + resto.m$G1110 + resto.m$G1101 + resto.m$G1111)/n.dd
        ## Comparison of Uber to Uber + DoorDash
        resto.m$share_dd_uber <- (resto.m$G1100 + resto.m$G1110 + resto.m$G1101 + resto.m$G1111)/n.uber
        #== Simplified versions of the above ==#
        ## Comparison of all DoorDash to all DoorDash + Uber Eats
        resto.m$share_uber_dd0 <- (resto.m$G1100)/(resto.m$G1000 + resto.m$G1100)
        # Comparison of Uber to Uber + DoorDash
        resto.m$share_dd_uber0 <- (resto.m$G1100)/(resto.m$G0100 + resto.m$G1100)

        resto.m$share_dd_other <- (n.dd - resto.m$G1000)/n.dd

        # Average number of platforms
        resto.m$avg_n <- compute.average.n.platforms(resto.m)

        ## Psi quality indices
        psi.names <- c()
        for (f in 1:(dat[[1]]$nplatforms - 1)){
            psi.f <- paste0('psi.', f)
            resto.m[, psi.f] <- dat[[1]]$demand.param$Psi[[market]][f]
            psi.names <- c(psi.names, psi.f)
        }

        # Merge demo.m and resto.m
        ## Specify which variables of resto.m to keep
        keep.vars <- c('zip', 'J_total', 'share_online',
                       'share_dd', 'share_dd_only', 'share_dd2',
                       'share_uber', 'share_uber_only', 'share_uber2',
                       'share_both','share_uber_dd', 'share_dd_uber',
                       'share_uber_dd0', 'share_dd_uber0',
                       'share_dd_other', 'avg_n', psi.names)
        ## Produce version of demo.m that accounts for restaurant heterogeneity
        demo.c <- demo.m
        demo.c$zip <- paste0(demo.c$zip, 'c')
        demo.i <- demo.m
        demo.i$zip <- paste0(demo.i$zip, 'i')
        demo.m <- dplyr::bind_rows(demo.c, demo.i)

        demo.m <- dplyr::inner_join(demo.m, resto.m[, keep.vars], by = 'zip')

        # Compute ratio of profits from online to offline
        pi.ratio   <- sapply(zips.m, function(z) max(EPi.m[[z]][idx.in])/EPi.m[[z]][1])
        pi.diff    <- sapply(zips.m, function(z) max(EPi.m[[z]][idx.in]) - EPi.m[[z]][1])
        mean.ratio <- sapply(zips.m, function(z) mean(EPi.m[[z]][idx.in])/EPi.m[[z]][1])
        demo.m <- merge.in.vector(demo.m, pi.ratio,   varname = 'pi_ratio')
        demo.m <- merge.in.vector(demo.m, pi.diff,    varname = 'pi_diff')
        demo.m <- merge.in.vector(demo.m, mean.ratio, varname = 'mean_ratio')

        for (g in 1:nportfolios){
            pi.g <- sapply(zips.m, function(z) EPi.m[[z]][g])
            demo.m <- merge.in.vector(demo.m, pi.g, varname = sprintf('pi%d', g))
        }

        demo.m$log_pi_ratio <- log(demo.m$pi_ratio)

        ## DD ratios
        dd.ratio   <- sapply(zips.m, function(z) EPi.m[[z]][2]/EPi.m[[z]][1])
        dd.diff    <- sapply(zips.m, function(z) EPi.m[[z]][2] - EPi.m[[z]][1])
        demo.m <- merge.in.vector(demo.m, dd.ratio,   varname = 'dd_ratio')
        demo.m <- merge.in.vector(demo.m, dd.diff,    varname = 'dd_diff')
        if (drop.na){
            demo.m <- demo.m[which(demo.m$dd_ratio > 0), ]
        }
        demo.m$log_dd_ratio <- log(demo.m$dd_ratio)

        ## Uber ratios
        uber.ratio   <- sapply(zips.m, function(z) EPi.m[[z]][3]/EPi.m[[z]][1])
        uber.diff    <- sapply(zips.m, function(z) EPi.m[[z]][3] - EPi.m[[z]][1])
        demo.m <- merge.in.vector(demo.m, uber.ratio,   varname = 'uber_ratio')
        demo.m <- merge.in.vector(demo.m, uber.diff,    varname = 'uber_diff')
        if (drop.na){
            demo.m <- demo.m[which(demo.m$uber_ratio > 0), ]
        }
        demo.m$log_uber_ratio <- log(demo.m$uber_ratio)

        # Comparison ratios
        both.ratio <- sapply(zips.m, function(z) EPi.m[[z]][4]/EPi.m[[z]][2])
        demo.m <- merge.in.vector(demo.m, both.ratio, varname = 'both_ratio')
        if (drop.na){
            demo.m <- demo.m[which(demo.m$both_ratio > 0), ]
        }
        demo.m$log_both_ratio <- log(demo.m$both_ratio)

        # Merge in county
        demo.m$zip0 <- sub('[ci]$', '', demo.m$zip)
        demo.m <- dplyr::left_join(demo.m, geo.sub, by = 'zip0')

        demo.m$pop_young_range <- demo.m$pop*demo.m$share_young_range

        demo.m$market <- market

        # Add in fee caps
        caps.m <- dat.m$caps.df.1
        caps.m$fee.cap <- 1*(caps.m$caps < 0.3)
        caps.m <- EconTools::rename.var(caps.m, 'caps', 'commission')
        demo.m <- dplyr::left_join(demo.m, caps.m, by = c('zip'))

        demo[[market]] <- demo.m
    }

    return(demo)
}

compute.average.n.platforms <- function(resto.m){
    resto.m$n1 <- resto.m$G1000 + resto.m$G0100 + resto.m$G0010 + resto.m$G0001
    resto.m$n2 <- resto.m$G1100 + resto.m$G1010 + resto.m$G1001 + resto.m$G0110 +
        resto.m$G0101 + resto.m$G0011
    resto.m$n3 <- resto.m$G1110 + resto.m$G1101 + resto.m$G1011 + resto.m$G0111
    resto.m$n4 <- resto.m$G1111
    avg_n <- resto.m$n1 + 2*resto.m$n2 + 3*resto.m$n3 + 4*resto.m$n4
    avg_n <- avg_n/resto.m$J_total
    return(avg_n)
}


combine.resto.data <- function(resto.c, resto.i){
    # Combine data on different types of restaurants
    # resto.c: chain restaurants
    # resto.i: independent restaurants
    resto.c$zip0 <- resto.c$zip
    resto.c$zip  <- paste0(resto.c$zip, 'c')
    resto.i$zip0 <- resto.i$zip
    resto.i$zip  <- paste0(resto.i$zip, 'i')
    resto.m <- dplyr::bind_rows(resto.c, resto.i)
    return(resto.m)
}

generate.predicted.counts <- function(EPi.m, kappa, nres.m, G.vars, scale.up = TRUE, Mu.f =  NULL){
    # Compute restaurant counts under profits EPi.m and cost parameters kappa
    P.1 <- compute.market.G.shares(EPi.m, kappa, Mu.f)

    if (scale.up){
        J.1 <- lapply(names(P.1), function(z) P.1[[z]]*nres.m[z])
    } else {
        J.1 <- P.1
    }
    J.1.mat <- as.data.frame(Reduce(rbind, J.1))
    colnames(J.1.mat) <- G.vars
    J.1.mat$zip <- names(P.1)
    return(J.1.mat)
}


merge.in.vector <- function(df, v, namevar = 'zip', varname = NULL){
    df.v <- as.data.frame(v)
    df.v[[namevar]] <- names(v)
    df <- dplyr::inner_join(df, df.v, by = namevar)
    if (!is.null(varname)){
        df <- EconTools::rename.var(df, 'v', varname)
    }
    return(df)
}

