
partnered.only <- TRUE
if (partnered.only){
    outpath <- 'data/yipitdata/resto_panel_v2.rds'
    outpath.indep <- 'data/yipitdata/resto_panel_indep_v2.rds'
    outpath.chain <- 'data/yipitdata/resto_panel_chain_v2.rds'
} else {
    outpath <- 'data/yipitdata/resto_panel_nonp_v2.rds'
    outpath.indep <- 'data/yipitdata/resto_panel_indep_nonp_v2.rds'
    outpath.chain <- 'data/yipitdata/resto_panel_chain_nonp_v2.rds'
}

DAT <- list()
DAT.indep <- list()
DAT.chain <- list()


times <- FoodDeliveryTools::platform.adoption.times()

years <- times$years
months <- times$months

keep.platforms <- c('offline', 'Postmates', 'Grubhub', 'DoorDash', 'UberEats')
online.platforms <- setdiff(keep.platforms, 'offline')

nlocs <- list()
for (year in years){
    print(year)
    nlocs[[year]] <- c()
    for (month in months[[year]]){
        print(month)
        inpath.loc <- sprintf('data/yipitdata/location_level_w_infogroup%s_%s%s.rds',
                               year, month, year)
        inpath.plat <- sprintf('data/yipitdata/plat_loc_level_w_infogroup%s_%s%s.rds',
                               year, month, year)

        locs <- readRDS(inpath.loc)
        plat <- readRDS(inpath.plat)
        plat$is_partnered_merchant[which(is.na(plat$is_partnered_merchant))] <- TRUE

        if (partnered.only){
            # Don't include non-partnered restaurants
            plat <- plat[plat$is_partnered_merchant, ]
        }

        nlocs[[year]][month] <- nrow(locs)

        plat <- plat[which(plat$platform %in% online.platforms), ]
        for (f in online.platforms){
            plat[, f] <- 1*(plat$platform == f)
        }
        G.df <- doBy::summaryBy(DoorDash + UberEats + Grubhub + Postmates ~ loc.id,
                                data = plat, FUN = sum, keep.names = TRUE)
        locs <- dplyr::left_join(locs, G.df, by = 'loc.id')
        for (f in online.platforms){
            locs[which(is.na(locs[, f])), f] <- 0
        }
        locs$nplatforms <- rowSums(locs[, online.platforms])
        locs$online <- locs$nplatforms >= 1

        #== Overall ==#
        zip.lvl <- doBy::summaryBy(nplatforms + online + DoorDash + UberEats + Grubhub + Postmates  ~ zip,
                                   FUN = mean, data = locs, keep.names = TRUE)
        ## Number of restaurants
        nresto.df <- doBy::summaryBy(loc.id ~ zip, FUN = length, data = locs)
        colnames(nresto.df) <- c('zip', 'nresto')
        zip.lvl <- dplyr::inner_join(zip.lvl, nresto.df, by = 'zip')

        zip.lvl$month <- which(months[[year]] == month)
        zip.lvl$year <- as.numeric(year)
        label <- paste0(month, year)
        DAT[[label]] <- zip.lvl

        #== Independent restaurants ==#
        locs.indep <- locs[which(is.na(locs$brand)), ]
        zip.lvl <- doBy::summaryBy(nplatforms + online + DoorDash + UberEats + Grubhub + Postmates  ~ zip,
                                   FUN = mean, data = locs.indep, keep.names = TRUE)
        ## Number of restaurants
        nresto.df <- doBy::summaryBy(loc.id ~ zip, FUN = length, data = locs.indep)
        colnames(nresto.df) <- c('zip', 'nresto')
        zip.lvl <- dplyr::inner_join(zip.lvl, nresto.df, by = 'zip')
        zip.lvl$month <- which(months[[year]] == month)
        zip.lvl$year <- as.numeric(year)
        label <- paste0(month, year)
        DAT.indep[[label]] <- zip.lvl

        #== Chain restaurants ==#
        locs.chain <- locs[which(!is.na(locs$brand)), ]
        zip.lvl <- doBy::summaryBy(nplatforms + online + DoorDash + UberEats + Grubhub + Postmates  ~ zip,
                                   FUN = mean, data = locs.chain, keep.names = TRUE)
        ## Number of restaurants
        nresto.df <- doBy::summaryBy(loc.id ~ zip, FUN = length, data = locs.chain)
        colnames(nresto.df) <- c('zip', 'nresto')
        zip.lvl <- dplyr::inner_join(zip.lvl, nresto.df, by = 'zip')
        zip.lvl$month <- which(months[[year]] == month)
        zip.lvl$year <- as.numeric(year)
        label <- paste0(month, year)
        DAT.chain[[label]] <- zip.lvl
    }
}

df <- Reduce(dplyr::bind_rows, DAT)
df$zip <- EconTools::fix.zip.codes(df$zip)
saveRDS(df, outpath)

# Independent
df.indep <- Reduce(dplyr::bind_rows, DAT.indep)
df.indep$zip <- EconTools::fix.zip.codes(df.indep$zip)
saveRDS(df.indep, outpath.indep)

# Chain
df.chain <- Reduce(dplyr::bind_rows, DAT.chain)
df.chain$zip <- EconTools::fix.zip.codes(df.chain$zip)
saveRDS(df.chain, outpath.chain)
