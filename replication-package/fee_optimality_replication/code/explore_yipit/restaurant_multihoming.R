# Characterize the extent of restaurant multihoming
library(EconTools)

use.nonp <- FALSE

data.dir <- 'data/yipitdata'
ig.year.suffix <- '2021'
inpath.plat <- sprintf('%s/plat_loc_level_w_infogroup%s_apr2021.rds', data.dir, ig.year.suffix)
inpath.loc  <- sprintf('%s/location_level_w_infogroup%s_apr2021.rds', data.dir, ig.year.suffix)

if (ig.year.suffix == '2021'){
    outpath.multihome <- 'output/explore_yipit/multihome_ig2021.csv'
    outpath.no.PM     <- 'output/explore_yipit/multihome_no_PM_ig2021.csv'
} else {
    outpath.multihome <- 'output/explore_yipit/multihome.csv'
    outpath.no.PM     <- 'output/explore_yipit/multihome_no_PM.csv'
}

platforms <- readRDS(inpath.plat)
locs      <- readRDS(inpath.loc)

if (!use.nonp){
    idx <- c(which(platforms$is_partnered_merchant), which(is.na(platforms$is_partnered_merchant)))
    idx <- unique(idx)
    platforms <- platforms[idx, ]
}

PLs <- unique(platforms$platform)
PLs <- c("DD" = 'DoorDash',
         "Uber" = 'UberEats',
         "GH" = 'Grubhub',
         "PM" = 'Postmates')
platforms <- platforms[which(platforms$platform %in% c(PLs, 'offline')), ]

locs <- locs[which(locs$loc.id %in% unique(platforms$loc.id)), ]
N.tot <- nrow(locs)
N <- c()

nplatforms <- length(PLs)
multi.home <- matrix(NA, nrow = nplatforms, ncol = nplatforms)
rownames(multi.home) <- colnames(multi.home) <- PLs

for (f in PLs){
    idx <- which(platforms$platform == f)
    n.f <- length(idx)
    N[f] <- n.f
    locs.f <- platforms$loc.id[idx]
    df.f <- platforms[which(platforms$loc.id %in% locs.f), ]

    for (g in PLs){
        multi.home[f, g] <- sum(df.f$platform == g)/N[f]
    }
}
multi.home <- as.data.frame(multi.home)
for (x in colnames(multi.home)){
    multi.home[[x]] <- sprintf('%0.2f', multi.home[[x]])
}
resto.shares <- N/N.tot
multi.home$shares <- sprintf('%0.2f', resto.shares)
cnames <- colnames(multi.home)
multi.home$platform <- names(PLs)
multi.home <- multi.home[, c('platform', cnames)]

write.dat(multi.home, file = outpath.multihome)

# Version excluding PM
no.PM <- multi.home[which(multi.home$platform != 'PM'), ]
write.dat(no.PM, file = outpath.no.PM)

