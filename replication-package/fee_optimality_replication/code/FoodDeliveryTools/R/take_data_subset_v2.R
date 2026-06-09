take.data.subset.v2 <- function(dat.m, keep.zips){
    # Subset dat.m to the ZIPs specified by `zips`

    ## Ensure that all ZIPs are present in dat.m
    keep.zips <- intersect(keep.zips, names(dat.m$buy.zip))
    ### Version of ZIPs with chain/independent labelling
    keep.zips1 <- c(paste0(keep.zips, 'c'),
                    paste0(keep.zips, 'i'))
    keep.zips1 <- intersect(keep.zips1, names(dat.m$J.G.1.m))

    keep.zips  <- sort(keep.zips)
    keep.zips1 <- sort(keep.zips1)

    ## Extract fields that require editing
    buy.m   <- dat.m$buy.m
    buy.zip <- dat.m$buy.zip

    zip.mat.m   <- dat.m$zip.mat.m
    zip.mat.1.m <- dat.m$zip.mat.1.m
    zip.map     <- dat.m$zip.map
    zip.map.1   <- dat.m$zip.map.1

    fees <- dat.m$fees

    resto.m   <- dat.m$resto.m
    resto.c.m <- dat.m$resto.c.m
    resto.i.m <- dat.m$resto.i.m
    J.G.m     <- dat.m$J.G.m
    J.G.1.m   <- dat.m$J.G.1.m

    caps.df   <- dat.m$caps.df
    caps.df.1 <- dat.m$caps.df.1
    caps.m    <- dat.m$caps.m
    comm      <- dat.m$comm

    ## Edit the fields
    buy.m <- buy.m[which(buy.m$zip %in% keep.zips), ]

    idx.row   <- which(rownames(zip.mat.m) %in% keep.zips)
    idx.col   <- which(colnames(zip.mat.m) %in% keep.zips)
    zip.mat.m <- zip.mat.m[idx.row, idx.col]
    if (length(keep.zips) == 1){
        zip.mat.m <- matrix(as.numeric(keep.zips), nrow = 1, ncol = 1)
        rownames(zip.mat.m) <- keep.zips
        colnames(zip.mat.m) <- keep.zips
    }

    idx.row <- which(rownames(zip.mat.1.m) %in% keep.zips1)
    idx.col <- which(colnames(zip.mat.1.m) %in% keep.zips1)
    zip.mat.1.m <- zip.mat.1.m[idx.row, idx.col]
    if (length(keep.zips1) == 1){
        zip.mat.m <- matrix(as.numeric(keep.zips1), nrow = 1, ncol = 1)
        rownames(zip.mat.1.m) <- keep.zips1
        colnames(zip.mat.1.m) <- keep.zips1
    }

    fees       <- subset.list(fees, keep.zips)
    comm       <- subset.list(comm, keep.zips1)
    resto.m    <- resto.m[which(resto.m$zip   %in% keep.zips), ]
    resto.c.m  <- resto.c.m[which(resto.c.m$zip %in% keep.zips), ]
    resto.i.m  <- resto.i.m[which(resto.i.m$zip %in% keep.zips), ]

    J.G.m     <- subset.list(J.G.m,   keep.zips)
    J.G.1.m   <- subset.list(J.G.1.m, keep.zips1)

    zip.map <- subset.list(zip.map, keep.zips)
    zip.map <- lapply(keep.zips, function(z) intersect(zip.map[[z]], keep.zips))
    names(zip.map) <- keep.zips

    zip.map.1   <- subset.list(zip.map.1, keep.zips1)
    zip.map.1   <- lapply(keep.zips1, function(z) intersect(zip.map.1[[z]], keep.zips1))
    names(zip.map.1) <- keep.zips1

    caps.df   <- caps.df[which(caps.df$zip %in% keep.zips), ]
    caps.df.1 <- caps.df.1[which(caps.df.1$zip %in% keep.zips1), ]

    if (!is.null(caps.m)){
        caps.m <- subset.list(caps.m, keep.zips1)
    }
    buy.zip <- subset.list(buy.zip, keep.zips)

    ## Update the fields of dat.m
    dat.m.sub <- dat.m
    dat.m.sub$buy.m   <- buy.m
    dat.m.sub$buy.zip <- sort.list(buy.zip)

    dat.m.sub$zip.map     <- sort.list(zip.map)
    dat.m.sub$zip.map.1   <- sort.list(zip.map.1)
    dat.m.sub$zip.mat.m   <- zip.mat.m
    dat.m.sub$zip.mat.1.m <- zip.mat.1.m

    dat.m.sub$fees <- sort.list(fees)
    dat.m.sub$comm <- sort.list(comm)

    dat.m.sub$resto.m   <- resto.m
    dat.m.sub$resto.c.m <- resto.c.m
    dat.m.sub$resto.i.m <- resto.i.m

    dat.m.sub$J.G.m   <- sort.list(J.G.m)
    dat.m.sub$J.G.1.m <- sort.list(J.G.1.m)

    dat.m.sub$caps.df   <- caps.df
    dat.m.sub$caps.df.1 <- caps.df.1
    if (!is.null(caps.m)){
        dat.m.sub$caps.m <- sort.list(caps.m)
    }

    dat.m.sub$Jp.G.m   <- map.J.to.Jp(dat.m.sub$J.G.m,   dat.m.sub$zip.mat.m)
    dat.m.sub$Jp.G.1.m <- map.J.to.Jp(dat.m.sub$J.G.1.m, dat.m.sub$zip.mat.1.m)

    return(dat.m.sub)
}
