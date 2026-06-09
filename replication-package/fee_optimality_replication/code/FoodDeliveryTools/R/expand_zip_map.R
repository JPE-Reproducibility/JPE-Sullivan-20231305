expand.zip.map <- function(zip.map){
    # Expand zip.map, the mapping between ZIPs and nearby ZIPs,
    # to include restaurant heterogeneity

    # Chains
    zip.map.c <- zip.map
    names(zip.map.c) <- paste0(names(zip.map.c), 'c')
    for (k in names(zip.map.c)){
        zips.0 <- zip.map.c[[k]]
        zips.0.c <- paste0(zips.0, 'c')
        zips.0.i <- paste0(zips.0, 'i')
        zips.0 <- c(zips.0.c, zips.0.i)
        zip.map.c[[k]] <- zips.0
    }

    # Independents
    zip.map.i <- zip.map
    names(zip.map.i) <- paste0(names(zip.map.i), 'i')
    for (k in names(zip.map.i)){
        zips.0 <- zip.map.i[[k]]
        zips.0.c <- paste0(zips.0, 'c')
        zips.0.i <- paste0(zips.0, 'i')
        zips.0 <- c(zips.0.c, zips.0.i)
        zip.map.i[[k]] <- zips.0
    }

    ## Paste the two lists together
    zip.map.1 <- c(zip.map.c, zip.map.i)

    return(zip.map.1)
}
