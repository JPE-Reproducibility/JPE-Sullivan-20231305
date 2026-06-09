# Make a mapping between zip codes and sets of zip codes that are
# within a certain radius
library(geosphere)
library(EconTools)

# Specify options
kilos.per.mile <- 1.60934
max.miles <- 5
max.kilos <- max.miles*kilos.per.mile

# Specify paths
inpath.geo <- 'data/geo/geo_with_zctas.csv'
outpath <- 'data/geo/zcta_map.rds'

geo <- read.dat(inpath.geo, colClasses = c('zip'  = 'character',
                                           'zcta' = 'character'))
geo <- geo[which(geo$is.zcta), ]
states <- unique(geo$state)

zip.map <- list()
for (state in states){
    print(state)
    state.zips <- geo$zcta[geo$state == state]
    geo.sub <- geo[which(geo$state == state), ]
    
    zip.coords   <- cbind(geo.sub$lon_zcta, geo.sub$lat_zcta)
    dist.mat <- distm(zip.coords)/1000
    
    rownames(dist.mat) <- state.zips
    close.mat <- 1*(dist.mat <= max.kilos)

    zip.map[[state]] <- lapply(1:nrow(close.mat), function(k) state.zips[which(close.mat[k, ] == 1)])
    names(zip.map[[state]]) <- state.zips
}

# Combine the lists
zip.map <- Reduce(c, zip.map)

# Save the lists
saveRDS(zip.map, file = outpath)
