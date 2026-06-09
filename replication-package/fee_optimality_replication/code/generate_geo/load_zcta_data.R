# Add a ZCTA variable to the geo data.frame, and save a version of the ZCTA
# dataset with a limited number of variables
library(dplyr)
library(geosphere)
library(EconTools)

inpath.simple <- 'data/simplemaps_uszips_basicv1.77/uszips.csv'
inpath.geo    <- 'data/geo/geo.csv'
outpath.zcta  <- 'data/geo/ZCTAs.csv'
outpath.zip   <- 'data/geo/geo_with_zctas.csv'

df <- read.dat(inpath.simple, colClasses = c('zip' = 'character'))
df <- df[which(df$zcta), ]
df <- df[, c('zip', 'lat', 'lng', 'population')]
colnames(df) <- c('zcta', 'lat_zcta', 'lon_zcta', 'pop')

write.dat(df, file = outpath.zcta)

geo <- read.dat(inpath.geo, colClasses = c('zip' = 'character'))

# Manual corrections
idx <- which(geo$county == 'District of Columbia VA')
geo$county[idx] <- 'Loudoun County VA'
geo$county_name[idx] <- 'Loudoun County'

idx <- which(geo$county == 'Gadsden County GA')
geo$county[idx] <- 'Decatur County GA'
geo$county_name[idx] <- 'Decatur County'

idx <- which(geo$county == 'Whiteside County IA')
geo$county[idx] <- 'Clinton County IA'
geo$county_name[idx] <- 'Clinton County'

# Add zcta to geo
df$zip <- df$zcta
geo <- left_join(geo, df, by = 'zip')

geo$pop[which(is.na(geo$zcta))] <- 0

idx.non.zcta <- which(is.na(geo$zcta))
geo$is.zcta <- TRUE
geo$is.zcta[idx.non.zcta] <- FALSE
non.zctas <- geo$zip[idx.non.zcta]
zcta.fill <- c()
for (nz in non.zctas){
    idx.nz <- which(geo$zip == nz)
    nz.county <- geo$county[idx.nz]
    zctas.county <- geo[which(geo$county == nz.county), ]
    zctas.county <- zctas.county[which(!is.na(zctas.county$zcta)), ]

    if (nrow(zctas.county) == 0){
        lat.dist <- abs(geo$latitude - geo$latitude[idx.nz])
        lon.dist <- abs(geo$longitude - geo$longitude[idx.nz])
        zctas.county <- geo[which(pmax(lon.dist, lat.dist) <= 2), ]
        zctas.county <- zctas.county[which(!is.na(zctas.county$zcta)), ]
    }

    x <- as.numeric(geo[idx.nz, c('longitude', 'latitude')])
    y <- cbind(zctas.county$lon_zcta, zctas.county$lat_zcta)
    dists.nz <- as.numeric(distm(x, y))
    zcta.fill[nz] <- zctas.county$zcta[which.min(dists.nz)]
}

geo$zcta[idx.non.zcta] <- zcta.fill

write.dat(geo, outpath.zip, quote = TRUE)
