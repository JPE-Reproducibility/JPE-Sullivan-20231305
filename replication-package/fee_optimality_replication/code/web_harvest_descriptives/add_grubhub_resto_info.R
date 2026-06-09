# Add data from YipitData to the Grubhub order details data
library(geosphere)
library(data.table)
library(dplyr)
library(EconTools)

# Specify paths
inpath.gh.details <- 'data/web_harvesting/grubhub_CBSAs/geocoded_details.csv'
inpath.YD <- 'data/yipitdata/listings_v2.csv'
dropvars <- c('open_hours', 'open_at_observation_time',
              'offers_pickup', 'phone_number',
              'estimated_min_order_value_cents', 'metro_tier',
              'restaurant_tags', 'estimated_delivery_fee_cents',
              'address_country', 'reference_observation_in_month',
              'fulfills_own_deliveries')

## To outputs
outdir <- 'data/web_harvesting/analysis_data'
create.dir(outdir)
outpath <- sprintf('%s/grubhub_with_resto_info.csv', outdir)

df.gh <- read.csv(inpath.gh.details, stringsAsFactors = FALSE)
YD <- fread(inpath.YD, drop = dropvars)
YD <- YD[platform == 'Grubhub']

# Limit to recent observations
YD <- YD[grep('^2021', YD$observation_month), ]

# Drop duplicates
YD <- YD[!duplicated(YD[, c('restaurant_id')]), ]

determine.id <- function(link){
    link.parts <- strsplit(link, '/')
    id <- link.parts[[1]]
    id <- id[length(id)]
    return(id)
}


df.gh$restaurant_id <- sapply(df.gh$link, determine.id)
colnames(YD)[colnames(YD) == 'name'] <- 'name_YD'

df.gh <- left_join(df.gh, YD, by = 'restaurant_id')

## Add distance
cat('Adding distances\n')
df.gh$distance <- NA
for (k in 1:nrow(df.gh)){
    if (is.na(df.gh$longitude[k])){
        next
    }
    collect.loc <- c(df.gh$collection_lon[k], df.gh$collection_lat[k])
    resto.loc    <- cbind(df.gh$longitude[k],  df.gh$latitude[k])
    df.gh$distance[k] <- distGeo(p1 = collect.loc, p2 = resto.loc)
    df.gh$distance[k] <- df.gh$distance[k]/1000 # in kilometres
}

write.dat(df.gh, outpath, quote = TRUE)


