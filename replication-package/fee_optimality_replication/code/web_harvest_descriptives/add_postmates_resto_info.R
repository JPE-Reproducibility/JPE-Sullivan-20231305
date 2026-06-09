## Merge YipitData restaurant data into the Postmates orders dataset
library(data.table)
library(geosphere)
library(dplyr)
library(stringdist)
library(EconTools)

inpath.pm <- 'data/web_harvesting/postmates_all_restaurants/nondelivery_fees/geocoded_orders.csv'
inpath.YD <- 'data/yipitdata/listings_v2.csv'
dropvars <- c('open_hours', 'open_at_observation_time',
              'offers_pickup', 'phone_number',
              'estimated_min_order_value_cents', 'metro_tier',
              'restaurant_tags', 'estimated_delivery_fee_cents',
              'address_country', 'reference_observation_in_month',
              'fulfills_own_deliveries')

outpath <- 'data/web_harvesting/analysis_data/postmates_with_resto_info.csv'



YD <- fread(inpath.YD, drop = dropvars)

# Postmates listings in YipitData are sparse, so we match each web-harvested Postmates record
# against listings on ALL four platforms in April-May 2021 and apply a +3
# distance penalty for non-Postmates matches (line below) — i.e., cross-platform
# matches are accepted only when same-platform candidates are clearly worse.
months <- c('2021-04-01', '2021-05-01')
YD <- YD[observation_month %in% months]
YD <- YD[platform %in% c('Postmates', 'Grubhub', 'DoorDash', 'UberEats')]

# Find each df.pm restaurant in the YD dataset
colnames(YD)[colnames(YD) == 'address_street'] <- 'streetAddress_YD'
colnames(YD)[colnames(YD) == 'postal_code']    <- 'zip_YD'
colnames(YD)[colnames(YD) == 'latitude']       <- 'latitude_YD'
colnames(YD)[colnames(YD) == 'longitude']      <- 'longitude_YD'
colnames(YD)[colnames(YD) == 'name']      <- 'resto_name_YD'

df.pm   <- read.csv(inpath.pm, stringsAsFactors = FALSE)
# Remove extra zip characters
df.pm$zip <- sub('-[0-9]+$', '', df.pm$zip)
df.pm$zip <- sub('\\*$', '', df.pm$zip)
df.pm$zip <- fix.zip.codes(df.pm$zip)
df.pm$restaurant_id     <- NA_character_
df.pm$platform          <- NA_character_
# Match YD$observation_month's IDate type so the downstream left_join works.
df.pm$observation_month <- as.IDate(rep(NA_character_, nrow(df.pm)))

# Shuffle for evaluation purposes
set.seed(2)
df.pm <- df.pm[sample(1:nrow(df.pm), nrow(df.pm), replace = FALSE), ]

df.pm$resto_name <- sub(' \\(.*\\)$', '', df.pm$resto_name)

for (k in 1:nrow(df.pm)){
    lat.k <- df.pm$latitude[k]
    lon.k <- df.pm$longitude[k]
    idx <- which((YD$latitude_YD  <= lat.k + 0.15) & (YD$latitude_YD  >= lat.k - 0.15) &
                 (YD$longitude_YD <= lon.k + 0.15) & (YD$longitude_YD >= lon.k - 0.15))
    YD.sub <- YD[idx, ]
    YD.sub$resto_name_YD <- sub(' \\(.*\\)$', '', YD.sub$resto_name_YD)

    if (nrow(YD.sub) == 0){
        next
    }

    # Distance 1: geographical. When the Postmates record lacks coordinates,
    # fall back to a zero-distance prior so the match is decided by name &
    # address similarity alone.
    if (!is.na(df.pm$longitude[k])){
        p1 <- c(df.pm$longitude[k], df.pm$latitude[k])
        p2 <- cbind(YD.sub$longitude_YD, YD.sub$latitude_YD)
        dists.geo <- distGeo(p1, p2)
    } else {
        dists.geo <- rep(0, times = nrow(YD.sub))
    }
    # Distance 2: name
    dists.name <- stringdist(df.pm$resto_name[k], YD.sub$resto_name_YD)
    # Distance 3: street address
    dists.street <- stringdist(df.pm$streetAddress[k], YD.sub$streetAddress_YD)
    # Distance 4: platform
    dists.platform <- 1*(YD.sub$platform != 'Postmates')

    total.dist <- dists.geo/10 + 3*dists.name + 0.5*dists.street + 3*dists.platform
    if (length(total.dist) == 0 || all(is.na(total.dist))) {
        # No usable candidate match -- e.g., name/address are NA. Skip.
        next
    }
    rID       <- YD.sub$restaurant_id[which.min(total.dist)]
    rplatform <- YD.sub$platform[which.min(total.dist)]
    rmonth    <- YD.sub$observation_month[which.min(total.dist)]
    df.pm$restaurant_id[k]     <- rID
    df.pm$platform[k]          <- rplatform
    df.pm$observation_month[k] <- rmonth
}

# Merge in YipitData
df.pm <- left_join(df.pm, YD, by = c('restaurant_id', 'platform', 'observation_month'))
df.pm <- rename.var(df.pm, 'platform', 'platform_YD')

create.dir(dirname(outpath))
write.dat(df.pm, outpath, quote = TRUE)


