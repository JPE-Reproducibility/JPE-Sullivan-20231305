# Add Infogroup restaurants into the YipitData restaurant listings dataset
library(dplyr)
library(stringr)
library(EconTools)

library(FoodDeliveryTools)

main <- function(){
    r.times <- FoodDeliveryTools::platform.adoption.times()
    years   <- r.times$years
    months  <- r.times$months

    for (year in years){
        for (month in months[[year]]){
            add.infogroup(month, year)
        }
    }
}

add.infogroup <- function(month, year){

    inpath.yd <- sprintf('data/yipitdata/brands_%s%s.rds', month, year)
    inpath.ig <- sprintf('data/infogroup/infogroup_%s.rds', year)
    inpath.geo <- 'data/geo/geo.csv'
    outpath <- sprintf('data/yipitdata/brands_w_infogroup%s_%s%s.rds',
                       year, month, year)

    df <- readRDS(inpath.yd)
    ig <- readRDS(inpath.ig)
    geo <- read.dat(inpath.geo)
    geo$zip <- fix.zip.codes(geo$zip)

    drop.vars <- c('primary_sic_code', 'sic6_descriptions',
                   'sic6_descriptions_sic1', 'sic6_descriptions_sic2', 'abi', 'city')
    for (dv in drop.vars){
        ig[, dv] <- NULL
    }

    ig$platform <- 'offline'
    ig <- rename.var(ig, 'company', 'restaurant')
    ig <- rename.var(ig, 'zipcode', 'zip')
    ig <- rename.var(ig, 'longitude', 'lon')
    ig <- rename.var(ig, 'latitude', 'lat')
    ig <- rename.var(ig, 'address_line_1', 'address')

    # Convert certain variables to title case
    ig$address <- str_to_title(ig$address)

    # Convert restaurant name to lower case
    ig$restaurant <- tolower(ig$restaurant)

    # Add chains
    ig$brand <- NA
    for (brand in names(brand.map)){
        idx <- grep(brand.map[brand], ig$restaurant, ignore.case = TRUE)
        ig$brand[idx] <- brand
    }

    # Rename some YipitData variables
    df <- rename.var(df, 'postal_code', 'zip')
    df <- rename.var(df, 'latitude',  'lat')
    df <- rename.var(df, 'longitude', 'lon')
    df <- rename.var(df, 'name',     'restaurant')

    # Delete geographical variables from YipitData (we will merge in
    # other geo variables later)
    delete.vars <- c('address_region', 'CBSA_name',
                     'address_locality', 'observation_month')
    for (dv in delete.vars){
        df[, dv] <- NULL
    }
    # Delete certain Infogroup variables as well
    ig$state         <- NULL
    ig$parent_number <- NULL
    
    ig <- rename.var(ig, 'address', 'address_street')

    df <- bind_rows(df, ig)
    df <- left_join(df, geo[, c('zip', 'county_name', 'fips', 'city', 'CBSA_name')], by = 'zip')

    # Save
    saveRDS(df, file = outpath)
}

main()
