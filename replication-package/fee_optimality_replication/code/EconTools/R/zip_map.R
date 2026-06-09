
zip.map <- function(){
    # Construct a data.frame mapping ZIP code to other geographical variables
    data(zip_codes, package = 'noncensus')
    data(counties, package = 'noncensus')

    keep.vars <- c('zip', 'fips')
    zip_codes <- zip_codes[, keep.vars]

    # Make some manual additions to the zip codes file
    ## A missing zip code in Plano, Texas
    add.zip <- c(zip = 75084, fips = 48085)
    zip_codes <- rbind(zip_codes, add.zip)
    ## A missing zip code in Honolulu, Hawaii
    add.zip <- c(zip = 96820, fips = 15003)
    zip_codes <- rbind(zip_codes, add.zip)
    ## A missing zip code in Lompoc, California
    add.zip <- c(zip = 93438, fips = 06083)
    zip_codes <- rbind(zip_codes, add.zip)
    ## A missing zip code in SF, California
    add.zip <- c(zip = 94188, fips = 06075)
    zip_codes <- rbind(zip_codes, add.zip)
    counties$fips <- sprintf('%s%s', counties$state_fips, counties$county_fips)
    keep.vars <- c('state', 'CSA', 'CBSA', 'fips', 'county_name')
    counties <- counties[, keep.vars]
    # Merge counties into the zip codes data
    zip_codes$fips <- convert.fips(zip_codes$fips)
    geo <- merge(x = zip_codes, y = counties, by = 'fips',
                 all.x = TRUE)

    return(geo)
}

