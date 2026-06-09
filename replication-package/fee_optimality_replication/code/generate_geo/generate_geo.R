# Specify a zip-code level dataset of geographical information
library(noncensus)
library(doBy)
library(EconTools)

data(corebased_areas)
data(zip_codes)
data(counties)

# Specify input paths
inpath.cbsa <- 'data/small_data/cbsa_codenames.csv'
inpath.zip <- 'data/simplemaps_uszips_basicv1.77/uszips.csv'
inpath.acs <- 'data/ACSDT5Y2019.B01003_2021-06-16T153509/ACSDT5Y2019.B01003_data_with_overlays_2021-06-12T231340.csv'

# Specify path to output file
outdir <- 'data/geo'
create.dir(outdir)
## Original ZIP code dataset before changing municipality variable
outpath.zip       <- sprintf('%s/geo.csv',   outdir)
## ZIP code dataset with revised municipality variable
outpath.zip.alt   <- sprintf('%s/geo_alt.csv',   outdir)
## Table of municipalities under first definition
outpath.munis     <- sprintf('%s/munis.csv', outdir)
## Table of municipalities under second definition
outpath.munis.alt <- sprintf('%s/munis_alt.csv', outdir)
## Mapping of ZIP codes to municipalities and counties
outpath.mkts <- sprintf('%s/zips_to_markets.csv', outdir)
## Table of county markets with indicator for inclusion in analysis
outpath.counties <- sprintf('%s/counties.csv', outdir)

# Manipulate counties data.frame
counties$fips <- paste0(counties$state_fips, counties$county_fips)
counties$state_fips  <- NULL
counties$county_fips <- NULL
counties <- rename.var(counties, 'population', 'county_pop')
keep.vars <- c('county_name', 'county_pop', 'fips', 'CBSA_name')

# Fix zip codes FIPs variable
zip_codes$fips <- as.character(zip_codes$fips)
zip_codes$fips <- ifelse(nchar(zip_codes$fips) == 4, 
                         paste0('0', zip_codes$fips), zip_codes$fips)

add.zip <- c(zip = '75084', city = 'Plano', state = 'TX',
             latitude = 33.0192, longitude = -96.7029, fips = '48085')
zip_codes <- rbind(zip_codes, add.zip)
## A missing zip code in Honolulu, Hawaii
add.zip <- c(zip = '96820', city = 'Honolulu', state = 'HI',
             latitude = 21.3524, longitude = -157.8838, fips = '15003')
zip_codes <- rbind(zip_codes, add.zip)
## A missing zip code in Honolulu, Hawaii
add.zip <- c(zip = '96802', city = 'Honolulu', state = 'HI',
             latitude = 21.3100, longitude = -157.8600, fips = '15003')
zip_codes <- rbind(zip_codes, add.zip)
## A missing zip code in Lompoc, California
add.zip <- c(zip = '93438',  city = 'Lompoc', state = 'CA',
             latitude = 34.6400, longitude = -120.4600, fips = '06083')
zip_codes <- rbind(zip_codes, add.zip)
## A missing zip code in SF, California
add.zip <- c(zip = '94188', city = 'San Francisco', state = 'CA',
             latitude = 37.7400, longitude = -122.3800, fips = '06075')
zip_codes <- rbind(zip_codes, add.zip)
## A missing zip code in SF, California
add.zip <- c(zip = '97003', city = 'Beaverton', state = 'OR',
             latitude = 45.5097, longitude = -122.8799, fips = '41067')
zip_codes <- rbind(zip_codes, add.zip)

# Merge counties to cbsa names
corebased_areas$CSA <- NULL
corebased_areas$type <- NULL
names(corebased_areas) <- c('CBSA', 'CBSA_name')
counties <- merge(counties, corebased_areas, by = 'CBSA',
                  all.x = TRUE, all.y = FALSE)
zip_codes <- merge(x = zip_codes, y = counties[, keep.vars], by = 'fips',
                   all.x = TRUE, all.y = FALSE)
zip_codes$municipality <- sprintf('%s %s', zip_codes$city, zip_codes$state)

# Redefine some of the municipalities
## Overriding counties
override.counties <- list(c(name = 'Westchester County', state = 'NY'),
                          c(name = 'Frederick County',   state = 'MD'),
                          c(name = 'Fairfax County',     state = 'VA'),
                          c(name = 'San Mateo County',   state = 'CA'))
for (cnty in override.counties){
    county.name  <- cnty['name']
    county.state <- cnty['state']
    idx <- which(zip_codes$county_name == county.name & zip_codes$state == county.state)
    zip_codes$municipality[idx] <- sprintf('%s %s', county.name, county.state)
}


# Generate county/state variable
zip_codes$county <- sprintf('%s %s', zip_codes$county_name, zip_codes$state)

# Fix some regions' names
# Correction for NYC in the geo dataset
nyc.counties <- c('Queens County NY', 'Kings County NY', 'Richmond County NY',
                  'Bronx County NY', 'New York County NY')
zip_codes$city[which(zip_codes$county %in% nyc.counties)] <- 'New York'

# Correction for LA neighbourhoods
la.cities <- c('Van Nuys', 'North Hollywood', 'Pacoima',
               'Northridge', 'Sylmar', 'Canoga Park', 
               'San Pedro', 'Reseda', 'Woodland Hills',
               'Panorama City', 'North Hills', 'Sherman Oaks',
               'Wilmington', 'Granada Hills')
idx.la <- (zip_codes$county == 'Los Angeles County CA') & 
    (zip_codes$city %in% la.cities)
zip_codes$city[which(idx.la)] <- 'Los Angeles'

zip_codes$city[which(zip_codes$city == 'Saint Louis')] <- 'St. Louis'
zip_codes$city[which(zip_codes$city == 'Saint Paul')] <- 'St. Paul'

centennial.zips <- c('80014', '80015', '80016',
                     '80111', '80112', '80121',
                     '80122', '80123', '80161')
zip_codes$city[which(zip_codes$zip %in% centennial.zips)] <- 'Centennial'

# Save the output file
write.csv(x = zip_codes, file = outpath.zip, row.names = FALSE)

# Market definitions
min.pop <- 100000
min.per.cbsa <- 3
cbsas <- read.csv(inpath.cbsa, stringsAsFactors = FALSE)
mkts <- zip_codes[which(zip_codes$CBSA_name %in% cbsas$CBSA_name), ]

alt.zip <- read.csv(inpath.zip, stringsAsFactors = FALSE)
keep.vars <- c('zip', 'lat', 'lng', 'city', 'state_id', 'state_name', 'zcta', 'population', 'density', 'county_fips', 'county_name')
alt.zip <- alt.zip[, keep.vars]
alt.zip$zip <- sapply(alt.zip$zip, fix.zip.codes)
alt.zip <- rename.var(alt.zip, 'city', 'city2')
alt.zip <- rename.var(alt.zip, 'lng', 'lon')

acs.zip <- read.csv(inpath.acs, stringsAsFactors = FALSE, skip = 1)
colnames(acs.zip) <- c('id', 'zip', 'population.acs', 'moe')
acs.zip$zip <- sub('ZCTA5 ', '', acs.zip$zip)
acs.zip <- acs.zip[, c('zip', 'population.acs')]

merge.in.vars <- c('zip', 'population', 'city2', 'lat', 'lon')
mkts$latitude  <- NULL
mkts$longitude <- NULL
mkts <- merge(x = mkts, y = alt.zip[, merge.in.vars], by = 'zip')
mkts <- merge(x = mkts, y = acs.zip, by = 'zip')

munis <- summaryBy(population + population.acs ~ municipality, 
                   data = mkts, FUN = sum, id = 'CBSA_name')
munis <- munis[order(munis$CBSA_name, -munis$population.sum), ]

munis$population.acs.sum <- NULL
munis$include <- 0
munis <- rename.var(munis, 'population.sum', 'population')
munis$CBSA_name <- as.character(munis$CBSA_name)

# Implement the decision rule for selecting municipalities
## Include all municipalities with at least min.pop population and at least
## min.per.cbsa municipalities within each CBSA
idx <- which(munis$population >= min.pop)
include.munis <- munis$municipality[idx]
for (cbsa in unique(munis$CBSA_name)){
    idx <- which(munis$CBSA_name == cbsa)
    cbsa.df <- munis[idx, ]
    cbsa.df <- cbsa.df[order(-cbsa.df$population), ]
    cbsa.munis <- cbsa.df$municipality[1:min(min.per.cbsa, nrow(cbsa.df))]
    include.munis <- c(include.munis, cbsa.munis)
}
# Make some manual additions
manual.additions <- c('Skokie IL', 'Evanston IL')
include.munis <- c(include.munis, manual.additions)

include.munis <- unique(include.munis)
munis$include[which(munis$municipality %in% include.munis)] <- 1

# Save the data
write.csv(x = munis, file = outpath.munis, row.names = FALSE)

# Now use a version in which excluded municipalities are grouped together into counties
min.pop.cnty <- 100000
mkts.alt <- merge(x = mkts, y = munis[, c('municipality', 'include')], 
                  by = 'municipality')
idx <- which(mkts.alt$include == 0)
excluded.munis <- unique(mkts.alt$municipality[idx])

mkts.alt$county_muni <- 0
mkts.alt$county_muni[idx]  <- 1
mkts.alt$municipality[idx] <- mkts.alt$county_name[idx]
mkts.alt$municipality[idx] <- sprintf('%s %s', mkts.alt$municipality[idx], mkts.alt$state[idx])

# Also revise the zip code dataset
zip.idx <- which(zip_codes$municipality %in% excluded.munis)
zip_codes$municipality[zip.idx] <- 
    sprintf('%s %s', zip_codes$county_name[zip.idx], zip_codes$state[zip.idx])
write.csv(zip_codes, file = outpath.zip.alt, row.names = FALSE)

munis.alt <- summaryBy(population ~ municipality, data = mkts.alt, FUN = sum,
                       id = c('county_muni', 'CBSA_name'), keep.names = TRUE)
munis.alt <- munis.alt[order(munis.alt$CBSA_name, -munis.alt$population), ]
munis.alt$include <- 1*(munis.alt$population >= min.pop.cnty)
munis.alt$include[munis.alt$county_muni == 0] <- 1
write.csv(x = munis.alt, file = outpath.munis.alt, row.names = FALSE)

# Save mapping from ZIP codes to markets
mkts.alt <- zip_codes[, c('zip', 'municipality', 'county', 'CBSA_name')]
mkts.alt <- mkts.alt[which(mkts.alt$municipality %in% munis.alt$municipality), ]

mkts.alt$include <- 1*(mkts.alt$municipality %in% munis.alt$municipality[munis.alt$include == 1])
mkts.alt <- merge(x = mkts.alt, y = alt.zip[, c('zip', 'lat', 'lon')], by = 'zip')
write.csv(mkts.alt, file = outpath.mkts, row.names = FALSE)

# County level analysis
counties.df <- summaryBy(population ~ county, 
                         data = mkts, FUN = sum, id = c('CBSA_name'))
counties.df <- counties.df[order(counties.df$CBSA_name, -counties.df$population.sum), ]
counties.df <- rename.var(counties.df, 'population.sum', 'population')
counties.df$include <- 1*(counties.df$population >= min.pop.cnty)
write.csv(x = counties.df, file = outpath.counties, row.names = FALSE)


# Message from data provider for simplemaps:
# We've used GIS software to calculate the centroid for every 
# ZIP code categorized as a ZCTA. A centroid is the average 
# position of all of the points in a shape. This is best way to 
# convert an area into a point. For zip codes that represent 
# discrete points, we've used authoritative sources such as the 
# National Weather Service to geocode them into latitude and 
# longitude.
