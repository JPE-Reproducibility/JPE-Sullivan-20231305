fix.geo <- function(geo){
    # Correct some problems in the geographical data

    ### - Bedford City merged with Bedford County
    county.pop <- unique(geo$county_pop[which(geo$fips == '51019')])
    geo$fips[which(geo$fips == '51515')] <- '51019'
    idx <- which(geo$fips == '51019')
    geo$county[idx]      <- 'Bedford County VA'
    geo$county_name[idx] <- 'Bedford County'
    geo$county_pop[idx]  <- county.pop

    ## Some ZCTAs in geo are incorrect - fix them
    ### 39818 is in Decatur County
    idx <- geo$county == 'Decatur County GA'
    idx.right <- which(geo$fips == '13087' & idx)
    idx.wrong <- which(geo$fips == '12039' & idx)
    geo$fips[idx.wrong] <- '13087'
    geo$county_pop[idx.wrong] <- geo$county_pop[idx.right[1]]

    # Clinton County IA's FIPS code is 19045
    pop <- unique(geo$county_pop[which(geo$fips == '19045')])
    geo$fips[which(geo$county == 'Clinton County IA')] <- '19045'
    geo$county_pop[which(geo$county == 'Clinton County IA')] <- pop

    # Scott County IL is getting confused with Scott County IA
    idx <- which(geo$county == 'Scott County IL')
    geo$fips[idx] <- '17171'
    geo$county_pop[idx] <- 5355

    # Similar issue with Clay County MO and Clay County AK
    idx <- which(geo$county == 'Clay County MO')
    geo$fips[idx] <- '29047'
    geo$county_pop[idx] <- median(geo$county_pop[idx])

    # Dulles VA is assigned to Washington DC
    idx <- which(geo$county == 'Loudoun County VA')
    geo$fips[idx] <- '51107'
    geo$county_pop[idx] <- median(geo$county_pop[idx])

    # ZIP 31788 is given the incorrect state and fips
    idx <- which(geo$zip == '31788')
    geo$state[idx] <- 'GA'
    geo$city[idx] <- 'Moultrie'
    geo$municipality[idx] <- 'Moultrie GA'
    geo$county[idx] <- 'Colquitt County GA'
    geo$county_name[idx] <- 'Colquitt County'

    # This ZIP doesn't exist
    geo <- geo[which(geo$zip != '79841'), ]
    geo <- geo[which(geo$zip != '58234'), ]
    geo <- geo[which(geo$zip != '78081'), ]

    # Independent city in Virginia
    idx.right <- which(geo$fips == '51520')
    idx <- which(geo$zip == '24205')
    geo$fips[idx]        <- '51520'
    geo$county_name[idx] <- unique(geo$county_name[idx.right])
    geo$county[idx]      <- unique(geo$county[idx.right])
    geo$county_pop[idx]  <- unique(geo$county_pop[idx.right])

    # Large parts of Portland OR are assigned to a Washington county
    idx <- which(geo$county == 'Clark County OR')
    idx.right <- which(geo$county == 'Multnomah County OR')
    geo$county[idx] <- 'Multnomah County OR'
    geo$county_name[idx] <- 'Multnomah County'
    geo$county_pop[idx] <- unique(geo$county_pop[idx.right])
    geo$fips[idx] <- unique(geo$fips[idx.right])

    # Some counties in MD assigned to a WV county
    idx <- which(geo$county == 'Berkeley County MD')
    idx.right <- which(geo$county == 'Washington County MD')
    geo$county[idx] <- 'Washington County MD'
    geo$county_name[idx] <- 'Washington County'
    geo$county_pop[idx] <- unique(geo$county_pop[idx.right])
    geo$fips[idx] <- unique(geo$fips[idx.right])

    return(geo)
}
