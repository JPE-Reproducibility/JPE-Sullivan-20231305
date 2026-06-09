load.geo <- function(inpath.geo){
    # Geographical data
    CC <- c('zcta' = 'character', 'zip' = 'character')
    geo <- EconTools::read.dat(inpath.geo, colClasses = CC)
    geo$fips <- EconTools::convert.fips(geo$fips)
    # Make fixes
    geo <- fix.geo(geo)

    return(geo)
}

load.zip.pop <- function(inpath.zip.pop){
    # Load data with ZCTA populations
    zip.pop <- EconTools::read.dat(inpath.zip.pop, colClasses = c('zcta' = 'character'))
    zip.pop <- zip.pop[, c('zcta', 'population')]

    # Collapse to ZIP3 level
    zip.pop$zip3 <- substr(zip.pop$zcta, 1, 3)
    zip.pop3 <- doBy::summaryBy(population ~ zip3, data = zip.pop, FUN = sum,
                                keep.names = TRUE)
    zip.pop3$population <- zip.pop3$population/1e6 # in millions

    zip.pop.data <- list(zip.pop = zip.pop, zip.pop3 = zip.pop3)
    return(zip.pop.data)
}

load.policy <- function(inpath.policy){
    ## COVID-19 policy stringency
    policy <- EconTools::read.dat(inpath.policy)
    policy$month <- paste0(policy$month, '-01')
    policy <- policy[, c('state', 'month', 'StringencyIndex')]
    colnames(policy) <- c('state', 'month', 'stringency')
    return(policy)
}

load.covid <- function(inpath.covid, geo, zip.pop){
    # Load and process data on COVID-19 rates

    # Load the data
    covid <- readRDS(inpath.covid)
    covid <- covid[, c('fips', 'month', 'new_cases')]
    covid$month <- paste0(covid$month, '-01')

    # Delete Puerto Rico
    fips.PR <- geo$fips[which(geo$state == 'PR')]
    covid <- covid[which(!(covid$fips %in% fips.PR)), ]
    geo <- geo[which(geo$state != 'PR'), ]

    ### For the purpose of merging, we need to take into account some
    ### changes in FIPs codes that recently occurred
    ### - Wade-Hampton Census Area became Kusilvak Census Area
    covid$fips[which(covid$fips == '02158')] <- '02270'
    ### - Shannon County became Oglala Lakota County
    covid$fips[which(covid$fips == '46102')] <- '46113'

    # Merge with county names and populations
    geo.fips <- geo[, c('fips', 'county', 'county_pop')]
    geo.fips <- geo.fips[!duplicated(geo.fips), ]
    months <- covid$month
    covid.all <- list()
    for (month in months){
        covid.m <- covid[which(covid$month == month), ]
        covid.m <- dplyr::inner_join(covid.m, geo.fips, by = 'fips')
        covid.all[[month]] <- covid.m
    }
    covid <- dplyr::bind_rows(covid.all)
    covid$new_cases_pc <- covid$new_cases/covid$county_pop

    # Produce a ZIP3-level COVID dataset
    geo.sub <- geo[, c('zcta', 'county')]
    geo.sub <- dplyr::left_join(geo.sub, zip.pop, by = 'zcta')
    # Now, merge in covid rates
    covid.sub <- covid[, c('month', 'county', 'new_cases_pc')]
    months <- unique(covid.sub$month)
    covid3 <- list()
    for (month in months){
        covid.sub.m <- covid.sub[which(covid.sub$month == month), ]
        geo.sub.m <- dplyr::left_join(geo.sub, covid.sub.m, by = 'county')
        # Compute weighted average of new_cases_pc by population
        geo.sub.m$new_cases_pc_pop <- geo.sub.m$new_cases_pc*geo.sub.m$population
        geo.sub3 <- doBy::summaryBy(new_cases_pc_pop + population ~ zip3,
                                    data = geo.sub.m, FUN = sum, id = 'month',
                                    keep.names = TRUE)
        covid3[[month]] <- geo.sub3
    }
    covid3 <- dplyr::bind_rows(covid3)
    covid3 <- covid3[which(!is.na(covid3$new_cases_pc_pop)), ]
    covid3$new_cases_pc <- covid3$new_cases_pc_pop/covid3$population
    covid3$new_cases_pc[which(covid3$population == 0)] <- 0

    covid.dat <- list(covid = covid, covid3 = covid3)
    return(covid.dat)
}


process.political.data <- function(inpath.election, geo, zip.pop, by.zcta = FALSE){
    # Prepare political data to be merged into the main data in Stata

    # Read and process the elections data
    elect <- EconTools::read.dat(inpath.election, colClasses = c('county_fips' = 'character'))
    elect <- elect[which(elect$year == 2020 & elect$office == 'US PRESIDENT'), ]
    elect <- elect[which(elect$party == 'DEMOCRAT'), ]
    ## Collapse across modes
    elect$county_name <- paste(elect$county_name, elect$state_po, sep = ' ')
    elect.all.modes <- doBy::summaryBy(candidatevotes ~ county_name,
                                       data = elect, id = 'county_fips',
                                       FUN = sum)
    colnames(elect.all.modes) <- c('county_name', 'democrat_vote', 'county_fips')
    elect.tot <- elect[, c('county_name', 'county_fips', 'totalvotes')]
    elect.tot <- elect.tot[!duplicated(elect.tot), ]
    elect <- dplyr::left_join(elect.all.modes, elect.tot, by = c('county_name', 'county_fips'))
    elect$democrat_share <- elect$democrat_vote/elect$totalvotes

    # Assign a FIPs to DC
    elect$county_fips[which(elect$county_name == 'DISTRICT OF COLUMBIA DC')] <- '11001'

    # Make a version as the ZIP3 level, weighting each ZIP's county by population
    ## First, handle FIPs that are in both elect and geo
    elect <- rename.var(elect, 'county_fips', 'fips')
    fips.common <- intersect(elect$fips, geo$fips)
    elect.sub <- elect[which(elect$fips %in% fips.common), ]
    geo.sub   <- geo[which(geo$fips %in% fips.common), ]
    geo.sub <- dplyr::left_join(geo.sub, zip.pop[, c('zcta', 'population')], by = 'zcta')
    geo.sub <- geo.sub[geo.sub$is.zcta, ]
    geo.sub <- dplyr::left_join(geo.sub, elect.sub[, c('fips', 'democrat_share')],
                                by = 'fips')

    if (by.zcta){
        geo.sub <- geo.sub[, c('zip', 'democrat_share')]
        return(geo.sub)
    } else {
        geo.sub$zip3 <- substr(geo.sub$zcta, 1, 3)
        geo.sub$democrat_share_pop <- geo.sub$democrat_share*geo.sub$population

        # Compute weighted average of new_cases_pc by population
        geo.sub3 <- doBy::summaryBy(democrat_share_pop + population ~ zip3,
                                    data = geo.sub, FUN = sum,
                                    keep.names = TRUE)
        geo.sub3$democrat_share <- geo.sub3$democrat_share_pop/geo.sub3$population
        geo.sub3 <- geo.sub3[, c('zip3', 'democrat_share')]

        ## Now take care of the other FIPs in Alaska
        missing.ak <- setdiff(geo$fips[geo$state == 'AK'], elect$fips)
        ### Assume there are missing AK observations for both elect and geo becaus
        ### of a coding difference
        missing.elect.ak <- setdiff(elect$fips[grep('AK$', elect$county_name)], geo$fips)
        idx <- which(elect$fips %in% missing.elect.ak)
        AK.share <- sum(elect$democrat_vote[idx])/sum(elect$totalvotes[idx])
        geo.sub.ak <- geo[which(geo$fips %in% missing.ak), ]
        geo.sub.ak <- geo.sub.ak[, c('zcta', 'is.zcta')]
        geo.sub.ak <- geo.sub.ak[geo.sub.ak$is.zcta, ]
        geo.sub.ak$zip3 <- substr(geo.sub.ak$zcta, 1, 3)
        geo.sub.ak <- geo.sub.ak[!duplicated(geo.sub.ak$zip3), ]
        geo.sub.ak <- geo.sub.ak[which(!(geo.sub.ak$zip3 %in% geo.sub3$zip3)), ]
        geo.sub.ak$democrat_share <- AK.share
        geo.sub.ak <- geo.sub.ak[, c('zip3', 'democrat_share')]
        geo.sub3 <- dplyr::bind_rows(geo.sub3, geo.sub.ak)
        geo.sub3 <- geo.sub3[!is.na(geo.sub3$democrat_share), ]
        return(geo.sub3)
    }
}
