# Process the Infogroup data on restaurant locations
library(EconTools)

main <- function(){
    # '2019_2020' produces data/infogroup/infogroup_2020.rds (consumed by
    # process_yipit/add_infogroup.R, Table 1 chain). It also produces
    # infogroup_2019.rds, which has no current consumer.
    # '2021' produces data/infogroup/infogroup_2021.rds (consumed by
    # process_yipit/add_infogroup.R and restaurant_price_sample/, OA Tab G1).
    # The '2022' dispatch branch is retained below for documentation but is
    # not called — no exhibit consumes infogroup_2022.rds.
    process.infogroup('2019_2020')
    process.infogroup('2021')
}


process.infogroup <- function(which.years){
    data.dir <- 'data/infogroup'
    if (which.years == '2019_2020'){
        inpath <- paste0(data.dir, '/jjcdvjugq1hsjpmn.csv')
    } else if (which.years == '2021') {
        inpath <- paste0(data.dir, '/hiyy2ujx9xeo2wtl.csv')
    } else if (which.years == '2022'){
        inpath <- paste0(data.dir, '/ciak14bdgvcfxaef.csv')
    }
    dat <- read.dat(inpath)

    # Fix the zip code variable
    dat$zipcode <- fix.zip.codes(dat$zipcode)

    # Drop some unused variables
    drop.vars <- c('cbsa_code', 'sic_code_2', 'sic_code_1', 'sic_code', 'sic6_descriptions_sic',
                   'primary_naics_code', 'naics8_descriptions', 'county_code', 'ticker')
    for (dv in drop.vars){
        dat[, dv] <- NULL
    }

    # Drop offices associated with restaurants
    office.terms <- c('FRANCHISE OFFICE', 'MANAGEMENT OFFICE', 'CATERING OFFICE',
                      'CORP OFFICE', 'BUSINESS OFFICE', 'REGIONAL OFFICE','RESTAURANT OFFICE',
                      'MAIN OFFICE', 'GENERAL OFFICE', 'CLUB OFFICE', 'DISTRICT OFFICE',
                      'LIC OFFICE', 'FOODS OFFICE', 'BANQUET OFFICE', '\\-OFFICE$', 'ADMINISTRATIVE OFFICE',
                      'CONSTRUCTION OFFICE', 'SUPPORT OFFICE', 'SATELLITE OFFICE',
                      'CO OFFICE', 'CENTRAL OFFICE', 'SALES OFFICE', 'LEASING OFFICE', 'CORPORATE OFFICE')
    for (ot in office.terms){
        idx <- which(!grepl(ot, dat$company, ignore.case = TRUE))
        print(ot)
        print(nrow(dat) - length(idx))
        dat <- dat[idx, ]
    }

    # Divide by year
    if (which.years == '2019_2020'){
        dat.19 <- dat[which(dat$archive_version_year == 2019), ]
        dat.20 <- dat[which(dat$archive_version_year == 2020), ]

        dat.19$archive_version_year <- NULL
        dat.20$archive_version_year <- NULL

        # Save the data
        outpaths <- paste0(data.dir, '/infogroup_2019', c('.rds', '.csv'))
        saveRDS(dat.19, file = outpaths[1])
        write.dat(dat.19, file = outpaths[2])

        outpaths <- paste0(data.dir, '/infogroup_2020', c('.rds', '.csv'))
        saveRDS(dat.20, file = outpaths[1])
        write.dat(dat.20, file = outpaths[2])
    } else if (which.years %in% c('2021', '2022')) {
        dat$archive_version_year <- NULL
        keep.vars <- c("abi"                  , "parent_number",          "company"          ,      "address_line_1"   ,     "city",
                      "state"                 , "zipcode"      ,          "primary_sic_code" ,      "sic6_descriptions",     "sic6_descriptions_sic1",
                      "sic6_descriptions_sic2", "latitude"     ,          "longitude")
        dat <- dat[, keep.vars]
        filename <- sprintf('%s/infogroup_%s', data.dir, which.years)
        outpaths <- paste0(filename, c('.rds', '.csv'))

        # Save the data
        saveRDS(dat,   file = outpaths[1])
        write.dat(dat, file = outpaths[2])
    }

}

main()
