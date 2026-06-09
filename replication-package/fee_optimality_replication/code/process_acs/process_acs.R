# Process 2019 ACS files
library(EconTools)


main <- function(){
    inpath.age     <- 'data/ACS/ACSST5Y2019.S0101_2021-12-27T205424/ACSST5Y2019.S0101_data_with_overlays_2021-11-06T184610.csv'
    inpath.edu     <- 'data/ACS/ACSST5Y2019.S1501_2021-12-27T205749/ACSST5Y2019.S1501_data_with_overlays_2021-11-08T185630.csv'
    inpath.marital <- 'data/ACS/ACSST5Y2019.S1201_2021-12-27T205628/ACSST5Y2019.S1201_data_with_overlays_2021-11-14T184552.csv'
    inpath.income  <- 'data/ACS/ACSST5Y2019.S1901_2022-04-11T163002/ACSST5Y2019.S1901_data_with_overlays_2021-11-04T111610.csv'

    create.dir('data/ACS')
    outdir <- 'data/ACS/processed'
    create.dir(outdir)
    outpath <- sprintf('%s/ACS_data.csv', outdir)

    age     <- process.age(inpath.age)
    marital <- process.marital(inpath.marital)
    edu     <- process.edu(inpath.edu)
    inc     <- process.inc(inpath.income)

    df <- dplyr::left_join(age, marital, by = 'zcta')
    df <- dplyr::left_join(df, edu, by = 'zcta')
    df <- dplyr::left_join(df, inc[, c('zcta', 'share_low_inc')], by = 'zcta')
    df$pop_over_18 <- df$pop_total
    df$pop_total <- NULL

    write.dat(df, file = outpath)
}

process.age <- function(inpath.age){
    # Age populations by ZCTA
    age <- read.dat(inpath.age, skip = 1)
    age <- fix.zcta(age)
    age <- remove.vars(age)
    age <- rename.var(age, 'Estimate..Total..Total.population', 'population')


    age.groups <- c('20.to.24', '25.to.29', '30.to.34', '35.to.39',
                    '40.to.44', '45.to.49', '50.to.54', '55.to.59',
                    '60.to.64')

    keep.vars <- c('zcta', 'population')
    for (ag in age.groups){
        varname <- paste0('Estimate..Total..Total.population..AGE..', ag, '.years')
        nv <- gsub('\\.', '_', ag)
        age[, nv] <- age[, varname]
        keep.vars <- c(keep.vars, nv)
    }

    var65plus  <- "Estimate..Total..Total.population..SELECTED.AGE.CATEGORIES..65.years.and.over"
    age <- rename.var(age, var65plus, '65+')
    keep.vars <- c(keep.vars, '65+')
    age <- age[, keep.vars]

    age.pops <- setdiff(colnames(age), c('zcta', 'population'))
    for (vn in age.pops){
        age[, paste0('share_', vn)] <- age[, vn]/age$population
    }

    return(age)
}

fix.zcta <- function(df){
    df <- rename.var(df, 'Geographic.Area.Name', 'zcta')
    df$zcta <- gsub('ZCTA5 ', '', df$zcta)
    return(df)
}

remove.vars <- function(df){
    drop.vars <- grep('Margin.of.Error', colnames(df), value = TRUE)
    drop.vars <- c(drop.vars, grep('(Male|Female)', colnames(df), value = TRUE))
    for (dv in drop.vars){
        df[[dv]] <- NULL
    }
    return(df)
}

process.marital <- function(inpath.marital){
    marital <- read.dat(inpath.marital, skip = 1)
    marital <- fix.zcta(marital)
    marital <- remove.vars(marital)
    marital <- rename.var(marital, 'Estimate..Total..Population.15.years.and.over', 'pop_over_15')
    marital <- marital[, !grepl('RACE', colnames(marital))]
    marital <- marital[, !grepl('PERCENT.ALLOCATED', colnames(marital))]
    marital <- marital[, !grepl('\\.1$', colnames(marital))]

    pop.cols <- setdiff(colnames(marital), c('id', 'zcta', 'pop_over_15'))
    for (pc in pop.cols){
        marital[, pc] <- sub('^-$', '0', marital[, pc])
        marital[, pc] <- as.numeric(marital[, pc])/100
    }


    ## Relabel variables
    colnames(marital) <- gsub('..Population.15.years.and.over', '', colnames(marital))
    colnames(marital) <- gsub('^Estimate..', '', colnames(marital))
    colnames(marital) <- gsub('\\.', '_', colnames(marital))
    marital <- rename.var(marital, 'Now_married__except_separated_', 'married')
    colnames(marital) <- tolower(colnames(marital))

    return(marital)
}

process.edu <- function(inpath.edu){
    edu <- read.dat(inpath.edu, skip = 1)
    edu <- fix.zcta(edu)
    edu <- remove.vars(edu)
    edu <- edu[, !grepl('(EARNINGS|POVERTY|RACE)', colnames(edu))]
    edu <- edu[, !grepl('Percent', colnames(edu))]

    colnames(edu) <- gsub('\\.', '_', colnames(edu))
    colnames(edu) <- gsub('^Estimate__Total__AGE_BY_EDUCATIONAL_ATTAINMENT__', '', colnames(edu))
    colnames(edu) <- gsub('Less_than_high_school_graduate', 'less_than_hs', colnames(edu))
    colnames(edu) <- gsub('High_school_graduate__includes_equivalency_', 'hs', colnames(edu))
    colnames(edu) <- gsub('Bachelor_s_degree_or_higher', 'bachelor_plus', colnames(edu))
    colnames(edu) <- gsub('Bachelor_s_degree', 'bachelor', colnames(edu))


    edu$less_than_hs <- edu$Population_18_to_24_years__less_than_hs +
                        edu$Population_25_years_and_over__Less_than_9th_grade +
                        edu$Population_25_years_and_over__9th_to_12th_grade__no_diploma
    edu$hs           <- edu$Population_18_to_24_years__hs +
                        edu$Population_25_years_and_over__hs +
                        edu$Population_18_to_24_years__Some_college_or_associate_s_degree +
                        edu$Population_25_years_and_over__Some_college__no_degree +
                        edu$Population_25_years_and_over__Associate_s_degree

    edu$college      <- edu$Population_18_to_24_years__bachelor_plus +
                        edu$Population_25_years_and_over__bachelor
    edu$advanced     <- edu$Population_25_years_and_over__Graduate_or_professional_degree

    edu$pop_total <- edu$Population_18_to_24_years + edu$Population_25_years_and_over
    edu.vars <- c('less_than_hs', 'hs', 'college', 'advanced')
    edu <- edu[, c('zcta', 'pop_total', edu.vars)]

    for (ev in edu.vars){
        nv <- paste0('share_', ev)
        edu[, nv] <- edu[, ev]/edu$pop_total
    }

    return(edu)
}

process.inc <- function(inpath.income){
    inc <- read.dat(inpath.income, skip = 1)
    inc <- fix.zcta(inc)
    inc <- remove.vars(inc)
    inc <- inc[, !grepl('Estimate\\.\\.Families', colnames(inc))]
    inc <- inc[, !grepl('Estimate\\.\\.Married',  colnames(inc))]
    inc <- inc[, !grepl('Estimate\\.\\.Nonfamily',  colnames(inc))]
    inc <- inc[, !grepl('Estimate\\.\\.Households\\.\\.(Median|Mean|PERCENT)',  colnames(inc))]

    colnames(inc) <- gsub('Estimate\\.\\.Households\\.\\.Total\\.\\.', 'Share', colnames(inc))
    # NOTE: share_low_inc is on a [0, 100] scale (ACS Share* columns are percents
    # and are not divided by 100 here), whereas the marital share variables
    # produced above are on [0, 1]. The lone downstream consumer
    # (explore_yipit/prepare_DiD_data.R) divides by 100 to compensate.
    inc$share_low_inc <- suppressWarnings(as.numeric(inc$ShareLess.than..10.000)  +
                             as.numeric(inc$Share.10.000.to..14.999) +
                             as.numeric(inc$Share.15.000.to..24.999) +
                             as.numeric(inc$Share.25.000.to..34.999) +
                             (1/3)*as.numeric(inc$Share.35.000.to..49.999))

    return(inc)
}


main()



