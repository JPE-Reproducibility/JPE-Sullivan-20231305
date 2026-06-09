# Produce a weekly ZIP-level fee-cap dataset from the hand-coded CSV listing
# of cap policies (start date, end date, jurisdiction, level, chain exclusion).
# Output: data/fee_caps/zip_fee_caps.rds — consumed by the Table 1, Appendix
# Table A1, and OA Figure D2 chains and by FoodDeliveryTools/R/prepare_data.R.
library(doBy)
library(dplyr)
library(EconTools)
library(FoodDeliveryTools)

# Inputs
inpath.caps <- 'data/small_data/commission_caps.csv'
inpath.geo  <- 'data/geo/geo_with_zctas.csv'
inpath.pop  <- 'data/simplemaps_uszips_basicv1.77/uszips.csv'

# Output
outdir.dat <- 'data/fee_caps'
create.dir(outdir.dat)
outpath.dat <- paste0(outdir.dat, '/zip_fee_caps.rds')

parse.flexible.dates <- function(x, col.name){
    # Robustly parse a character date column that may mix ISO (YYYY-MM-DD)
    # and slash-delimited values, as has happened across hand edits of the
    # cap CSV. Slash-delimited values are resolved to day-first or
    # month-first column-wise: a convention is adopted only if it parses
    # every slash value, and the function stops (rather than coercing to
    # NA) on ambiguous or unrecognised values.
    out   <- rep(as.Date(NA), length(x))
    blank <- is.na(x) | x == ''
    iso   <- grepl('^\\d{4}-\\d{1,2}-\\d{1,2}$', x)
    iso.s <- grepl('^\\d{4}/\\d{1,2}/\\d{1,2}$', x)
    slash <- grepl('^\\d{1,2}/\\d{1,2}/\\d{4}$', x)
    bad   <- !blank & !iso & !iso.s & !slash
    if (any(bad)){
        stop(sprintf('%s: unrecognised date value(s): %s',
                     col.name, paste(unique(x[bad]), collapse = ', ')))
    }
    out[iso]   <- as.Date(x[iso],   format = '%Y-%m-%d')
    out[iso.s] <- as.Date(x[iso.s], format = '%Y/%m/%d')
    if (any(slash)){
        v <- x[slash]
        ok.dmy <- !any(is.na(as.Date(v, format = '%d/%m/%Y')))
        ok.mdy <- !any(is.na(as.Date(v, format = '%m/%d/%Y')))
        if (ok.dmy & !ok.mdy){
            out[slash] <- as.Date(v, format = '%d/%m/%Y')
        } else if (ok.mdy & !ok.dmy){
            out[slash] <- as.Date(v, format = '%m/%d/%Y')
        } else if (ok.dmy & ok.mdy){
            stop(sprintf(paste0('%s: slash-delimited dates are consistent ',
                                'with both DD/MM/YYYY and MM/DD/YYYY (%s); ',
                                'normalise the column to YYYY-MM-DD.'),
                         col.name, paste(unique(v), collapse = ', ')))
        } else {
            stop(sprintf(paste0('%s: slash-delimited date(s) invalid under ',
                                'both DD/MM and MM/DD conventions: %s'),
                         col.name, paste(unique(v), collapse = ', ')))
        }
    }
    miss <- !blank & is.na(out)
    if (any(miss)){
        stop(sprintf('%s: failed to parse date value(s): %s',
                     col.name, paste(unique(x[miss]), collapse = ', ')))
    }
    out
}

caps <- read.dat(inpath.caps)
geo  <- load.geo(inpath.geo)
pop  <- read.dat(inpath.pop)

# Drop annotation columns that are not used in the merge
for (v in c('source', 'source_alt', 'source_3', 'note', 'fee_response', 'chains_note')){
    caps[[v]] <- NULL
}

# Merge in ZCTA populations
pop$zcta <- fix.zip.codes(pop$zip)
pop$zip <- NULL
geo <- geo[which(geo$is.zcta), ]
geo <- left_join(geo, pop[, c('zcta', 'population')], by = 'zcta')
geo$population[which(is.na(geo$population))] <- 0

# Fill missing end dates with a far-future sentinel
caps$end_date[which(caps$end_date == '')] <- '2050-01-01'

# Parse the date columns (robust to the saving convention; stops on
# ambiguous or unrecognised values instead of producing NAs)
caps$start_date <- parse.flexible.dates(caps$start_date, 'start_date')
caps$end_date   <- parse.flexible.dates(caps$end_date,   'end_date')

# Treat missing chain-status as "chains are covered"
caps$excludes_chains[which(caps$excludes_chains == '')] <- 0
caps$excludes_chains[is.na(caps$excludes_chains)]       <- 0

# Restrict to caps that came into effect in 2020 or 2021
caps <- caps[which(format(caps$start_date, '%Y') %in% c('2020', '2021')), ]

# For each ZIP, build a weekly time series 2020-01-01..2021-06-30 of:
#   cap         — minimum applicable cap level at that date (Inf if none)
#   excl_chains — 1 if any applicable cap excludes chains, else 0
# Caps are layered city > state > county; the lowest cap wins, and if any
# applicable jurisdiction excludes chains the resulting period inherits that.
start.date <- as.Date('2020-01-01', '%Y-%m-%d')
end.date   <- as.Date('2021-06-30', '%Y-%m-%d')
ts.dates   <- seq(from = start.date, to = end.date, by = 7)
n.dates    <- length(ts.dates)

zip.cap.data <- list()
for (k in 1:nrow(geo)){
    print(k)
    zip.caps    <- rep(Inf, times = n.dates)
    excl.chains <- rep(1,   times = n.dates)
    zip         <- geo$zip[k]
    zip.county  <- geo$county_name[k]
    zip.state   <- geo$state[k]
    zip.city    <- geo$city[k]
    zip.pop     <- geo$population[k]

    city.level   <- (caps$city == zip.city)     & (caps$state == zip.state)
    county.level <- (caps$county == zip.county) & (caps$state == zip.state)
    state.level  <- (caps$state == zip.state)   & (caps$city == 'all cities') & (caps$county == '')

    if (any(city.level)){
        city.caps <- which(city.level)
        city.cap.level   <- rep(Inf, times = n.dates)
        city.excl.chains <- rep(1,   times = n.dates)
        for (city.cap in city.caps){
            cap.begin <- caps$start_date[city.cap]
            cap.end   <- caps$end_date[city.cap]
            idx <- which((ts.dates >= cap.begin) & (ts.dates <= cap.end))
            city.cap.level[idx]   <- caps$cap[city.cap]
            city.excl.chains[idx] <- caps$excludes_chains[city.cap]
        }
        zip.caps    <- city.cap.level
        excl.chains <- city.excl.chains
    }

    if (any(state.level)){
        state.caps <- which(state.level)
        state.cap.level   <- rep(Inf, times = n.dates)
        state.excl.chains <- rep(1,   times = n.dates)
        for (state.cap in state.caps){
            cap.begin <- caps$start_date[state.cap]
            cap.end   <- caps$end_date[state.cap]
            idx <- which((ts.dates >= cap.begin) & (ts.dates <= cap.end))
            state.cap.level[idx]   <- caps$cap[state.cap]
            state.excl.chains[idx] <- caps$excludes_chains[state.cap]
        }
        idx <- which(zip.caps > state.cap.level)
        zip.caps[idx] <- state.cap.level[idx]
        idx <- which(excl.chains > state.excl.chains)
        excl.chains[idx] <- state.excl.chains[idx]
    }

    if (any(county.level)){
        county.caps <- which(county.level)
        county.cap.level   <- rep(Inf, times = n.dates)
        county.excl.chains <- rep(1,   times = n.dates)
        for (county.cap in county.caps){
            cap.begin <- caps$start_date[county.cap]
            cap.end   <- caps$end_date[county.cap]
            idx <- which((ts.dates >= cap.begin) & (ts.dates <= cap.end))
            county.cap.level[idx]   <- caps$cap[county.cap]
            county.excl.chains[idx] <- caps$excludes_chains[county.cap]
        }
        idx <- which(zip.caps > county.cap.level)
        zip.caps[idx] <- county.cap.level[idx]
        idx <- which(excl.chains > county.excl.chains)
        excl.chains[idx] <- county.excl.chains[idx]
    }

    # excl_chains is only meaningful inside cap windows
    excl.chains[is.infinite(zip.caps)] <- 0

    zip.cap.data[[zip]] <- data.frame(zip = zip, date = ts.dates, pop = zip.pop,
                                      cap = zip.caps, excl_chains = excl.chains)
}

zip.df <- dplyr::bind_rows(zip.cap.data)

saveRDS(list(zip.df = zip.df, zip.cap.data = zip.cap.data), file = outpath.dat)
