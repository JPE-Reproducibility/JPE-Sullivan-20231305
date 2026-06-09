## Describe the estimation sample

library(EconTools)
library(FoodDeliveryTools)

library(doBy)
library(dplyr)

inpath.dat <- 'data/est_dat/connect_sample_v3_hedonic_sset_0s.csv'
inpath.J <- 'data/est_dat/zcta_code_counts-yipitdata_w_infogroup2021_partnered_apr2021.csv'
inpath.geo <- 'data/geo/geo_with_zctas.csv'
inpath.code <- 'data/est_dat/cbsa_ids-yipitdata.csv'

outpath.tab <- 'output/demand_estimation/sample_size.csv'
outpath.T   <- 'output/demand_estimation/T_i_quantiles.csv'

dat <- read.dat(inpath.dat)
resto <- read.dat(inpath.J)
geo <- read.dat(inpath.geo)
codes <- read.dat(inpath.code, sep = '|')


dat$id <- dat$PANEL_ID

dat$ntrans <- rowSums(dat[, grepl('^f[0-9]+$', colnames(dat))])
ntrans <- summaryBy(ntrans ~ m, data = dat, FUN = sum)
# Get number of panelists
npanelists <- summaryBy(USER_ID ~ m, data = dat, FUN = function(x) length(unique(x)))
colnames(ntrans) <- c('m', 'ntrans')
colnames(npanelists) <- c('m', 'npanelists')
ntrans <- dplyr::left_join(ntrans, npanelists, by = 'm')

resto <- left_join(resto, geo, by = 'zip')

nresto <- summaryBy(J_total + G0000 ~ CBSA_name,
                    FUN = sum, data = resto)
nresto <- inner_join(nresto, codes, by = 'CBSA_name')

s.size <- left_join(nresto, ntrans, by = 'm')
colnames(s.size) <- c('market', 'nres', 'noff', 'm', 'ntrans', 'npanelists')
s.size <- s.size[, c('market', 'npanelists', 'ntrans', 'nres', 'noff')]
s.size$market <- sub('-.*$', '', s.size$market)

write.dat(s.size, outpath.tab)

## Distribution of T_i
T.i <- dat$T_i

qtiles <- c(0.5, 0.9, 0.95, 0.96, 0.97, 0.98, 0.99, 0.999)
T.qtiles <- quantile(T.i, qtiles)
T.qtiles <- as.data.frame(T.qtiles)
write.table(T.qtiles, file = outpath.T)

