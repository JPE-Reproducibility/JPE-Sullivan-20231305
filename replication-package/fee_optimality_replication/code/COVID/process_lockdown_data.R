library(EconTools)
inpath <- 'data/USA-covid-policy-master/data/OxCGRT_US_latest.csv'
outpath <- 'data/USA-covid-policy-master/processed.csv'

dat <- read.dat(inpath)

vars <- c('RegionName', 'RegionCode', 'Jurisdiction', 'Date',
          'StringencyIndex', 'GovernmentResponseIndex', 'ContainmentHealthIndex',
          'EconomicSupportIndex')
dat <- dat[, vars]

dat$month <- sub('[0-9]{2}$', '', dat$Date)

dat <- doBy::summaryBy(StringencyIndex + GovernmentResponseIndex + ContainmentHealthIndex + EconomicSupportIndex ~ RegionCode + month,
                       id = c('RegionName', 'Jurisdiction'), data = dat, FUN = mean, keep.names = TRUE)
dat <- dat[which(dat$RegionCode != ''), ]

dat$state <- sub('US_', '', dat$RegionCode)
dat$Jurisdiction <- NULL
dat <- dat[grep('^202[01]', dat$month), ]
dat$month <- sub('^2020', '2020-', dat$month)
dat$month <- sub('^2021', '2021-', dat$month)

write.dat(dat, outpath)
