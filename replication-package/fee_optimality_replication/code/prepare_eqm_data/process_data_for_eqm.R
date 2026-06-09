# Save data to be used in platform portfolio estimation and in CFs
library(Matrix)
library(EconTools)
library(FoodDeliveryTools)

# Specify options
data.opts <- list()
data.opts$nsim <- 50
data.opts$cap.month <- '2021-04-01'
data.opts$importance.sampling <- TRUE

outdir <- 'data/eqm_data'


nsim.suffix <- ifelse(data.opts$nsim == 10, '', sprintf('_nsim%s', data.opts$nsim))

outpath <- sprintf('%s/eqm_data%s.rds', outdir, nsim.suffix)

# Prepare the data
set.seed(1)
dat <- prepare.data(data.opts)

# Save the data
saveRDS(dat, file = outpath)

