
# Determine the top cuisines and chains in the YipitData dataset


library(data.table)
library(EconTools)
library(FoodDeliveryTools)

# Settings
nchains   <- 50
ncuisines <- 100

# Paths
inpath.YD <- 'data/yipitdata/listings_v2.csv'
outdir   <- 'output/process_yipit'
create.dir(outdir)
outpath <- sprintf('%s/top_chains_and_cuisines.rds', outdir)

# Load data
dropvars <- c('open_hours', 'open_at_observation_time',
              'offers_pickup', 'phone_number',
              'estimated_min_order_value_cents', 'metro_tier',
              'restaurant_tags', 'estimated_delivery_fee_cents',
              'address_country', 'reference_observation_in_month',
              'fulfills_own_deliveries')
YD <- fread(inpath.YD, drop = dropvars)
YD <- YD[platform %in% c('Grubhub', 'Postmates', 'UberEats', 'DoorDash')]

# Limit to a month
YD <- YD[grep('^2021-04', YD$observation_month), ]

chains <- YD$chain
cuisines <- YD$cuisine

# Determine top chains
chains <- chains[which(chains != '')]
top.chains <- s.table(chains, K = nchains)

# Determine top cuisines
cuisines <- cuisines[which(cuisines != '')]


cuisines.split <- lapply(cuisines, strsplit, split = ', ')
cuisines.split <- lapply(cuisines.split, function(x) x[[1]])
cuisines.all <- unlist(cuisines.split)

# Process the categories and drop unwanted ones
cuisines.all <- process.cuisines(cuisines.all)
drop.cats <- c('pickup', 'takeout', 'grocery', 'convenience', 'catering',
               'personal care', 'medicine', 'exclusive to eats',
               'home & personal care', 'everyday essentials',
               'grocery items', 'black-owned', 'black owned')
cuisines.all <- cuisines.all[which(!(cuisines.all %in% drop.cats))]

top.cuisines <- s.table(cuisines.all, ncuisines)

output <- list(top.cuisines = top.cuisines,
               top.chains = top.chains)
saveRDS(output, file = outpath)
