# Compute price indices for estimation

library(EconTools)

# Specify paths
outdir <- 'output/numerator_menu_pricing/disagg_results'
inpath.reg <- paste0(outdir, '/pricing_reg_combo.csv')
inpath.dat <- 'data/numerator/all_baskets_static-combined.rds'
inpath.cap <- 'data/fee_caps/monthly_fee_caps.csv'
outpath      <- paste0(outdir, '/price_indices.csv')
outpath.pbar <- paste0(outdir, '/pbar_value.txt')

# Load regression results
reg <- read.dat(inpath.reg)
# Load data
dat <- readRDS(inpath.dat)
cap <- read.dat(inpath.cap)

## Merge datasets
cap$month <- sub('-01$', '', cap$month)
cap$POSTAL_CODE <- fix.zip.codes(cap$zip)
dat <- dplyr::left_join(dat, cap, by = c('POSTAL_CODE', 'month'))
dat$has_cap <- 1*(dat$cap < 0.3)

# Extract regression estimates. The regression has a single pooled online
# intercept; Postmates does not have its own coefficient and is treated as
# identical to DoorDash here (DoorDash acquired Postmates in 2020).
phi <- reg$estComm[which(reg$var == 'Online platform')]
phi <- as.numeric(rep(phi, times = 4)) # Copy to assign phi to each of the four platforms

beta  <- as.numeric(reg$estComm[which(reg$var == 'Commission rate')])
gamma <- as.numeric(reg$estComm[which(reg$var == 'Commission rate $\\times$ online')])

r <- sort(unique(cap$cap))

index.0 <- exp(beta*r)

NF <- length(phi)
index.1 <- sapply(1:NF, function(f) exp(phi[f] + (beta + gamma)*r))

idx.30 <- which(r == 0.30)
factor.dd <- index.1[idx.30, 1]

# Compute average basket subtotal for DoorDash
keep.months <- c('2021-04', '2021-05', '2021-06')
idx.dd <- which(dat$platform == 'dd' & dat$month %in% keep.months &
                    dat$static_trans & dat$has_cap == 0)
avg.basket <- mean(dat$BASKET_SUB_TOTAL[idx.dd], na.rm = TRUE)

p.bar <- avg.basket/factor.dd

# Adjust by the average price factor
index.1 <- index.1*p.bar
index.0 <- index.0*p.bar

# Combine into data.frame
df <- as.data.frame(index.1)
cnames <- c('dd', 'uber', 'gh', 'pm')
colnames(df) <- cnames
df$direct <- index.0
df$r      <- r
df <- df[, c('r', 'direct', cnames)]

writeLines(sprintf('%0.2f', p.bar), outpath.pbar)

# Save the results
write.dat(df, outpath)
