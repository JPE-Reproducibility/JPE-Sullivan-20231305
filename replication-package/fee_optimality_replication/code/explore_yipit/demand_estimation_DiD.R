# DiD regressions on the YipitData ZIP/month consumer panel feeding the
# consumer-choice demand estimation. Replaces the prior Stata pair
# (demand_estimation_DiD_sales.do / demand_estimation_DiD_fees.do); outputs
# match the Stata results to ~7 significant digits and are written in the
# same format expected by process_demand_estimation_DiD.R.
#
# Input:
#   data/yipitdata/consumer_panel_processed.csv  (from prepare_DiD_data.R)
#
# Outputs:
#   output/explore_yipit/sales_DiD_for_demand_estimation.csv
#   output/explore_yipit/fee_DiD_for_demand_estimation.csv

library(data.table)
library(fixest)

inpath        <- 'data/yipitdata/consumer_panel_processed.csv'
outdir        <- 'output/explore_yipit'
outpath.sales <- file.path(outdir, 'sales_DiD_for_demand_estimation.csv')
outpath.fees  <- file.path(outdir, 'fee_DiD_for_demand_estimation.csv')

if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# ---- Helpers -----------------------------------------------------------

# Replicate Stata's `destring <v>, replace force ignore(",")`:
# strip commas, coerce to numeric, non-numeric -> NA.
destring_force <- function(x){
    if (is.numeric(x)) return(x)
    suppressWarnings(as.numeric(gsub(',', '', x, fixed = TRUE)))
}

# Stata %9.0g-like number formatting: round to single-precision (Stata's
# default float storage gives ~7 sig digits), then display with up to 7 sig
# digits, no leading zero on |x|<1, "." for NA, "0" for exact zero.
fmt_stata <- function(x){
    if (is.na(x)) return('.')
    if (x == 0)  return('0')
    x <- as.numeric(as.single(x))
    s <- formatC(x, digits = 7, format = 'g')
    s <- sub('^(-?)0\\.', '\\1.', s)
    s
}

# ---- Load + global cleaning -------------------------------------------

# Read with na.strings = "" so the literal string "NA" stays "NA" (Stata semantics).
df <- fread(inpath, sep = '|', na.strings = '')
setnames(df, tolower(names(df)))

df <- df[state != 'PR' & cbsa_name != 'NA']

demos       <- c('share_young', 'share_married', 'share_highinc')
nearby_vars <- c('n_nearby_doordash', 'n_nearby_grubhub', 'n_nearby_ubereats',
                 'n_nearby_postmates', 'n_nearby_nplatforms', 'n_nearby_online')
covar_plus  <- c('new_cases_pc', 'stringency', 'covid_dem',
                 paste0('log_', nearby_vars))

# Clean demos (both .do files do this before anything else); drop missings.
for (v in demos) df[, (v) := destring_force(get(v))]
df <- df[!is.na(share_young) & !is.na(share_married) & !is.na(share_highinc)]


# =======================================================================
# SALES regression  (mirrors demand_estimation_DiD_sales.do)
# =======================================================================

# Collapse to (month, zip): sum orders_scaled, first() of everything else.
# Stata's collapse processes within each group in original row order; data.table
# preserves the same order under by=.
sales_cols <- c('commission', 'population', 'new_cases_pc', 'stringencyindex',
                'democrat_share', nearby_vars, demos)

df_sales <- df[, c(list(orders_scaled = sum(orders_scaled)),
                   lapply(.SD, function(x) x[1])),
               by = .(month, zip), .SDcols = sales_cols]

# Clean nearby controls (post-collapse, per the .do file) and drop missings.
for (v in nearby_vars) df_sales[, (v) := destring_force(get(v))]
for (v in nearby_vars) df_sales <- df_sales[!is.na(get(v))]

# Replace zeros with 1 in nearby controls, then log.
for (v in nearby_vars){
    df_sales[get(v) == 0, (v) := 1]
    df_sales[, paste0('log_', v) := log(get(v))]
}

# encode month -> integer (Stata sorts alphabetically; YYYY-MM-01 -> chronological).
df_sales[, t := as.integer(factor(month, levels = sort(unique(month))))]
df_sales[, log_sales  := log(orders_scaled)]
setnames(df_sales, 'stringencyindex', 'stringency')
df_sales[, covid_dem := democrat_share * new_cases_pc]
df_sales[, W         := population]

# xtreg log_sales <covar_plus> i.t commission [aweight=W], fe   <=>
# feols(log_sales ~ <covar_plus> + factor(t) + commission | zip,
#       weights = ~W, vcov = 'iid')
# Default fixest small-sample DOF matches xtreg (subtracts FE).
fml.sales <- as.formula(paste(
    'log_sales ~', paste(covar_plus, collapse = ' + '),
    '+ factor(t) + commission | zip'
))
m.sales <- feols(fml.sales, data = df_sales, weights = ~W, vcov = 'iid')

# Compute Stata-style intercept: mean(y) - sum(b_j * mean(x_j)), using weights.
# (xtreg reports _cons; downstream doesn't use it but we include it for parity.)
# Restrict to the same rows feols used (drop log_sales = -Inf from log(0)).
df_sales_est <- df_sales[is.finite(log_sales)]
wmean <- function(x) weighted.mean(x, df_sales_est$W)
sales_means <- list(
    log_sales    = wmean(df_sales_est$log_sales),
    new_cases_pc = wmean(df_sales_est$new_cases_pc),
    stringency   = wmean(df_sales_est$stringency),
    covid_dem    = wmean(df_sales_est$covid_dem),
    commission   = wmean(df_sales_est$commission)
)
for (v in nearby_vars){
    sales_means[[paste0('log_', v)]] <- wmean(df_sales_est[[paste0('log_', v)]])
}
coefs_s <- coef(m.sales)
t_levels <- sort(unique(df_sales$t))
t_share  <- sapply(t_levels[-1], function(k) weighted.mean(df_sales_est$t == k, df_sales_est$W))
names(t_share) <- paste0('factor(t)', t_levels[-1])

cons_sales <- sales_means$log_sales
for (v in covar_plus) cons_sales <- cons_sales - coefs_s[v] * sales_means[[v]]
cons_sales <- cons_sales - coefs_s['commission'] * sales_means$commission
for (nm in names(t_share))   cons_sales <- cons_sales - coefs_s[nm] * t_share[nm]

# =======================================================================
# FEES regression  (mirrors demand_estimation_DiD_fees.do)
# =======================================================================

df_fees <- df  # no collapse for fees

# Clean nearby controls; drop missings.
for (v in nearby_vars) df_fees[, (v) := destring_force(get(v))]
for (v in nearby_vars) df_fees <- df_fees[!is.na(get(v))]

df_fees <- df_fees[merchant_name %in% c('DoorDash', 'Uber', 'Grub Hub')]

# Replace zeros with 1, then log
for (v in nearby_vars){
    df_fees[get(v) == 0, (v) := 1]
    df_fees[, paste0('log_', v) := log(get(v))]
}

# encode month -> t (1..17); encode merchant_name -> f (alphabetical: DoorDash=1, Grub Hub=2, Uber=3)
df_fees[, t := as.integer(factor(month,         levels = sort(unique(month))))]
df_fees[, f := as.integer(factor(merchant_name, levels = sort(unique(merchant_name))))]

df_fees[, fee_plus_price     := total_fee2 + aov_feesandtips_excluded]
df_fees[, log_fee_plus_price := log(fee_plus_price)]
# Note: the .do file's `capture rename stringencyindex stringency` silently fails
# here because the source data carries BOTH columns -- a pre-existing `stringency`
# (raw 0-100 OxCGRT index) and `stringencyindex` (the same index / 100). The fees
# regression therefore uses the raw `stringency` column. Do not rename in R.
df_fees[, covid_dem := democrat_share * new_cases_pc]
df_fees[, W         := population]
df_fees[, zip_f     := paste(zip, f, sep = '_')]

# reghdfe log_fee_plus_price <covar_plus> i.t##i.f commission [aweight=W], absorb(zip#f)
# i.t##i.f expands to i.t + i.f + i.t#i.f.  i.f is fully absorbed by zip#f.
fml.fees <- as.formula(paste(
    'log_fee_plus_price ~', paste(covar_plus, collapse = ' + '),
    '+ factor(t) * factor(f) + commission | zip_f'
))
m.fees <- feols(fml.fees, data = df_fees, weights = ~W, vcov = 'iid')

# Stata-style intercept for fees (restrict to rows feols uses)
df_fees_est <- df_fees[is.finite(log_fee_plus_price)]
wmeanf <- function(x) weighted.mean(x, df_fees_est$W)
fees_means <- list(
    log_fee_plus_price = wmeanf(df_fees_est$log_fee_plus_price),
    new_cases_pc       = wmeanf(df_fees_est$new_cases_pc),
    stringency         = wmeanf(df_fees_est$stringency),
    covid_dem          = wmeanf(df_fees_est$covid_dem),
    commission         = wmeanf(df_fees_est$commission)
)
for (v in nearby_vars){
    fees_means[[paste0('log_', v)]] <- wmeanf(df_fees_est[[paste0('log_', v)]])
}
coefs_f <- coef(m.fees)
cons_fees <- fees_means$log_fee_plus_price
for (v in covar_plus) cons_fees <- cons_fees - coefs_f[v] * fees_means[[v]]
cons_fees <- cons_fees - coefs_f['commission'] * fees_means$commission
t_lvls <- sort(unique(df_fees$t))
f_lvls <- sort(unique(df_fees$f))
for (k in t_lvls[-1]){
    nm <- paste0('factor(t)', k)
    if (nm %in% names(coefs_f))
        cons_fees <- cons_fees - coefs_f[nm] *
            weighted.mean(df_fees_est$t == k, df_fees_est$W)
}
for (k in t_lvls[-1]) for (j in f_lvls[-1]){
    nm <- paste0('factor(t)', k, ':factor(f)', j)
    if (nm %in% names(coefs_f))
        cons_fees <- cons_fees - coefs_f[nm] *
            weighted.mean(df_fees_est$t == k & df_fees_est$f == j, df_fees_est$W)
}

# =======================================================================
# Write CSVs in estout (b, se) format
# =======================================================================

# Build a tidy data.frame of (name, b, se) in the same row order as the .do
# files' estout output, then format numbers Stata-style.

estout_row <- function(name, b, se){
    data.frame(name = name, b = fmt_stata(b), se = fmt_stata(se),
               stringsAsFactors = FALSE)
}

# ---- Sales: covar_plus -> i.t (1.t baseline) -> commission -> _cons ----
co_s <- coef(m.sales); se_s <- sqrt(diag(vcov(m.sales)))

rows_s <- do.call(rbind, c(
    lapply(covar_plus, function(v) estout_row(v, co_s[v], se_s[v])),
    list(estout_row('1.t', 0, NA)),
    lapply(t_levels[-1], function(k){
        nm <- paste0('factor(t)', k)
        estout_row(paste0(k, '.t'), co_s[nm], se_s[nm])
    }),
    list(estout_row('commission', co_s['commission'], se_s['commission'])),
    list(estout_row('_cons', cons_sales, NA))   # SE for _cons not reproduced
))

writeLines(c(
    ',.,',
    ',b,se',
    paste(rows_s$name, rows_s$b, rows_s$se, sep = ',')
), outpath.sales)

# ---- Fees: covar_plus -> i.t -> i.f -> i.t#i.f -> commission -> _cons --
co_f <- coef(m.fees); se_f <- sqrt(diag(vcov(m.fees)))

rows_f <- list()
for (v in covar_plus) rows_f[[length(rows_f)+1]] <- estout_row(v, co_f[v], se_f[v])
rows_f[[length(rows_f)+1]] <- estout_row('1.t', 0, NA)
for (k in t_lvls[-1]){
    nm <- paste0('factor(t)', k)
    rows_f[[length(rows_f)+1]] <- estout_row(paste0(k, '.t'), co_f[nm], se_f[nm])
}
# i.f main effects: all absorbed -> 0 with "." SE
for (j in f_lvls) rows_f[[length(rows_f)+1]] <- estout_row(paste0(j, '.f'), 0, NA)
# i.t#i.f interactions: enumerate t=1..17, f=1..3 (Stata estout outputs all)
for (k in t_lvls) for (j in f_lvls){
    nm <- paste0('factor(t)', k, ':factor(f)', j)
    if (k == 1 || j == 1 || !(nm %in% names(co_f))){
        rows_f[[length(rows_f)+1]] <- estout_row(paste0(k, '.t#', j, '.f'), 0, NA)
    } else {
        rows_f[[length(rows_f)+1]] <- estout_row(paste0(k, '.t#', j, '.f'),
                                                 co_f[nm], se_f[nm])
    }
}
rows_f[[length(rows_f)+1]] <- estout_row('commission', co_f['commission'], se_f['commission'])
rows_f[[length(rows_f)+1]] <- estout_row('_cons', cons_fees, NA)
rows_f <- do.call(rbind, rows_f)

writeLines(c(
    ',.,',
    ',b,se',
    paste(rows_f$name, rows_f$b, rows_f$se, sep = ',')
), outpath.fees)

cat('Wrote', outpath.sales, '\n')
cat('Wrote', outpath.fees,  '\n')
