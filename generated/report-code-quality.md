## Code Quality

### Python

[ADVISORY] `.query(` or `.loc[` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (identify_static_trans-summary.py, line 32)
  → buy = buy.loc[~pd.isna(buy['TRANSACTION_DATE'])]

[ADVISORY] `.query(` or `.loc[` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (identify_static_trans-summary.py, line 34)
  → buy = buy.loc[idx]

[ADVISORY] `.query(` or `.loc[` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (identify_static_trans-summary.py, line 45)
  → buy.loc[buy['_merge'] == 'both', 'in_static'] = 1

[ADVISORY] `.query(` or `.loc[` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (identify_static_trans-summary.py, line 48)
  → buy_static = deepcopy(buy.loc[buy['in_static'] == 1])

[ADVISORY] `.query(` or `.loc[` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (identify_static_trans-summary.py, line 49)
  → buy_not    = deepcopy(buy.loc[buy['in_static'] == 0])

[ADVISORY] `.query(` or `.loc[` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (identify_static_trans.py, line 51)
  → buy.loc[buy['_merge'] == 'both', 'in_static'] = 1

[ADVISORY] `.query(` or `.loc[` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (identify_static_trans.py, line 54)
  → buy_static = deepcopy(buy.loc[buy['in_static'] == 1])

[ADVISORY] `.query(` or `.loc[` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (identify_static_trans.py, line 55)
  → buy_not    = deepcopy(buy.loc[buy['in_static'] == 0])

### R

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (generate_synthetic_data.R, line 82)
  → uszips <- merge(uszips, state.name.lookup,

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (generate_synthetic_data.R, line 118)
  → counties <- merge(counties, corebased_areas[, .(CBSA, CBSA_name)],

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (generate_synthetic_data.R, line 127)
  → zip.to.cbsa <- merge(nz[, .(zip, fips)],

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (generate_synthetic_data.R, line 199)
  → panelists.A.cbsa <- merge(panelists.A,

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (generate_synthetic_data.R, line 534)
  → treated.spans <- merge(zip.city, caps[, .(city, state, start_date, end_date)],

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (generate_synthetic_data.R, line 626)
  → people <- merge(people, nz[, .(zip, STATE = state)],

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (generate_synthetic_data.R, line 758)
  → baskets <- merge(baskets, anchor.tbl[, .(USER_ID, platform.target, month.target)],

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (generate_synthetic_data.R, line 772)
  → baskets <- merge(baskets, banner.tbl, by.x = 'BANNER_ID',

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (zip_map.R, line 28)
  → geo <- merge(x = zip_codes, y = counties, by = 'fips',

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (evaluate_numerator.R, line 29)
  → nmr.cbsa <- merge(nmr.cbsa, cbsa.codes, by = 'CBSA_name')

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (generate_geo.R, line 71)
  → counties <- merge(counties, corebased_areas, by = 'CBSA',

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (generate_geo.R, line 73)
  → zip_codes <- merge(x = zip_codes, y = counties[, keep.vars], by = 'fips',

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (generate_geo.R, line 142)
  → mkts <- merge(x = mkts, y = alt.zip[, merge.in.vars], by = 'zip')

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (generate_geo.R, line 143)
  → mkts <- merge(x = mkts, y = acs.zip, by = 'zip')

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (generate_geo.R, line 178)
  → mkts.alt <- merge(x = mkts, y = munis[, c('municipality', 'include')],

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (generate_geo.R, line 206)
  → mkts.alt <- merge(x = mkts.alt, y = alt.zip[, c('zip', 'lat', 'lon')], by = 'zip')

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (prepare_regression_data.R, line 109)
  → buy <- merge(buy, geo.sub, by = 'zip')

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (static_event_dataset.R, line 36)
  → period.df <- merge(period.df, period.level, by = 'period')

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (static_event_dataset.R, line 40)
  → buy <- merge(x = buy, y = period.df, by = 'date',

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (data_processing.R, line 374)
  → df <- merge(x = df, y = state.mapping, by = 'state.abbrev',

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (data_processing.R, line 376)
  → df <- merge(x = df, y = state.tz, by = 'state',

### Stata

[ADVISORY] Sample drop (`drop if` / `keep if`) not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (estimate_price_effects_disagg.do, line 53)
  → keep if brand_na == 0

[ADVISORY] Sample drop (`drop if` / `keep if`) not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (estimate_price_effects_disagg.do, line 54)
  → keep if pbanner == 0

[ADVISORY] Sample drop (`drop if` / `keep if`) not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (estimate_price_effects_disagg.do, line 55)
  → keep if coefoff != .

[ADVISORY] Sample drop (`drop if` / `keep if`) not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (estimate_price_effects_disagg.do, line 56)
  → keep if coefon != .

[ADVISORY] Sample drop (`drop if` / `keep if`) not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (estimate_price_effects_disagg.do, line 65)
  → keep if n_banner >= 10000

