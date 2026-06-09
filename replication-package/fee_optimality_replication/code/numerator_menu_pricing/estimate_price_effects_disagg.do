* Estimate impacts of commission caps on prices

* Add path to the save_vcov helper. Stata must be launched from the repository
* root for this relative adopath to resolve.
version 19

adopath + "code/numerator_menu_pricing"

* Load data
use "data/numerator/item_level_price_panel_merged.dta", clear

* Specify paths to outputs
local outdir "output/numerator_menu_pricing/disagg_results"
capture mkdir "`outdir'"

local outpath_disc       "`outdir'/TWFE_discr.csv"
local outpath_disc_vcov  "`outdir'/TWFE_discr_vcov.csv"
local outpath_conti      "`outdir'/TWFE_conti.csv"
local outpath_conti_vcov "`outdir'/TWFE_conti_vcov.csv"

local outpath_disc_allsample       "`outdir'/TWFE_discr_allsample.csv"
local outpath_disc_vcov_allsample  "`outdir'/TWFE_discr_vcov_allsample.csv"
local outpath_conti_allsample      "`outdir'/TWFE_conti_allsample.csv"
local outpath_conti_vcov_allsample "`outdir'/TWFE_conti_vcov_allsample.csv"

sort month
encode month, gen(month_id)
encode item, gen(item_code)

* Treat caps that exclude chains as no-cap in the price-effects regression
gen has_excl_cap = 0
replace has_excl_cap = 1 if has_cap == 1 & excl_chains == 1
replace has_cap = 0 if has_excl_cap == 1

* Drop observations with pbanner, NA brand
gen pbanner = 0
replace pbanner = 1 if banner_id == "doordash"
replace pbanner = 1 if banner_id == "ubereats"
replace pbanner = 1 if banner_id == "grubhubcom"

gen brand_na = 0
replace brand_na = 1 if brand == "unknown"
replace brand_na = 1 if brand == "N/A"

gen log_price = log(item_unit_price)

gen online = 0
replace online = 1 if platform != "na"
gen online_cap = online*cap

encode platform, gen(p_id)

keep if brand_na == 0
keep if pbanner == 0
keep if coefoff != .
keep if coefon != .

* Banner counts
tempfile by_banner
preserve 
collapse (count) n_banner = basket_id, by(banner_id)
save `by_banner'
restore
merge m:1 banner_id using `by_banner'
keep if n_banner >= 10000
encode banner_id, gen(BID)


* Merge in the number of baskets in which each item appears in its ZIP

reghdfe log_price online has_cap##online if  month_id > 6 & item_unit_price >= 1 & banner_id != "mcdonalds" & banner_id != "taco_bell" & banner_id != "chickfila" & banner_id != "wendys" & banner_id != "burger_king" & banner_id != "popeyes" & coefon <= 1 & coefoff <= 1 & n_on >= 100 & n_off >= 100, absorb(zip3 item_code month_id)
esttab using "`outpath_disc'", se replace
save_vcov using "`outpath_disc_vcov'", replace

reghdfe log_price online cap online_cap if month_id > 6 & item_unit_price >= 1 & banner_id != "mcdonalds" & banner_id != "taco_bell" & banner_id != "chickfila" & banner_id != "wendys" & banner_id != "burger_king" & banner_id != "popeyes" & coefon <= 1 & coefoff <= 1 & n_on >= 100 & n_off >= 100, absorb(zip3 item_code month_id)
esttab using "`outpath_conti'", se replace
save_vcov using "`outpath_conti_vcov'", replace


* With the entire sample
reghdfe log_price online has_cap##online if item_unit_price >= 1 & banner_id != "mcdonalds" & banner_id != "taco_bell" & banner_id != "chickfila" & banner_id != "wendys" & banner_id != "burger_king" & banner_id != "popeyes" & coefon <= 1 & coefoff <= 1 & n_on >= 100 & n_off >= 100, absorb(zip3 item_code month_id)
esttab using "`outpath_disc_allsample'", se replace
save_vcov using "`outpath_disc_vcov_allsample'", replace

reghdfe log_price online cap online_cap if item_unit_price >= 1 & banner_id != "mcdonalds" & banner_id != "taco_bell" & banner_id != "chickfila" & banner_id != "wendys" & banner_id != "burger_king" & banner_id != "popeyes" & coefon <= 1 & coefoff <= 1 & n_on >= 100 & n_off >= 100, absorb(zip3 item_code month_id)
esttab using "`outpath_conti_allsample'", se replace
save_vcov using "`outpath_conti_vcov_allsample'", replace

