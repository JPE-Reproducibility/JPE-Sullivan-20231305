
* Convert the item-level panel used to analyze menu prices from csv to dta

version 19

insheet using "data/numerator/item_level_price_panel_merged.csv", clear

* Save on memory
drop major_category_description
drop sector_description
drop sector_id
drop department_description
drop mean_na
drop sd_na
drop n_na

* Convert to numeric
destring mean_dd mean_gh mean_uber, replace force
destring sd_dd sd_gh sd_uber, replace force
destring mean_on mean_off sd_on sd_off, replace force
destring totn coefoff coefon, replace force
destring n_dd n_gh n_uber n_on n_off, replace force 
destring basket_sub_total, replace force

* Convert coefficients of variation for platforms
gen coefdd   = sd_dd/mean_dd
gen coefuber = sd_uber/mean_uber
gen coefgh   = sd_gh/mean_gh

save "data/numerator/item_level_price_panel_merged.dta", replace
