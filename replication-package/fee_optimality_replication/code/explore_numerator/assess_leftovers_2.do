* Assess how ordering today affects the likelihood of future orders
* (writes per-regression CSVs consumed by assess_leftovers_3.R)
version 19

insheet using "data/numerator/daily_panel.csv", clear
save "data/numerator/daily_panel.dta", replace

local off_seven "output/explore_numerator/leftover_off_7.csv"
local on_seven  "output/explore_numerator/leftover_on_7.csv"
local off_three "output/explore_numerator/leftover_off_3.csv"
local on_three  "output/explore_numerator/leftover_on_3.csv"


use "data/numerator/daily_panel.dta", clear


encode date, gen(date_id)
encode month, gen(month_id)

* Offline, 7 days
reghdfe offline_next7 offline_order online_order, absorb(user_id##month_id date_id)
esttab using "`off_seven'", se nostar noparentheses plain cells((b se)) replace
	
* Online, 7 days
reghdfe online_next7  offline_order online_order, absorb(user_id##month_id date_id)
esttab using "`on_seven'", se nostar noparentheses plain cells((b se)) replace

* Offline, 3 days
reghdfe offline_next3 offline_order online_order, absorb(user_id##month_id date_id)
esttab using "`off_three'", se nostar noparentheses plain cells((b se)) replace
* Online, 3 days
reghdfe online_next3  offline_order online_order, absorb(user_id##month_id date_id)
esttab using "`on_three'", se nostar noparentheses plain cells((b se)) replace


preserve
gen x = 1
collapse (mean) offline_next7 online_next7  offline_next3 online_next3, by(x)
drop x
outsheet using "output/explore_numerator/mean_depvar.csv", replace
restore
