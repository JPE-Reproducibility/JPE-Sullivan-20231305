# `code/numerator_menu_pricing`

Estimation of how restaurant prices vary with platform commissions, using the
Numerator item-level transactions data.

## Pipeline

Run the steps in the order below (R scripts via `Rscript`, Stata steps in an
interactive Stata session or via `stata-mp -b do <file>`):

1. `prepare_menu_data.R` — write `data/numerator/item_level_price_panel.rds`.
2. `prepare_geo_data.R` — write `data/numerator/{nlistings,commission_cap}_for_price_analysis.rds`.
3. `prepare_item_data.R` — write `data/numerator/{item_characteristics, item_prices_and_sales}.rds`.
4. `merge_item_level_data.R` — write `data/numerator/item_level_price_panel_merged.csv`.
5. (Stata) `convert_to_dta.do` — produce the `.dta`.
6. (Stata) `estimate_price_effects_disagg.do` — TWFE regressions writing
   `output/numerator_menu_pricing/disagg_results/TWFE_*.csv`. Uses the helper
   `save_vcov.ado` to export the e(V) matrix.
7. `generate_item_level_reg_table.R` — formatted regression tables, including
   `pricing_reg_combo_allsample.csv` (= **Appendix Table A1**).
8. `compute_price_indices.R` — write `output/numerator_menu_pricing/disagg_results/price_indices.csv`.
9. `describe_prices.R` — scatter plots in `output/numerator_menu_pricing/descr/scatter_prices/`
   that constitute **OA Figure D1a–d**.

## Stata requirements

The two `.do` files in this directory need:

- **Stata 18** or later (pinned by `save_vcov.ado:5: version 18`).
- The community-contributed packages `reghdfe` and `estout` (for `esttab`).
  Install with `ssc install reghdfe` and `ssc install estout` from within Stata.
- Stata must be launched from the **repository root** so that the relative
  `adopath + "code/numerator_menu_pricing"` in `estimate_price_effects_disagg.do`
  resolves correctly (this is what makes `save_vcov` visible).

## Downstream consumers of `price_indices.csv`

`output/numerator_menu_pricing/disagg_results/price_indices.csv` feeds:

- `prepare_est_data/produce_month_zip_level.R` (the `did` branch) → MATLAB
  consumer-choice GMM estimator → **Table 2**, **OA Tables K2 / K3**.
- `FoodDeliveryTools/R/prepare_data.R` (the `est.year >= 2025` branch) →
  `prepare_eqm_data/process_data_for_eqm.R` → `data/eqm_data/eqm_data*.rds` →
  CF data layer → **Tables 3a–15**, **OA Tables H1, J1a/b, K1, K4, K5**,
  **OA Figures B1, K1, K2**, **Figures 3–8**.

### Platform columns in `price_indices.csv`

The output has columns `r`, `direct`, `dd`, `uber`, `gh`, `pm`. The pricing
regression in `estimate_price_effects_disagg.do` estimates a *single pooled*
online intercept (no platform-specific interactions); `compute_price_indices.R`
copies that single coefficient into all four platform columns. So `dd`, `uber`,
`gh`, and `pm` are identical at every `r`. The Postmates column is included
because downstream consumers (`FoodDeliveryTools/R/prepare_data.R`, which
expects a 5-vector indexed by `c('direct', 'dd', 'uber', 'gh', 'pm')`) need a
Postmates entry; this implicitly treats Postmates as identical to DoorDash,
which is reasonable given that DoorDash acquired Postmates in 2020.
