# YipitData - data exploration

## Data preparation
- `prepare_DiD_data.R`: prepare the YipitData ZIP/month consumer panel for the DiD regressions. Writes `data/yipitdata/consumer_panel_processed.csv`.

## Difference-in-differences estimates feeding consumer choice estimation
- `demand_estimation_DiD.R`: runs both DiD regressions (sales and fees) and writes the intermediate CSVs `output/explore_yipit/sales_DiD_for_demand_estimation.csv` and `output/explore_yipit/fee_DiD_for_demand_estimation.csv`. Replaces the prior Stata `.do` pair; outputs agree with the Stata results to ~7 significant digits (Stata's float-storage precision).
- `process_demand_estimation_DiD.R`: combines the two regression outputs into `DiD_for_demand_estimation.csv` (consumed by `code/demand_estimation/`) and the paper-formatted `DiD_for_demand_estimation-paper.csv` (**Table 1**).

## Other analyses
- `yipit_price_structure.R`: describe the price structure (consumer fees and commissions) by month for cap and no-cap ZCTAs. Produces **OA Figure D2a, D2b**.
- `restaurant_multihoming.R`: characterize the extent of restaurant multihoming. Produces **OA Table D1b**.
