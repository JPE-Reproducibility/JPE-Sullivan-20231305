# Processing Numerator data

The Numerator data are subject to a confidentiality agreement and are not
included in the replication archive (with the permission of the editor).
Researchers with their own access to the Numerator panel can use the scripts
in this directory to produce the panel-level files consumed by the rest of
the codebase.

## Required input files (must be supplied by the replicator)

The format-specific preprocessing scripts that unzipped and concatenated the
historical deliveries used in the paper have been removed from this
replication directory, since the layout of any future Numerator delivery is
unlikely to match the one used here. The scripts below assume the following already-combined inputs
are present:

| File | Description |
|---|---|
| `data/numerator/standard_nmr_feed_people_table.csv` | Panelist-level demographic table (pipe-separated) |
| `data/numerator/combined_static_table.csv` | Static-panelist date-range table |
| `data/numerator/static_connect_table.csv` | Per-panelist e-mail connection start/end dates |
| `data/numerator/qsr_transactions_master.csv` | Item-level QSR transactions (concatenated across deliveries) |
| `data/numerator/summary_data-combined.csv` | Basket-level summary transactions (concatenated across deliveries) |
| `data/numerator/combined_item_table.rds` | Item-level metadata (concatenated across deliveries) |

## Run order

1. `collapse_static.R` — collapse the static table to one row per panelist.
   Reads `combined_static_table.csv`.
   Writes `static_users-combined.{rds,csv}`.
2. `identify_static_trans.py` — flag which item-level transactions occur
   while a panelist is in the static panel.
   Reads `qsr_transactions_master.csv` and `static_users-combined.csv`.
   Writes `qsr_baskets_{static,nonstatic}-combined.csv`.
3. `identify_static_trans-summary.py` — same flagging for the summary data.
   Reads `summary_data-combined.csv` and `static_users-combined.csv`.
   Writes `summary_baskets_{static,nonstatic}-combined.csv`.
4. `combine_fact_summary.R` — merge the item-level (fact) and summary
   transactions, preferring fact entries on basket-ID collisions.
   Writes `all_baskets_{static,nonstatic}-combined.csv`.
5. `add_category.R` — add platform and category labels to the basket-level
   datasets.
   Writes `all_baskets_{static,nonstatic}-combined.rds`.
6. `produce_nonstatic_table.R` — produce an analogue of the static table for
   panelists outside the core panel.
   Writes `nonstatic_users-combined.{csv,rds}`.
7. `static_event_dataset.R` — produce the monthly panel used by downstream
   event-study and DiD code.
   Writes `static_panel_monthly-combined.rds` and `months_df-combined.rds`.
8. `prepare_regression_data.R` — build the regression-ready dataset.
   Writes `regression_data-{april,may,june}_v3.rds`.
