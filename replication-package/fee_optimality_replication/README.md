# README for "Fee Optimality in a Two-Sided Market" (Michael Sullivan)

## Overview

This replication package contains the code, data, and outputs needed to
reproduce the results in *Fee Optimality in a Two-Sided Market* by Michael
Sullivan (*Journal of Political Economy*). 

The pipeline is organized into five stages:

1. **Data preparation.** Process raw consumer-panel, restaurant-listings,
   geographic, demographic, COVID, and fee-cap data into the analysis
   datasets stored under `data/est_dat/`, `data/eqm_data/`, and various
   processed subdirectories.
2. **Descriptive analysis.** Produce Figure 1 (consumer market shares),
   Figure 2 (restaurant platform-subset distribution), Table 1
   (difference-in-differences estimates), and the supporting
   Online Appendix exhibits on multi-homing patterns and price structure
   (OA Section D), Numerator-panel validation (OA Section E),
   web-harvested platform-data descriptives (OA Section F), and the
   restaurant-price sample (OA Section G).
3. **Model estimation.** GMM estimation of consumer demand
   (`code/demand_estimation/`), estimation of restaurant marginal costs
   (`code/recover_restaurant_costs/`), estimation of restaurant adoption
   parameters (`code/restaurant_FC_estimation/`), and inversion of platform
   marginal costs from observed fees (`code/estimate_platform_costs/`).
   Produces Tables 2–5.
4. **Counterfactual analysis.** Code in `code/CF/` simulates equilibrium
   responses to commission caps and other policy interventions, producing
   Figures 3–8, Tables 6–15, and the supporting exhibits in OA Section K
   (post-estimation analysis of demand elasticities, diversion ratios,
   socially vs privately optimal markups, and counterfactual welfare
   heterogeneity).
5. **Bootstrap inference.** The directories `code/bootstrap_restaurant_costs/`
   and (within MATLAB) `code/demand_estimation/fe_estimation_boot.m` provide
   bootstrap standard errors for the restaurant-cost and fixed-effect
   parameters used downstream.

The frozen platform-side restaurant-listings/fee data are documented in
the Data Availability section below.

## Layout of this archive

This archive is organized into four top-level directories:

- **`code/`** — every R, Python, MATLAB, and Stata script in the
  pipeline. The chain is orchestrated by the top-level `run_all.sh`.
- **`output_orig/`** — the original outputs produced by running the
  pipeline against the proprietary real data. These are the exhibits
  that appear in the published paper, kept here for direct verification.
- **`output/`** — empty at archive time; gets populated when you run
  `run_all.sh` on the data tree assembled per the next section. The
  contents will be the synthetic-data analog of `output_orig/`.
- **`data/`** — the on-disk data tree the code reads from. As shipped
  it contains only the real-data files the author has the rights to
  redistribute (public-use ACS, COVID indicators, election results,
  the author-collected fee-cap legislation panel, the web-harvested
  platform data, and the restaurant price sample, plus the aggregated
  parameter / demographic auxiliaries under `data/bootstrap/`,
  `data/demand_estimation/`, and `data/eqm_data/`). The proprietary
  Numerator, YipitData, Edison, InfoGroup, and SimpleMaps inputs are
  **not** included and the corresponding subdirectories
  (`data/numerator/`, `data/yipitdata/`, `data/infogroup/`,
  `data/simplemaps_uszips_basicv1.77/`) do not exist in the archive.
  **The replicator must produce them before the pipeline can run**, by
  invoking the synthetic-data generator described in the next section
  (or by placing real proprietary files at those paths). The
  orchestrator (`run_all.sh`) calls the synthetic generator
  automatically during Stage 0 if the synthetic data is not already on
  disk, so simply running `bash run_all.sh` is sufficient for most
  users.

## Assembling `data/` before running the pipeline

Because the proprietary Numerator, YipitData, Edison, InfoGroup, and
SimpleMaps inputs cannot be redistributed, replicators need to populate
the proprietary file paths inside `data/` themselves before the
pipeline can run. There are two supported ways to do this.

### Option A — synthetic substitutes (default; recommended for code review)

Run the synthetic-data generator once from the repository root:

```
Rscript code/generate_synthetic_data.R
```

The generator deterministically populates:

- `data/numerator/` — synthetic Numerator consumer panel
- `data/yipitdata/` — synthetic YipitData restaurant listings + per-month RDS files
- `data/yipitdata/consumer_panel.csv` — synthetic Edison fee panel (with a built-in cap-induced DiD shock so the structural α coefficient comes out at a sensible sign)
- `data/infogroup/` — synthetic InfoGroup restaurant locations + the per-year processed `.rds` files
- `data/simplemaps_uszips_basicv1.77/uszips.csv` — deterministic from `noncensus::zip_codes` joined with the redistributed ACS S0101 population file

The synthetic data is calibrated so that every published exhibit's
script runs end-to-end and produces output of the expected schema, but
the numerical values in `output/` will not match those in
`output_orig/`.

### Option B — real proprietary inputs (for full reproducibility)

Replicators who have their own access to the proprietary inputs can skip
`generate_synthetic_data.R` and instead place the real files at the same
target paths under `data/` (e.g., the real Numerator panel at
`data/numerator/`, the real YipitData listings at `data/yipitdata/`,
etc.). The instructions in the Data Availability section below name the
specific files the code expects and how to obtain them.

## Running the pipeline

After `data/` has been assembled (Option A or Option B above), invoke
the orchestrator:

```
bash run_all.sh
```

Or for a single stage:

```
bash run_all.sh --only 2
```

See `run_all.sh --list` for the stage list.

**Expected total runtime is approximately 160 hours on a 16-core laptop
with 64 GB RAM.** The most computationally intensive elements are the
 MATLAB demand-estimation chain (structural step + 100-replicate 
 bootstrap), the restaurant-cost bootstrap
(`code/bootstrap_restaurant_costs/`), and the counterfactual sweeps in
`code/CF/`.

## Data Availability and Provenance Statements

The analysis draws on a mix of public-use datasets (American Community
Survey, COVID indicators, election results), author-collected datasets
(fee-cap legislation, web-harvested platform data, a restaurant price
sample), and proprietary datasets covering consumer purchases,
restaurant listings, geographic reference data, and platform-side
characteristics. The proprietary datasets are covered by data-use
agreements or licences with their vendors and **cannot be
redistributed** through this archive; the public-use and
author-collected datasets are included in full under `data/`.

### Statement about Rights

- [X] I certify that the author of the manuscript has legitimate access to
  and permission to use the data used in this manuscript.
- [X] I certify that the author has documented permission to redistribute /
  publish the public data contained within this replication package.
  Permissions are documented in `LICENCE.txt`. Proprietary
  or vendor-licensed datasets (Numerator, YipitData / Edison, InfoGroup,
  SimpleMaps) are **not** redistributed; replicators must obtain them
  directly from the vendors as described below.

### Licence for Data

The author-collected and derived data included under `data/` are
released under an **MIT Licence** (see `LICENCE.txt`).
Redistributed public-use data retain their original terms: the ACS
tables and JHU/OxCGRT/MIT Election Lab files are public domain or
CC-BY 4.0 as noted in their source descriptions, and the IPUMS extract
is redistributed solely under IPUMS USA's replication-archive provision
(see the IPUMS entry below).

### Summary of Availability

- [ ] All data **are** publicly available.
- [X] Some data **cannot be made** publicly available.
- [ ] **No data can be made** publicly available.

- [X] Confidential data used in this paper and not provided as part of the
  public replication package will be preserved for **five years** after
  publication, in accordance with journal policies.

#### Summary of Data Availability

| Data.Name | Data.Files | Location | Provided | Citation |
|---|---|---|---|---|
| American Community Survey aggregate tables (5-year, 2015–2019) | `ACSST5Y2019.S0101_*/`, `S1201_*/`, `S1501_*/`, `S1901_*/` | `data/ACS/` | Yes | U.S. Census Bureau (2020) |
| American Community Survey microdata (1-year, 2021) | `ACS_one_year_2021_v3.csv` | `data/ACS/` | Yes | Ruggles et al. (2025) [IPUMS USA] |
| OxCGRT COVID-19 policy data | OxCGRT_US_latest.csv | `data/USA-covid-policy-master/data/` | Yes | Hale et al. (2021) |
| Johns Hopkins COVID-19 case data | time_series_covid19_confirmed_US.csv | `data/COVID/` | Yes | Dong, Du, & Gardner (2020) |
| MIT Election Lab — county presidential returns | countypres_2000–2020.csv | `data/ElectionData/dataverse_files/` | Yes | MIT Election Data and Science Lab (2018) |
| SimpleMaps US Zip Code Database (Basic, v1.77) | uszips.csv | `data/simplemaps_uszips_basicv1.77/` | No | SimpleMaps (2021) |
| Fee-cap legislation panel (hand-collected) | commission_caps.csv | `data/small_data/` | Yes | Author-collected (2021–2022) |
| Restaurant price sample (hand-collected) | restaurant_price_sample.csv | `data/small_data/restaurant_price_sample/` | Yes | Author-collected (2022) |
| Second Measure platform market shares by CBSA (hand-transcribed) | second_measure_by_cbsa_march_2021.csv | `data/small_data/` | Yes | Bloomberg Second Measure (2021) |
| Author-constructed lookup tables (markets, time zones) | cbsa_codenames.csv, state_timezones.csv, timezone_offsets.csv | `data/small_data/` | Yes | Author-constructed |
| Numerator consumer panel | standard_nmr_feed_people_table.csv, standard_nmr_feed_item_table.csv, standard_nmr_feed_banner_table.csv, combined_static_table.csv, static_connect_table.csv, qsr_transactions_master.csv, summary_data-combined.csv | `data/numerator/` | No | Numerator (2021) |
| YipitData restaurant listings | listings_v2.csv | `data/yipitdata/` | No | YipitData (2021) |
| Edison transactional panel (consumer panel) | consumer_panel.csv | `data/yipitdata/` | No | Edison (2021) |
| InfoGroup / Data Axle restaurant census | jjcdvjugq1hsjpmn.csv (2019–2020), hiyy2ujx9xeo2wtl.csv (2021) | `data/infogroup/` | No | Data Axle, Inc. (2019, 2020, 2021) |
| Platform-side web-harvested data (collected Feb–Sep 2021; published descriptives use the Q2 window) | per-platform listings panels, geocoded fee/wait-time details, non-delivery fee records | `data/web_harvesting/` | Yes (raw layers; the YipitData-merged analysis intermediates are excluded and regenerated by the pipeline) | Author-collected (2021) |

### Details on each Data Source

**American Community Survey (ACS) aggregate Subject Tables, 5-year
2015–2019 estimates.** Four pre-aggregated tables — S0101 (Age and
Sex), S1201 (Marital Status), S1501 (Educational Attainment), and
S1901 (Income in the Past 12 Months) — used as ZCTA-level demographic
covariates and to construct demographic interactions in the demand
estimation. Downloaded from the U.S. Census Bureau's public data
portal at https://data.census.gov/ (December 2021 and April 2022); the
"Download" action there bundles each table into a directory containing
a `_data_with_overlays_*.csv` (the values + margins of error), a
`_metadata_*.csv` (column dictionary), and a `_table_title_*.txt`
(human-readable header). Included in raw form under
`data/ACS/ACSST5Y2019.<table>_*/`; processed by
`code/process_acs/process_acs.R` into
`data/ACS/processed/ACS_data.csv` (the file consumed downstream). To
re-download from scratch: visit https://data.census.gov/, search for
each table ID (e.g. `S0101`), filter the result to "2019 ACS 5-Year
Estimates Subject Tables" and the same geography included here
(all ZCTAs), and click Download. Public domain (no licence, no
registration required).

**ACS microdata via IPUMS USA, 1-year 2021 sample.** Individual-level
ACS records (~3.25 M rows) used by
`code/explore_numerator/representativeness.R` to compare the demographic
composition of the Numerator panel against the U.S. population
(produces OA Table E1). Included as `data/ACS/ACS_one_year_2021_v3.csv`.
Extracted from IPUMS USA (https://usa.ipums.org/usa/). The variable
list includes `YEAR, SAMPLE, SERIAL, CBSERIAL, HHWT, CLUSTER, STRATA,
GQ, HHINCOME, PERNUM, PERWT, FAMSIZE, NCHILD, AGE, MARST, …` (IPUMS
variable names). To re-extract: register at
https://usa.ipums.org/usa-action/menu (free), select **2021 ACS** as
the sample under "Select Samples", add the variables listed in the
file header, submit the extract, and download the resulting CSV. IPUMS
USA permits redistribution only "for the purpose of replication
archives" (see https://usa.ipums.org/usa/terms.shtml), under which
this archive includes the extract.

**Oxford COVID-19 Government Response Tracker (OxCGRT).** Daily
state-level government-response indices (stringency, containment, etc.).
Downloaded from the OxCGRT public repository. Included in raw form as
`data/USA-covid-policy-master/data/OxCGRT_US_latest.csv`; processed by
`code/COVID/process_lockdown_data.R` into
`data/USA-covid-policy-master/processed.csv`. CC-BY 4.0.

**Johns Hopkins University CSSE COVID-19 case data.** Daily county-level
confirmed-case counts. Downloaded from the JHU CSSE GitHub repository
(time-series files). Included under `data/COVID/`; processed by
`code/COVID/analyze_covid.R` into `data/COVID/covid_monthly.rds` and
`covid_processed.csv`. CC-BY 4.0.

**MIT Election Lab county presidential election results, 2000–2020.**
Downloaded from the Harvard Dataverse. Included under
`data/ElectionData/dataverse_files/countypres_2000-2020.csv`. CC-BY 4.0.

**Geographic crosswalks.** The files under `data/geo/` (a ZIP-/ZCTA-/
county-/CBSA-level crosswalk with lat/lon) are derived, built by
`code/generate_geo/`. Their one external, non-public input is the
**SimpleMaps US Zip Code Database** (Basic edition, v1.77), which the
build scripts read from
`data/simplemaps_uszips_basicv1.77/uszips.csv`. Rebuilding from scratch
additionally consumes the public ACS detail table B01003 (ZCTA
population), included in raw form under
`data/ACSDT5Y2019.B01003_2021-06-16T153509/`. **The SimpleMaps database
is not included in the replication package** — SimpleMaps' licence
prohibits public redistribution (`license.txt` in that directory). The
replicator
must obtain it directly: download the free US Zip Code Database from
https://simplemaps.com/data/us-zips, unzip it into
`data/simplemaps_uszips_basicv1.77/`, and confirm the extracted file is
named `uszips.csv`. The free edition is provided on condition of a
clearly visible backlink to that URL (see `license.txt`).

File not provided:

- `data/simplemaps_uszips_basicv1.77/uszips.csv` — SimpleMaps US Zip
  Code Database, Basic edition, v1.77.

**Fee-cap legislation panel.** Author-collected from press coverage,
municipal/state ordinances, and platform announcements covering policies
in effect from January 2020 through June 2021. Defines monthly maximum
commission rates by ZCTA over that window. The hand-collected
source file is `data/small_data/commission_caps.csv`; the analysis files
`data/fee_caps/monthly_fee_caps.csv` and `zip_fee_caps.rds` are built
from it by `code/describe_fee_caps/`. Released under the same licence as
other author-collected data (`LICENCE.txt`).

**Restaurant price sample.** Hand-collected menu-price information for
randomly sampled restaurants, provided as a single CSV
(`data/small_data/restaurant_price_sample/restaurant_price_sample.csv`)
consolidating the three collection waves used in the analysis; the
`wave` column identifies the draw (`seed_1868_list_1`,
`seed_1867_list_4`, `seed_1_extra`, where the seed is the RNG seed of
the sampling draw). One row per sampled restaurant: name/address
identifiers (`restaurant_name`, `street_address`, `city`, `state`,
`muni`, `search_query`), collector screening flags (`closed`,
`no_website`, `no_menu`, `not_a_restaurant`, `not_on_doordash`,
`not_on_uber`, `not_on_grubhub`; `x` = flagged), and two menu items per
restaurant (`menu_item_1`/`_2`) with offline, DoorDash, Uber Eats, and
Grubhub prices in dollars (`x` = item unavailable on that platform).
Read by `code/restaurant_price_sample/price_indices.R` to construct OA
Table G1 and the `price_indices.rds` robustness input. The original
per-wave xlsx collection workbooks are preserved in the project archive
outside the package. Released under the same licence as other
author-collected data.

**Second Measure market shares.** Platform market shares by metro area
(March 2021) estimated by the market research firm Bloomberg Second
Measure from observed consumer spending, hand-transcribed from the
interactive metro-level market-share table on their public "datapoints"
post "Which company is winning the restaurant food delivery war?"
(Liyin Yeo, 14 April 2021). Read from
`data/small_data/second_measure_by_cbsa_march_2021.csv` by
`code/explore_numerator/evaluate_numerator.R` as the external benchmark
in OA Figures E1 and E2.

**Author-constructed lookup tables.** Three small reference tables in
`data/small_data/` constructed by the author: `cbsa_codenames.csv`
(maps full Census CBSA titles to the short market codes used in file
names and scripts), `state_timezones.csv` (maps U.S. state names to
time zones), and `timezone_offsets.csv` (maps time zones to hour
offsets relative to Eastern). The time-zone tables are used to
normalise timestamps in the web-harvest processing; the CBSA table is
read throughout the pipeline.

**Numerator consumer panel.** Individual-level food-delivery transaction
records from Numerator (formerly InfoScout) covering 2019–2021. Used to
construct the consumer-side DiD moments and choice probabilities.
**Proprietary; not redistributed.** Researchers are advised to contact
Numerator (https://www.numerator.com/) to discuss the purchase of
the data required to replicate the article's analysis. The Numerator
data files used in this analysis (e.g.,
`data/numerator/qsr_transactions_master.csv`; see the table below) are
**not** included in this archive.

Specifically, the replicator must obtain the following seven tables from
Numerator, covering the full panel period (2019–2021) and the panel
universe used by the paper (U.S. delivery-active panelists; see the OA
for sample definitions). None of these files are provided:

| File expected at `data/numerator/...` | Description |
|---|---|
| `standard_nmr_feed_people_table.csv` | Panelist-level demographic table (pipe-separated): one row per panelist with demographic covariates. |
| `standard_nmr_feed_item_table.csv` | Item-level product metadata. Two deliveries are expected: `data/numerator/standard_nmr_feed_item_table.csv` and `data/numerator/2022-03-04/standard_nmr_feed_item_table.csv`; `code/process_numerator/combine_item_table.R` concatenates them into the derived `combined_item_table.rds`. |
| `standard_nmr_feed_banner_table.csv` | Banner (chain) lookup table (pipe-separated), used to attach chain identities to the summary transactions in `combine_fact_summary.R`. |
| `combined_static_table.csv` | Static-panelist table — rows giving the date range over which each panelist is in the "static" (continuously-observed) panel. |
| `static_connect_table.csv` | Per-panelist e-mail-receipt connection start/end dates. |
| `qsr_transactions_master.csv` | Item-level QSR (quick-service restaurant) transactions, concatenated across Numerator deliveries. |
| `summary_data-combined.csv` | Basket-level summary transactions, concatenated across deliveries. |

These seven files are the inputs to the processing pipeline in
`code/process_numerator/`, which produces the downstream analysis
datasets (`regression_data-*_v3.rds`, the monthly static panel, etc.).
The full run order — including the intermediate files written at each
step — is documented in `code/process_numerator/readme.md`. The
format-specific preprocessing scripts that unzipped and concatenated the
original Numerator deliveries have been removed from this archive, since
the layout of any future Numerator delivery is unlikely to match the one
used here; the replicator is expected to produce the seven inputs
above from whatever raw feed Numerator provides.

**YipitData restaurant listings.** Restaurant listings panel (merged
with InfoGroup) capturing restaurants' platform adoption.
**Proprietary; not redistributed.** Researchers may request data access
from YipitData (https://yipitdata.com/). The entry-point input,
`data/yipitdata/listings_v2.csv`, is produced from the raw YipitData
feed by a preprocessing script (`process_listings.py`) that — as with
Numerator — is no longer part of this archive, since the layout of any
future feed is unlikely to match the one used here; the replicator must
produce this file from the feed YipitData provides. The downstream
`locations_w_infogroup*` and `plat_loc_level_w_infogroup*` series are
built by `code/process_yipit/`. None of these `data/yipitdata/` files
are included in this archive.

File not provided:

- `data/yipitdata/listings_v2.csv` — restaurant listings panel.

**Edison transactional panel (consumer panel).** ZIP/month/platform-level
panel of food-delivery order volumes and average fees (including
estimates of average basket subtotals, delivery fees, service fees,
taxes, and tips), based on a panel of e-mail receipts and covering
January 2020 to May 2021. Used in the consumer-side DiD analysis and to
construct OA Figures E1 and E2 (cross-validation of the Numerator panel;
`code/explore_numerator/evaluate_numerator.R`). The analysis code reads
the panel from `data/yipitdata/consumer_panel.csv`. **Proprietary; not
redistributed.** Access is obtained through Edison; researchers should
contact Edison directly.

File not provided:

- `data/yipitdata/consumer_panel.csv` — ZIP/month consumer panel of
  platform transactions.

**InfoGroup / Data Axle restaurant census.** Annual restaurant universe
files (2019, 2020, 2021) used to enumerate the population of
restaurants in each metro and to construct restaurant attributes
(chain status, location). **Proprietary; not redistributed.** Available
from Reference USA/Data Axle via Wharton Research Data Services.
`code/process_infogroup/process_infogroup.R` builds the
`infogroup_<year>.rds` analysis files from the raw vendor extracts.

Files not provided (the replicator must supply both):

- `data/infogroup/jjcdvjugq1hsjpmn.csv` — 2019 and 2020 archive-year
  vintages (a single extract; split by year in processing).
- `data/infogroup/hiyy2ujx9xeo2wtl.csv` — 2021 vintage.

**Web-harvested platform-side data (2021).** Restaurant listings,
consumer-fee schedules, and delivery wait times collected by the author
directly from the four platforms (DoorDash, Uber Eats, Grubhub,
Postmates). Collection ran from February to September 2021 (per-file
`collection_time` ranges: DoorDash/Uber listings from February, Grubhub
details June–July, Postmates non-delivery fees through late September);
the published descriptive statistics (OA Table F2) restrict to the
second quarter, 1 April – 30 June 2021, via the date filter in
`code/web_harvest_descriptives/descr_tabs.R`. Observations dated after
the fee-cap panel's June 2021 endpoint inherit the May 2021 cap
snapshot in the cap merge (documented in `data_processing.R`). The
collection has two layers.
First, platform-by-platform *listings* panels enumerate every
restaurant page visible during the window — one row per restaurant
page — harvested by ZCTA/city page enumeration (Postmates via its
public API): `data/web_harvesting/doordash_all_restaurants/resto_city.csv`,
`ubereats_all_restaurants/all_ubereats.csv`,
`grubhub_CBSAs/combined_listings_all.csv` (with the de-duplicated
version `combined_listings_no_dupl.csv`), and
`postmates_all_restaurants/all_restaurants.csv`.
Second, *detail* harvests over that universe record each page's
advertised delivery fee and wait-time range, with ArcGIS-geocoded
coordinates (`doordash_details/`, `uber_details/`, and
`grubhub_CBSAs/geocoded_details.csv`), and service fees, small-order
fees, and taxes from attempted baskets (`doordash_nondelivery/`,
`ubereats_nondelivery/`, and
`postmates_all_restaurants/nondelivery_fees/geocoded_orders.csv`;
Grubhub did not display these separately).
`code/web_harvest_descriptives/` matches the detail data to the
**proprietary YipitData listings panel** (`add_*_resto_info.R`) and
combines the result into
`data/web_harvesting/analysis_data/analysis_data.rds`
(`data_processing.R`), from which OA Tables F1 and F2 are produced.
Because these merged intermediates embed YipitData-derived fields
(restaurant identities, partnership status, chain/cuisine
classifications), **they are not redistributed**; replicators who have
obtained the YipitData listings regenerate them by running the
`code/web_harvest_descriptives/` chain in the order given in the
pipeline instructions.

The browser-automation code used for the collection is not part of the
archive: the data are a frozen snapshot that re-running the harvesters
could not reproduce, because the platforms' pages and APIs have changed
substantively since 2021 and Postmates has been absorbed into Uber
Eats. Reproducibility of the analysis is preserved by
`code/web_harvest_descriptives/`, which consumes the raw files listed
above together with the proprietary YipitData listings panel (see the
YipitData entry). The included raw layers are released under the same
licence as other author-collected data (`LICENCE.txt`).

### Metadata and variable descriptions

Variable-level documentation for each raw source, per the journal's
metadata policy (§1.5). For included sources the codebook is either bundled
inside `data/` or is linked below; for proprietary sources (which
replicators obtain from the vendor) the variables consumed by the
analysis are enumerated here, and the vendor's full data dictionary is
provided with the licensed delivery.

**ACS aggregate Subject Tables.** Each table bundle is self-documenting:
`data/ACS/ACSST5Y2019.<table>_*/` contains a `*_metadata_*.csv` mapping
every machine-readable column code (e.g. `S0101_C01_001E`) to its
human-readable description, and a `*_table_title_*.txt` with the table
title. The same column definitions are browsable at
https://data.census.gov/ under each table ID.

**ACS microdata (IPUMS USA).** The extract's variable list is enumerated
in the source description above. Variable definitions, allowed values,
and coding schemes follow the IPUMS USA documentation at
https://usa.ipums.org/usa-action/variables/group (e.g. `HHINCOME`,
`MARST`, `EDUC` use the standard IPUMS codes).

**OxCGRT COVID-19 policy data.** The snapshot is the OxCGRT *US
subnational* repository (https://github.com/OxCGRT/USA-covid-policy);
variable definitions and the allowed values of every indicator are
given in the project codebook:
https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md.
The analysis extracts the state-by-month means of `StringencyIndex`,
`GovernmentResponseIndex`, `ContainmentHealthIndex`, and
`EconomicSupportIndex` (each 0–100), with states identified by
`RegionCode` (`US_<postal code>`).

**Johns Hopkins COVID-19 case data.** A `readme.txt` is bundled in
`data/COVID/`. The file follows the standard JHU CSSE US time-series
layout: one row per county (`UID`, `FIPS`, `Admin2` = county name,
`Province_State`, latitude/longitude) followed by one column per day of
cumulative confirmed cases. Upstream documentation:
https://github.com/CSSEGISandData/COVID-19.

**MIT Election Lab county returns.** The Dataverse codebook is bundled
as `data/ElectionData/dataverse_files/County Presidential Returns
2000-2020.md`. Columns: `year`, `state`, `state_po` (postal
abbreviation), `county_name`, `county_fips`, `office`, `candidate`,
`party`, `candidatevotes`, `totalvotes`, `version`, `mode` (vote mode).

**SimpleMaps US Zip Code Database (not redistributed).** Columns: `zip`,
`lat`, `lng`, `city`, `state_id`, `state_name`, `zcta` (TRUE if the ZIP
is its own ZCTA), `population`, `density`, `county_name`, `county_fips`,
`imprecise`, `military`. Field documentation:
https://simplemaps.com/data/us-zips.

**Fee-cap legislation panel (`data/small_data/commission_caps.csv`,
author-collected).** One row per cap policy. Columns:
`city` (enacting city; `all cities` for state-level policies; empty for
county-level policies), `state` (two-letter code), `county` (county name
for county-level policies, e.g. `Clark County`; empty otherwise), `cap`
(maximum total commission rate as a decimal fraction of the order
subtotal, e.g. `0.15`; where an ordinance separates delivery and
ancillary fees the *total* cap is coded, e.g. NYC's 15% + 5% is `0.20`),
`start_date` (effective date), `end_date` (date the cap lapsed; empty
if still in force at collection). Hand edits have used both ISO
`YYYY-MM-DD` and day-first `DD/MM/YYYY` date conventions;
`code/describe_fee_caps/evolution_of_fee_caps_v2.R` parses both
robustly and stops on ambiguous or unrecognised values. Remaining
columns: `temporary` (`yes` if tied to an emergency
declaration), `excludes_chains` (`1` if chain restaurants are exempt,
`0`/empty otherwise), `chains_note` and `note` (free-text annotations),
`fee_response` (free text), `source`/`source_alt`/`source_3` (URLs of
press coverage or ordinance text documenting the policy).

**Restaurant price sample
(`data/small_data/restaurant_price_sample/seed_*/`, author-collected).**
One workbook per sampling draw (the directory name records the random
seed); one row per sampled restaurant. Columns: `restaurant_name`,
`street_address`, `city`, `state`, `muni` (municipality label),
`search_query`; status flags `no_website`, `closed`, `not_a_restaurant`,
`no_menu`, `not_on_doordash`, `not_on_uber`, `not_on_grubhub`; and two
sampled menu items, each with `menu_item_<k>`, `offline_price_<k>`,
`doordash_price_<k>`, `uber_price_<k>`, `grubhub_price_<k>` (prices in
USD as displayed in spring 2021).

**Second Measure market shares
(`data/small_data/second_measure_by_cbsa_march_2021.csv`,
hand-transcribed).** One row per metro × platform. Columns: `cbsa`
(short market code matching `cbsa_codenames.csv`), `platform`
(`dd`/`uber`/`gh`/`pm`), `share` (share of March 2021 meal-delivery
sales in the metro, decimal fraction).

**Author-constructed lookup tables (`data/small_data/`).**
`cbsa_codenames.csv` — `CBSA_name` (full Census CBSA title), `cbsa`
(short market code). `state_timezones.csv` — `state` (full state name),
`timezone` (Eastern/Central/Mountain/Pacific). `timezone_offsets.csv` —
`timezone`, `offset` (hours relative to Eastern, e.g. Central = −1).

**Numerator consumer panel (proprietary).** Pipe-delimited feed tables
plus comma-delimited extracts. Variables consumed by the analysis:
`standard_nmr_feed_people_table.csv` — `POSTAL_CODE`, `USER_ID`, `AGE`,
`HOUSEHOLD_INCOME`, `HOUSEHOLD_SIZE`, `MARITAL_STATUS`, `EDUCATION`,
`GENDER`, `RACE`, `HISPANIC_FLAG`, `URBANICITY`, `STATE` (panelist
demographics in Numerator's categorical codings);
`standard_nmr_feed_item_table.csv` — `ITEM_ID`, `LOWEST_CATEGORY_ID`,
`item_description`; `standard_nmr_feed_banner_table.csv` — `BANNER`,
`RETAILER`, `CHANNEL`, `PARENT_CHANNEL`;
`qsr_transactions_master.csv` — one row per item × basket with
`BASKET_ID`, `USER_ID`, `BANNER`/`BANNER_ID`, `ITEM_*` (quantity, unit
price, total), `TRANSACTION_DATE`, `ORDER_METHOD_TYPE`,
`DELIVERY_PROVIDER`, `ORDER_PROVIDER` (these three identify the
delivery platform), `POSTAL_CODE`, `BASKET_SUB_TOTAL`, `BASKET_TOTAL`.
Numerator supplies its full data dictionary with the licensed feed.

**YipitData restaurant listings (proprietary).** `listings_v2.csv` —
one row per restaurant × platform × month: `restaurant_id`,
`merchant_name`, `name`, `platform`, `observation_month`,
`postal_code`, `latitude`, `longitude`.

**Edison transactional panel (proprietary).** `consumer_panel.csv` —
ZIP × month × merchant aggregates: `zip`, `month`, `merchant_name`,
`orders_scaled` (scaled order volume),
`observed_orders_for_orders_scaled_calculation`, `avg_service_fee`,
`avg_delivery_fee`, `avg_order_discount`, `avg_order_tax`,
`avg_order_tip`, `aov_feesandtips_included`, `aov_feesandtips_excluded`
(average order values in dollars).

**InfoGroup / Data Axle restaurant census (proprietary).** One row per
establishment: `abi` (establishment ID), `parent_number`, `company`,
`address_line_1`, `city`, `state`, `zipcode`, `primary_sic_code`
(restaurants are SIC 5812), `sic6_descriptions` (+ `_sic1`, `_sic2`),
`latitude`, `longitude`, `archive_version_year`. Data Axle's data
dictionary accompanies the licensed delivery.

**Web-harvested platform data (author-collected).**
`geocoded_details.csv` — one row per restaurant page: platform-side
IDs, ArcGIS-geocoded latitude/longitude, advertised consumer delivery
fee, advertised wait-time range, harvest date. `geocoded_orders.csv` —
one row per attempted basket: service fee, small-order fee, taxes, and
the destination's latitude/longitude. Listings files — one row per
restaurant page visible during the collection window, with platform
IDs and location fields. `analysis_data/analysis_data.rds` — the
combined fee dataset behind OA Table F1 (`fee_decomp.csv`) and OA
Table F2 (`overall_descr.csv`).

## Dataset list

The provided data are located in `data/` with the following subdirectories.
Files that are computed by code in this archive and saved to disk are
marked "derived"; files that are inputs from external sources are marked
by their source.

| Data file | Source | Notes | Provided |
|---|---|---|---|
| `data/ACS/ACSST5Y2019.S0101_*/`, `S1201_*/`, `S1501_*/`, `S1901_*/` | data.census.gov | ACS 5-year 2015–2019 Subject Tables (Age & Sex, Marital Status, Education, Income) | Yes |
| `data/ACS/ACS_one_year_2021_v3.csv` | IPUMS USA | 2021 1-year ACS microdata extract | Yes |
| `data/ACS/processed/ACS_data.csv`, `ACS_nearby.csv` | derived | Built by `code/process_acs/process_acs.R` and `nearby_demos_acs.R` from the 5-year aggregate tables | Yes |
| `data/COVID/covid_monthly.rds`, `covid_processed.csv` | derived (JHU) | Built by `code/COVID/analyze_covid.R` | Yes |
| `data/USA-covid-policy-master/processed.csv` | derived (OxCGRT) | Built by `code/COVID/process_lockdown_data.R` | Yes |
| `data/ElectionData/dataverse_files/countypres_2000-2020.csv` | MIT Election Lab | Public | Yes |
| `data/geo/geo.csv`, `geo_with_zctas.csv`, `zip_map.rds` | derived + U.S. Census | Built by `code/generate_geo/` | Yes |
| `data/fee_caps/monthly_fee_caps.csv`, `zip_fee_caps.rds` | Author | Built by `code/describe_fee_caps/` | Yes |
| `data/small_data/commission_caps.csv` | Author | Hand-coded fee-cap policy listing (source of `data/fee_caps/`) | Yes |
| `data/small_data/restaurant_price_sample/restaurant_price_sample.csv` | Author | Hand-collected restaurant price sample (OA Table G1) | Yes |
| `data/small_data/second_measure_by_cbsa_march_2021.csv` | Bloomberg Second Measure | Hand-transcribed metro market shares (OA Figures E1/E2) | Yes |
| `data/small_data/cbsa_codenames.csv`, `state_timezones.csv`, `timezone_offsets.csv` | Author | Constructed lookup tables (markets, time zones) | Yes |
| `data/numerator/*` | Numerator Inc. | Proprietary; obtain from vendor | **No** |
| `data/yipitdata/*` (except `consumer_panel.csv`) | YipitData | Proprietary; obtain from vendor | **No** |
| `data/yipitdata/consumer_panel.csv` | Edison | Proprietary; obtain from vendor | **No** |
| `data/infogroup/*` | Data Axle (InfoGroup) | Proprietary; obtain via library subscription | **No** |
| `data/web_harvesting/*` | Author (web-harvested Feb–Sep 2021) | Frozen snapshot (raw layers only); documented in the Data Availability section | Yes |
| `data/web_harvesting/analysis_data/*` | derived (author + YipitData) | YipitData-merged intermediates; **not redistributed** — regenerated by `code/web_harvest_descriptives/` | **No** |
| `data/est_dat/*` | derived | Estimation samples built by `code/prepare_est_data/`; **not redistributed** because they include panelist-level Numerator records — regenerated by the pipeline | **No** |
| `data/eqm_data/eqm_data*.rds` | derived | Equilibrium-analysis data built by `code/prepare_eqm_data/process_data_for_eqm.R`; **not redistributed** because the per-county frames carry panelist-level rows — regenerated by the pipeline | **No** |
| `data/eqm_data/nearby_demo_data.rds`, `pop_dist.csv` | derived | ZIP-level demographic + population aggregates | Yes |
| `data/demand_estimation/est_results-yipitdata.mat`, `inference.mat` | derived | Output of `code/demand_estimation/` MATLAB chain (θ-vector + inference matrices) | Yes |
| `data/bootstrap/dat/Psi/`, `data/bootstrap/dat/restoFC/` | derived | Bootstrap replicates of structural parameter vectors (small CSVs / RDS) | Yes |
| `data/bootstrap/dat/{CCP,EPi,restoMC}/` | derived | Bootstrap replicates of CCPs / per-individual expected profits / restaurant marginal costs; **not redistributed** because they carry panelist-level fitted values — regenerated by the pipeline | **No** |

## Computational requirements

### Software Requirements

The pipeline mixes **R, MATLAB, and Stata**. Two setup scripts are
provided to install the R and Stata dependencies:

- `code/0_setup.R` — installs all CRAN packages plus the two
  locally-developed R packages (`EconTools`, `FoodDeliveryTools`).
  Idempotent. Run from the repository root:
  ```bash
  Rscript code/0_setup.R
  ```
- `code/0_setup.do` — installs `ftools`, `reghdfe`, and `estout` from
  SSC. Idempotent. Run via `do code/0_setup.do`.

MATLAB toolboxes cannot be installed by a script; they must be available
under the replicator's MATLAB licence.

- **R** (the published results were last produced with R 4.5.2)
  - CRAN packages: `AER`, `data.table`, `doBy`, `dplyr`, `fixest`,
    `geosphere`, `Matrix`, `parallel`, `pracma`, `R.matlab`, `RColorBrewer`,
    `scales`, `stringdist`, `stringi`, `stringr`, `tidyr`, `wesanderson`,
    and `noncensus` (a dependency of `EconTools`; archived from
    CRAN — if `install.packages("noncensus")` fails, `code/0_setup.R`
    falls back to the version-pinned CRAN archive tarball
    (`noncensus_0.1.tar.gz`), and failing that to
    `github.com/ramhiser/noncensus`).
  - **Two locally-developed R packages** at `code/EconTools/` (general
    file-I/O and formatter helpers) and `code/FoodDeliveryTools/`
    (analysis-specific helpers: data loaders, equilibrium computations,
    post-estimation utilities). `code/0_setup.R` installs both from
    source.
- **MATLAB** (last run with R2026a)
  - **Parallel Computing Toolbox** (used by the `parfor` loop in
    `fe_estimation_boot.m`).
  - **Statistics and Machine Learning Toolbox** (used by various
    distribution functions in the simulation routines).
  - **Optimization Toolbox** (referenced by a few auxiliary helpers,
    though the production GMM estimator does not depend on it).
- **Stata** (last run with **Stata 19**)
  - `reghdfe` (for the consumer-DiD regressions in
    `code/explore_yipit/` and the price-effect regressions in
    `code/numerator_menu_pricing/estimate_price_effects_disagg.do`)
  - `estout` (for table export from Stata)
  - `ftools` (a `reghdfe` dependency)

The code was developed and last run under **macOS**. No
operating-system-specific features are used in the analysis code; the
pipeline should run on Linux without modification. Windows replicators
may encounter path-separator issues in a handful of R scripts; if so,
substitute `file.path(...)` for the literal `/` separators.

### Controlled Randomness

- [X] Random seeds are set at the start of every script that uses random
  numbers. The relevant lines are:
  - `code/demand_estimation/estimate_consumer_choice.m:11` — `rng(1)`,
    then re-seeded inside `auxiliary_functions/GenerateDataObjs.m:9` so
    that simulation draws are deterministic.
  - `code/demand_estimation/fe_estimation_boot.m:15` — top-level
    `rng(1)`; replicate-specific seeds set via `rng(b)` inside the
    `parfor` loop.
  - `code/demand_estimation/auxiliary_functions/RunFE.m:31,38` —
    `rng(1)` and per-replicate `rng(b)`.
  - `code/demand_estimation/compute_GMM_SE_v2.m:7` — `rng(1)`.
  - R scripts using random number generation typically set `set.seed(1)`
    at the top of each `main()` (e.g.,
    `code/demand_estimation/postest_analysis.R`).

The MATLAB simulation draws are pre-generated once per estimation run
and stored in `data_objs` (see `GenerateDataObjs.m`); this implicitly
implements common random numbers across calls to `ComputeSimMoments`
within a given run, which is required for the numerical Jacobian in
`compute_GMM_SE_v2.m` to be well-defined.

### Memory, Runtime, Storage Requirements

#### Summary time to reproduce

Approximate time needed to reproduce all analyses on a 16-core, 64 GB
laptop:

- [ ] <10 minutes
- [ ] 10-60 minutes
- [ ] 1-2 hours
- [ ] 2-8 hours
- [ ] 8-24 hours
- [ ] 1-3 days
- [X] **3-14 days** (approximately 160 hours wall-clock on a 16-core
  laptop). The largest individual stages are the MATLAB demand
  estimation (`estimate_consumer_choice.m` plus the 100-replicate FE
  bootstrap `fe_estimation_boot.m`), the restaurant-cost bootstrap
  (`code/bootstrap_restaurant_costs/`), and the counterfactual sweeps in
  `code/CF/`.
- [ ] > 14 days

#### Summary of required storage space

The archive as distributed (i.e., after the proprietary `data/numerator/`,
`data/yipitdata/`, `data/infogroup/`, and
`data/simplemaps_uszips_basicv1.77/` subtrees are excluded — see the
Data Availability section above) is approximately **11 GB**. The full
working tree on the author's machine, including the proprietary data
that cannot be redistributed, is approximately **213 GB**.

- [ ] < 25 MBytes
- [ ] 25 MB - 250 MB
- [ ] 250 MB - 2 GB
- [ ] 2 GB - 25 GB
- [X] **25 GB - 250 GB**. The proprietary data the replicator must
  obtain from vendors adds roughly 145 GB to disk
  (Numerator ~115 GB, YipitData / Edison ~29 GB, InfoGroup ~1.4 GB).
  Intermediate outputs produced by the pipeline (the `eqm_data*.rds`
  files, MATLAB `.mat` files, bootstrap replicates, and the
  `output/CF_feefirst/analysis_*` directories) add a further ~20 GB.
- [ ] > 250 GB

#### Computational Details

The code was last run on **a 2023 16-core M3 Max MacBook Pro with 64 GB
RAM running macOS Sequoia**. No external compute (HPC, cluster, cloud)
is required; all HPC-specific scripts (`*.sbatch`, `*_grid.sh`,
`HPC_*.m`) were removed during the replication-package cleanup, and the
surviving code runs entirely on a
single machine.

## Description of programs/code

The repository is organized as follows:

```
.
├── code/                       Analysis code (R, MATLAB, Stata)
│   ├── COVID/                  COVID policy + case data processing (→ Table 1 inputs)
│   ├── CF/                     Counterfactual analysis (Figures 3–8, Tables 6–15)
│   ├── compute_pop_distribution/   Population-weight construction for equilibrium data
│   ├── demand_estimation/      Consumer-choice GMM estimation (Table 2, OA K2, K3)
│   ├── describe_fee_caps/      Fee-cap legislation panel construction
│   ├── describe_portfolio_choice/  Restaurant-platform adoption description (Figure 2)
│   ├── EconTools/              Project-specific general-purpose R package (file-I/O, formatter helpers)
│   ├── estimate_platform_costs/    Platform marginal-cost inversion (Table 5)
│   ├── explore_numerator/      Numerator-panel descriptive analysis (OA E1)
│   ├── explore_yipit/          YipitData DiD (Table 1, OA D2, OA D1b)
│   ├── FoodDeliveryTools/      Project-specific analysis R package
│   ├── generate_geo/           Geographic crosswalk construction
│   ├── neutrality/             Fee-structure neutrality counterfactual (OA C1, C2)
│   ├── numerator_menu_pricing/ Menu-price regression + price indices (Appendix A1, OA D1a-d)
│   ├── prepare_eqm_data/       Build equilibrium-analysis input (`eqm_data*.rds`)
│   ├── prepare_est_data/       Build estimation samples
│   ├── process_acs/            ACS demographic processing
│   ├── process_infogroup/      InfoGroup restaurant-census processing
│   ├── process_numerator/      Numerator panel ingestion + DiD-panel construction
│   ├── process_yipit/          YipitData panel ingestion + brand merging
│   ├── recover_restaurant_costs/   Restaurant marginal-cost recovery (Tables 3a/b, OA H1)
│   ├── restaurant_choice_demos/    Restaurant-choice demographic feature build
│   ├── restaurant_FC_estimation/   Restaurant adoption choice estimation (Tables 4a/b/c)
│   ├── restaurant_price_sample/    Restaurant-price sample regressions (OA G1)
│   ├── bootstrap_restaurant_costs/ Bootstrap SE construction for the cost chain
│   ├── web_harvest_descriptives/   Web-harvested-data descriptive statistics (OA F1, F2)
│   └── zip_code_weights/       Population-weight construction
├── data/                       Public + author-collected inputs and aggregated derivatives
│   └── small_data/             Hand-collected/author-constructed small inputs (e.g., commission_caps.csv)
├── output/                     Empty at archive time; populated by `run_all.sh`
├── output_orig/                Outputs produced by the author from the proprietary real data (reference exhibits)
└── (root)                      README.md, LICENCE.txt, run_all.sh
```

Several `code/` subdirectories were deleted during cleanup because they
contained exploratory analyses that did not feed into any final
exhibit.

### Licence for Code

The code is licensed under an **MIT Licence**. See
`LICENCE.txt`.

## Instructions to Replicators

> **Setup (one-time).** Install R, MATLAB R2026a, and Stata 19 as
> described in *Software Requirements*. Then run the two setup scripts
> to install R/Stata dependencies (including the locally-developed
> `EconTools` and `FoodDeliveryTools` packages):
>
> ```bash
> Rscript code/0_setup.R
> stata -b do code/0_setup.do
> ```
>
> Obtain the proprietary datasets (Numerator, YipitData / Edison,
> InfoGroup) from the relevant vendors and place them under the
> directories indicated in the **Dataset list** above. The pipeline will
> fail at specific stages if any of these are missing; the error message
> from R/MATLAB will identify the missing file.

### Running the pipeline

The full pipeline is driven by the top-level orchestrator
`run_all.sh`. It invokes every R, Stata, and MATLAB script
in dependency order, organised into nine numbered stages
(0 setup; 1 raw data; 2 descriptive exhibits; 3 demand estimation;
4 restaurant marginal costs; 5 restaurant fixed costs; 6 restaurant-cost
bootstrap; 7 platform marginal costs; 8 counterfactuals). The
step-by-step listing further down this section documents what each
stage runs and which exhibits it produces — those steps are the same
ones the orchestrator invokes, listed so a replicator targeting a
single exhibit can find the relevant script directly.

```bash
# Full pipeline (~160 hours on a 16-core, 64 GB workstation)
run_all.sh

# Resume from a specific stage
run_all.sh --from 4

# Run a contiguous range of stages
run_all.sh --from 4 --to 5

# Run a single stage
run_all.sh --only 8

# List the stages without running anything
run_all.sh --list

# Echo every command without executing (useful for inspection)
run_all.sh --dry-run
```

The orchestrator reads several environment variables so it can adapt
to non-default installations:

- `RSCRIPT` (default `Rscript`) — R launcher.
- `STATA` (default `stata-mp -b do`) — Stata launcher. Set to
  `stata-se -b do`, `statamp -b do`, etc. as your installation requires.
- `MATLAB` (default `matlab -nodisplay -nosplash -batch`) — MATLAB
  launcher.
- `CF_NCORES` (default `8`) — parallel-cluster size used by the
  counterfactual solvers in stage 8.

The orchestrator runs every command from the repository root and
aborts on the first non-zero exit, printing the failing command. All
intermediate outputs are written to fixed paths (no spec-timestamp
suffix), so downstream stages pick up the latest run automatically.

The step-by-step listing below catalogues the scripts run by each
stage. Replicators who want to run a single exhibit can invoke its
script directly rather than running the orchestrator.

**Stage 1 — Process raw inputs.**

1. `Rscript code/process_acs/process_acs.R`
2. `Rscript code/process_acs/nearby_demos_acs.R`
3. `Rscript code/COVID/analyze_covid.R`
4. `Rscript code/COVID/process_lockdown_data.R`
5. `Rscript code/generate_geo/generate_geo.R`
6. `Rscript code/generate_geo/load_zcta_data.R`
7. `Rscript code/describe_fee_caps/monthly_fee_cap_data.R`
8. `Rscript code/describe_fee_caps/evolution_of_fee_caps_v2.R`
9. `Rscript code/process_infogroup/process_infogroup.R`
10. `Rscript code/process_numerator/...` (sequence documented in
    `code/process_numerator/readme.md`)
11. `Rscript code/process_yipit/...` (sequence documented in
    `code/process_yipit/readme.md`)
12. `Rscript code/compute_pop_distribution/pop_dist_take2.R`
13. `Rscript code/zip_code_weights/construct_zip_code_weights.R`
14. `Rscript code/prepare_est_data/run_all.R` (orchestrates the
    estimation-sample build)
15. Stata: `do code/numerator_menu_pricing/estimate_price_effects_disagg.do`
16. `Rscript code/numerator_menu_pricing/compute_price_indices.R`

**Stage 2 — Descriptive analysis.**

17. `Rscript code/explore_yipit/prepare_DiD_data.R` (builds
    `data/yipitdata/consumer_panel_processed.csv`)
18. `Rscript code/explore_yipit/demand_estimation_DiD.R` (the R port of
    the previous Stata DiD; produces the inputs to Table 1)
19. `Rscript code/explore_yipit/process_demand_estimation_DiD.R`
    (produces **Table 1**)
20. `Rscript code/explore_yipit/yipit_price_structure.R` (produces
    **OA Figures D2a, D2b**)
21. `Rscript code/explore_yipit/restaurant_multihoming.R` (produces
    **OA Table D1b**)
22. `Rscript code/explore_numerator/plot_market_shares.R` (produces
    **Figure 1**)
23. `Rscript code/explore_numerator/market_shares_by_CBSA.R`
24. `Rscript code/explore_numerator/evaluate_numerator.R` (produces
    **OA Figure E1, E2**)
25. `Rscript code/explore_numerator/excess_inertia.R` (produces
    **OA Table D1a**)
26. `Rscript code/explore_numerator/representativeness.R` (produces
    **OA Table E1**)
27. `Rscript code/describe_portfolio_choice/describe_portfolio_choice.R`
    (produces **Figure 2**)
28. `Rscript code/web_harvest_descriptives/data_processing.R`
29. `Rscript code/web_harvest_descriptives/describe_prices.R` (produces
    **OA Table F1**)
30. `Rscript code/web_harvest_descriptives/descr_tabs.R` (produces
    **OA Table F2**)
31. `Rscript code/numerator_menu_pricing/generate_item_level_reg_table.R`
    (produces **Appendix Table A1** from the regression CSV that step 15
    already wrote)
32. `Rscript code/numerator_menu_pricing/describe_prices.R` (produces
    **OA Figures D1a–d**)

**Stage 3 — Structural estimation.**

33. Launch the MATLAB demand-estimation pipeline:

    ```bash
    cd code/demand_estimation
    matlab -nodisplay -nosplash -batch "run_pipeline"
    ```

    This runs, in order: `estimate_consumer_choice` → `fe_estimation` →
    `compute_GMM_SE_v2` → `fe_estimation_boot` → `SE_table`. All outputs
    are written to fixed paths so the downstream R chain reads them
    automatically.
34. `Rscript code/demand_estimation/produce_table.R` (produces
    **Table 2**)
35. `Rscript code/prepare_eqm_data/process_data_for_eqm.R` (builds
    `data/eqm_data/eqm_data_nsim50.rds`, consumed by the
    cost/CF chain)
36. `Rscript code/demand_estimation/postest_analysis.R` (produces
    **OA Tables K2, K3** plus per-market elasticity/diversion files
    consumed by `cannibalization.R` and `code/CF/`)
37. `Rscript code/demand_estimation/cannibalization.R`
38. `Rscript code/demand_estimation/sample_size_table.R`
39. `Rscript code/demand_estimation/evaluate_fit.R` (model-fit
    diagnostics)
40. `Rscript code/recover_restaurant_costs/recover_restaurant_costs_v2.R`
    (writes `output/recover_restaurant_costs/retaurant_costs.rds`)
41. `Rscript code/recover_restaurant_costs/recover_restaurant_costs_NPP.R`
    (the non-parity-penalty variant used by OA Table H1)
42. `Rscript code/bootstrap_restaurant_costs/bootstrap_all_costs.R`
    (runs the four boot stages in order; takes hours)
43. `Rscript code/recover_restaurant_costs/summarize_costs_v3.R`
    (produces **Tables 3a, 3b**)
44. `Rscript code/recover_restaurant_costs/produce_GMM_table.R`
45. `Rscript code/recover_restaurant_costs/compare_pricing_models.R`
    (produces **OA Table H1**)
46. `Rscript code/restaurant_price_sample/price_indices.R` (produces
    **OA Table G1**)
47. `Rscript code/restaurant_FC_estimation/...` (sequence to produce
    **Tables 4a, 4b, 4c**; consult the individual scripts in
    `code/restaurant_FC_estimation/` for the dependency order)
48. MATLAB: `cd code/estimate_platform_costs && matlab -batch
    "determine_submarkets; invert_FOCs_by_county"`
49. `Rscript code/estimate_platform_costs/describe_pMC.R` (produces
    **Table 5**)

**Stage 4 — Counterfactual analysis.**

50. Sequence in `code/CF/` produces all of Figures 3–8 and Tables 6–15
    plus OA K-series. See the headers of each `code/CF/*.R` script for
    its specific exhibit assignment and the end-to-end dependency order
    encoded in `run_all.sh`.

**Stage 5 — Neutrality counterfactual.**

51. `Rscript code/neutrality/compare_fixed_prop.R` (produces
    **OA Tables C1a, C1b, C2a, C2b**)

## List of tables and programs

The main-text table-to-program mapping is reproduced below. Online
Appendix exhibits follow the same structure: each producing script is
named after its target section (e.g., `code/explore_numerator/*` produces
OA Section E exhibits, `code/web_harvest_descriptives/*` produces OA
Section F, `code/CF/*` produces the OA K-series, etc.). Output paths
mirror the producer's directory under `output/`.

- [ ] All numbers provided in text in the paper
- [X] All tables and figures in the paper
- [ ] Selected tables and figures in the paper, as explained and justified below

| Figure/Table | Program | Output file |
|---|---|---|
| Figure 1 | `code/explore_numerator/plot_market_shares.R` | `output/explore_numerator/market_shares_Q2-2021_small.pdf` |
| Figure 2 | `code/describe_portfolio_choice/describe_portfolio_choice.R` | `output/describe_portfolio_choice/portfolio_distribution_ig2021.pdf` |
| Figure 3 | `code/CF/analyze_vary_cap.R` | `output/CF_feefirst/analysis_nsim50/vary_cap_all_markets.pdf` |
| Figure 4a-c | `code/CF/analyze_vary_cap.R` | `output/CF_feefirst/analysis_nsim50/{fee,Jshr,Sratio}_by_cap_level.pdf` |
| Figure 5 | `code/CF/variety_vs_fixed_costs.R` | `output/CF_feefirst/analysis_nsim50/variety_vs_fixed_costs_by_cap_level.pdf` |
| Figure 6a-b | `code/CF/relative_change_plot_take2.R` | `output/CF_feefirst/analysis_nsim50/{rpi_profit_effects_sub,c_welfare_effects}.pdf` |
| Figure 7 | `code/CF/analyze_twosided.R` | `output/CF_feefirst/analysis_nsim50/twosided_all_markets.pdf` |
| Figure 8a-c | `code/CF/analyze_twosided.R` | `output/CF_feefirst/analysis_nsim50/{fee,Jshr,Sratio}_by_cap_level-twosided.pdf` |
| Table 1 | `code/explore_yipit/process_demand_estimation_DiD.R` | `output/explore_yipit/DiD_for_demand_estimation-paper.csv` |
| Table 2 | `code/demand_estimation/produce_table.R` | `output/demand_estimation/est_SE-yipitdata_formatted.csv` |
| Table 3a | `code/recover_restaurant_costs/summarize_costs_v3.R` | `output/recover_restaurant_costs/describe/combo_tab_mc_all_SEs.csv` |
| Table 3b | `code/recover_restaurant_costs/summarize_costs_v3.R` | `output/recover_restaurant_costs/describe/combo_tab_all_SEs.csv` |
| Table 4a-c | `code/restaurant_FC_estimation/GMM_CCP_table.R` | `output/restaurant_FC_estimation/restaurant_cost_RC_GMM-ccp_{sigmas,kappas}.csv` and `cost_by_size_pool.pdf` |
| Table 5 | `code/estimate_platform_costs/describe_pMC.R` | `output/estimate_platform_costs/MC_H/spec_nsim50/MC_results.csv` |
| Table 6 | `code/CF/compare_priv_soc.R` | `output/CF_feefirst/analysis_nsim50/compare_priv_soc_CR.csv` |
| Table 7 | `code/CF/compute_distortions.R` | `output/CF_feefirst/analysis_nsim50/distortions_table.csv` |
| Table 8a-b | `code/CF/compare_priv_soc.R` | `output/CF_feefirst/analysis_nsim50/{welfare_soc_priv,soc_priv_observables}.csv` |
| Tables 9–15 | `code/CF/...` | see individual script headers in `code/CF/` for the per-table producer and output path |
| Appendix Table A1 | `code/numerator_menu_pricing/generate_item_level_reg_table.R` | `output/numerator_menu_pricing/disagg_results/pricing_reg_combo_allsample.csv` |
| OA Tables / Figures | various | producers under `code/explore_numerator/`, `code/web_harvest_descriptives/`, `code/restaurant_price_sample/`, `code/CF/`, `code/demand_estimation/postest_analysis.R`, `code/neutrality/`, etc. — output paths mirror the producer's directory under `output/` |

## References

The references below have been cross-checked against the live analysis
code: each cited dataset appears in at least one current script
(`data/numerator/` is read by 27 scripts, `data/yipitdata/` by 23,
`data/ACS/` by 9, `data/COVID/` and `data/USA-covid-policy-master/` by
2 each, `data/ElectionData/` by 1, `data/infogroup/` by 2, the web-harvested
data files by 6, and the Edison panel
(`data/yipitdata/consumer_panel.csv`) by 6, including
`evaluate_numerator.R` for OA Figures E1/E2).

- Bloomberg Second Measure (2021). "Which company is winning the
  restaurant food delivery war?" (Liyin Yeo, 14 April 2021)
  [metro-level market-share estimates from observed consumer spending;
  March 2021 values].
  https://secondmeasure.com/datapoints/food-delivery-services-grubhub-uber-eats-doordash-postmates/

- Dong, E., Du, H., & Gardner, L. (2020). An interactive web-based
  dashboard to track COVID-19 in real time. *The Lancet Infectious
  Diseases*, 20(5), 533–534.
  https://doi.org/10.1016/S1473-3099(20)30120-1

- Data Axle, Inc. (2019, 2020, 2021). U.S. Businesses Database
  [proprietary dataset]. Papillion, NE: Data Axle, Inc. (formerly
  InfoGroup, Inc.).

- Edison (2021). U.S. Food-Delivery Consumer Panel [proprietary
  dataset].

- Hale, T., Angrist, N., Goldszmidt, R., Kira, B., Petherick, A.,
  Phillips, T., Webster, S., Cameron-Blake, E., Hallas, L., Majumdar, S.,
  & Tatlow, H. (2021). A global panel database of pandemic policies
  (Oxford COVID-19 Government Response Tracker). *Nature Human
  Behaviour*, 5, 529–538.
  https://doi.org/10.1038/s41562-021-01079-8

- MIT Election Data and Science Lab (2018). County Presidential Election
  Returns 2000–2020 [dataset]. Harvard Dataverse, V11.
  UNF:6:HaZ8GWG8D2abLleXN3uEig==. https://doi.org/10.7910/DVN/VOQCHQ

- Numerator, Inc. (2021). Numerator Insights Consumer Panel [proprietary
  dataset]. Chicago, IL: Numerator, Inc.

- Ruggles, S., Flood, S., Sobek, M., Backman, D., Cooper, G., Rivera
  Drew, J. A., Richards, S., Rogers, R., Schroeder, J., & Williams,
  K. C. W. (2025). IPUMS USA: Version 16.0 [dataset]. Minneapolis, MN:
  IPUMS. https://doi.org/10.18128/D010.V16.0

- SimpleMaps (2021). US Zip Code Database, Basic edition, v1.77
  [dataset]. Pareto Software, LLC. https://simplemaps.com/data/us-zips

- U.S. Census Bureau (2020). American Community Survey 5-Year Estimates,
  2015–2019: Subject Tables S0101, S1201, S1501, S1901 [dataset].
  Washington, DC: U.S. Census Bureau. https://data.census.gov/

- YipitData (2021). U.S. Food-Delivery Restaurant Listings [proprietary
  dataset]. New York, NY: YipitData.

---

## Acknowledgements

See the main text for acknowledgements.

For replication-package-specific questions, please contact:
**michael.sullivan@sauder.ubc.ca**.
