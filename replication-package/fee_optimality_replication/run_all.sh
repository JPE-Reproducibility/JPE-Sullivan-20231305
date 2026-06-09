#!/usr/bin/env bash
#
# run_all.sh — top-level orchestrator for the replication package of
# "Fee Optimality in a Two-Sided Market" (Michael Sullivan, JPE).
#
# Invokes every R, Stata, and MATLAB script in the pipeline in dependency
# order. The pipeline is organised into 8 stages; any contiguous range of
# stages can be run via --from / --to. See replication/README.md for
# the conceptual pipeline overview.
#
# Usage:
#   replication/run_all.sh                  # run every stage 0..8
#   replication/run_all.sh --from 4         # resume from stage 4
#   replication/run_all.sh --from 4 --to 5  # run stages 4 and 5 only
#   replication/run_all.sh --only 7         # run stage 7 only (= --from 7 --to 7)
#   replication/run_all.sh --list           # print the stage list and exit
#   replication/run_all.sh --dry-run        # echo every command without running
#
# Conventions:
#   - All paths are relative to the repository root. The script cds there
#     before doing anything.
#   - Each stage prints "[stage N] <name>" before running, and any non-zero
#     exit aborts the whole run with the failing command shown.
#   - Heavy stages are tagged "[HEAVY]" in --list. They typically need a
#     workstation or cluster, not a laptop. Expected total runtime on a
#     16-core, 64 GB workstation is ~160 hours (per README.md).
#
# Dependencies (one-time):
#   Rscript code/0_setup.R   # installs all CRAN packages + local R packages
#   stata -b do code/0_setup.do   # installs ftools/reghdfe/estout from SSC
#   MATLAB R2023a or later (for code/demand_estimation/).
#
# Replicator entry points before running any stage:
#   - Make sure the proprietary inputs in data/numerator/, data/yipitdata/,
#     and data/infogroup/ are in place (see replication/README.md §"Data
#     Availability").

set -euo pipefail

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

# Stata launch. Adjust to statamp / stata-se / stata-mp as appropriate.
# Some setups also need -e for batch mode without the GUI.
STATA="${STATA:-stata-mp -b do}"

# MATLAB launch. -nodisplay/-nosplash keep it headless.
MATLAB="${MATLAB:-matlab -nodisplay -nosplash -batch}"

# R launch.
RSCRIPT="${RSCRIPT:-Rscript}"

# Number of cores for parallelised CF scripts (defaults inside CF/*.R use
# Sys.getenv('CF_NCORES', 8)). Override here for a different machine.
export CF_NCORES="${CF_NCORES:-8}"

# Find repository root (the directory containing this script).
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="${SCRIPT_DIR}"
cd "${REPO_ROOT}"

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

FROM=0
TO=8
DRY_RUN=0
LIST=0

while [[ $# -gt 0 ]]; do
    case "$1" in
        --from)   FROM="$2"; shift 2 ;;
        --to)     TO="$2";   shift 2 ;;
        --only)   FROM="$2"; TO="$2"; shift 2 ;;
        --list)   LIST=1;    shift   ;;
        --dry-run) DRY_RUN=1; shift  ;;
        -h|--help)
            sed -n '2,40p' "$0" | sed 's/^# //;s/^#$//'
            exit 0
            ;;
        *) echo "Unknown argument: $1" >&2; exit 1 ;;
    esac
done

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

run() {
    # Echo and run (or just echo, under --dry-run).
    echo "  $*"
    if [[ "${DRY_RUN}" -eq 0 ]]; then
        eval "$@"
    fi
}

run_r() {   run "${RSCRIPT} $*"; }
run_stata() { run "${STATA} $*"; }
run_matlab() { run "${MATLAB} \"$*\""; }

banner() {
    local stage="$1"; shift
    echo
    echo "================================================================"
    echo "[stage ${stage}] $*"
    echo "================================================================"
}

stage_should_run() {
    local n="$1"
    [[ "${n}" -ge "${FROM}" && "${n}" -le "${TO}" ]]
}

# ---------------------------------------------------------------------------
# Stage list (for --list)
# ---------------------------------------------------------------------------

if [[ "${LIST}" -eq 1 ]]; then
    cat <<'EOF'
Stages:
  0  Setup (R + Stata package installs)
  1  Raw-data processing                            [HEAVY: numerator]
  2  Descriptive exhibits (Figures 1-2, Table 1, OA D/E/F/G)
  3  Demand estimation (MATLAB)                     [HEAVY]
  4  Restaurant marginal costs + Table 3a/b, in-text vartheta
  5  Restaurant fixed-cost estimation + Table 4
  6  Restaurant-cost bootstrap (SEs)                [HEAVY]
  7  Platform marginal costs + Table 5              [HEAVY]
  8  Counterfactuals + Tables 6-15, Figures 3-8     [HEAVY, HPC recommended]
EOF
    exit 0
fi

# ---------------------------------------------------------------------------
# STAGE 0 — Setup
# ---------------------------------------------------------------------------
# Idempotent dependency install. Skip if the working environment already has
# the R packages and the FoodDeliveryTools / EconTools local packages installed.

if stage_should_run 0; then
    banner 0 "Setup"
    run_r "code/0_setup.R"
    # Comment out the Stata setup if Stata is unavailable in your environment;
    # only the price-index estimation (stage 1) and the item-level DiD (stage 2)
    # depend on Stata-side packages.
    run_stata "code/0_setup.do"
    # Generate synthetic substitutes for the proprietary Numerator,
    # YipitData, Edison, InfoGroup, and SimpleMaps inputs (Option A in the
    # README). Skip this step if data/numerator/qsr_transactions_master.csv
    # already exists -- e.g., because the replicator has placed the real
    # proprietary files at the data/ paths (Option B). To force a fresh
    # synthetic draw, delete data/numerator/qsr_transactions_master.csv
    # before invoking run_all.sh.
    if [[ ! -f data/numerator/qsr_transactions_master.csv ]]; then
        run_r "code/generate_synthetic_data.R"
    else
        echo "  [stage 0] data/numerator/qsr_transactions_master.csv already exists; skipping synthetic-data generation."
    fi
fi

# ---------------------------------------------------------------------------
# STAGE 1 — Raw-data processing
# ---------------------------------------------------------------------------
# Produces the cleaned-and-merged source datasets in data/est_dat/,
# data/eqm_data/, data/geo/, data/yipitdata/, data/numerator/, etc. that the
# rest of the pipeline reads. The Numerator processing in particular is
# memory-heavy (8.1 GB transactions input).
#
# Many of these scripts are independent of each other; in production they could
# be parallelised. We invoke them serially here for transparency.

if stage_should_run 1; then
    banner 1 "Raw-data processing"

    # ACS demographics
    run_r "code/process_acs/process_acs.R"
    run_r "code/process_acs/nearby_demos_acs.R"

    # Geography (ZCTA / ZIP crosswalks, range maps)
    run_r "code/generate_geo/generate_geo.R"
    run_r "code/generate_geo/load_zcta_data.R"
    run_r "code/compute_pop_distribution/pop_dist_take2.R"
    run_r "code/zip_code_weights/construct_zip_code_weights.R"

    # Restaurant-side / InfoGroup
    run_r "code/process_infogroup/process_infogroup.R"

    # YipitData (restaurant listings panel) — sequenced as: add_brands ->
    # add_infogroup -> add_store_locations -> separate -> compute_subset ->
    # determine_top_chains -> produce_DiD_data.
    run_r "code/process_yipit/add_brands.R"
    run_r "code/process_yipit/add_infogroup.R"
    run_r "code/process_yipit/add_store_locations_w_infogroup.R"
    run_r "code/process_yipit/separate_data.R"
    run_r "code/process_yipit/compute_subset.R"
    run_r "code/process_yipit/determine_top_chains_and_cuisines.R"
    run_r "code/process_yipit/produce_DiD_data_v2.R"

    # Numerator (consumer panel) — combination of Python + R steps.
    # The Python scripts handle the heavy join across the *_table.csv files.
    # Ordering is dictated by file-level data flow:
    #   collapse_static -> static_users-combined.csv
    #   identify_static_trans -> qsr_baskets_*-combined.csv
    #   identify_static_trans-summary -> summary_baskets_*-combined.csv
    #   combine_fact_summary -> all_baskets_*-combined.csv
    #   add_category -> all_baskets_*-combined.rds
    #   static_event_dataset, produce_nonstatic_table, prepare_regression_data
    #     all consume the .rds outputs.
    run_r "code/process_numerator/collapse_static.R"
    run "python3 code/process_numerator/identify_static_trans.py"
    run "python3 code/process_numerator/identify_static_trans-summary.py"
    run_r "code/process_numerator/combine_fact_summary.R"
    run_r "code/process_numerator/combine_item_table.R"
    run_r "code/process_numerator/add_category.R"
    run_r "code/process_numerator/static_event_dataset.R"
    run_r "code/process_numerator/produce_nonstatic_table.R"
    run_r "code/process_numerator/prepare_regression_data.R"

    # Fee-cap legislation panel -> monthly_fee_caps.csv, zip_fee_caps.rds.
    # Both downstream-consumed by many other scripts.
    run_r "code/describe_fee_caps/monthly_fee_cap_data.R"
    run_r "code/describe_fee_caps/evolution_of_fee_caps_v2.R"

    # Restaurant adoption demographics
    run_r "code/restaurant_choice_demos/nearby_demographics.R"

    # Estimation-data assembly (zcta resto, month/zip-level merging, multi-month
    # combination). Mirrors code/prepare_est_data/run_all.R.
    run_r "code/prepare_est_data/zcta_mapping.R"
    run_r "code/prepare_est_data/zcta_resto_data.R"
    run_r "code/prepare_est_data/produce_month_zip_level.R"
    run_r "code/prepare_est_data/combine_data.R"
    run_r "code/prepare_est_data/check_contradiction.R"
    run_r "code/prepare_est_data/combine_months.R"

    # Menu-price indices (Numerator pricing chain) — produces the live
    # price_indices.csv consumed by Table 2, the CF chain, etc.
    # Step 5 (convert_to_dta.do) and step 6 (estimate_price_effects_disagg.do)
    # are the Stata parts; the rest are R.
    run_r "code/numerator_menu_pricing/prepare_menu_data.R"
    run_r "code/numerator_menu_pricing/prepare_geo_data.R"
    run_r "code/numerator_menu_pricing/prepare_item_data.R"
    run_r "code/numerator_menu_pricing/merge_item_level_data.R"
    run_stata "code/numerator_menu_pricing/convert_to_dta.do"
    run_stata "code/numerator_menu_pricing/estimate_price_effects_disagg.do"
    run_r "code/numerator_menu_pricing/generate_item_level_reg_table.R"
    run_r "code/numerator_menu_pricing/compute_price_indices.R"
fi

# ---------------------------------------------------------------------------
# STAGE 2 — Descriptive exhibits
# ---------------------------------------------------------------------------
# Figures 1-2, Table 1, and the descriptive OA sections (D, E, F, G).
# These don't depend on the structural estimates, so they can be run any time
# after stage 1.

if stage_should_run 2; then
    banner 2 "Descriptive exhibits"

    # Figure 2 — restaurant platform-portfolio distribution
    run_r "code/describe_portfolio_choice/describe_portfolio_choice.R"

    # OA Section E — Numerator panel validation.
    # Ordering: market_shares_by_CBSA produces shrs_Q*.csv consumed by
    # evaluate_numerator; alternative_market_shares produces
    # market_shares_2019.rds consumed by explore_yipit/prepare_DiD_data.R.
    run_r "code/explore_numerator/market_shares_by_CBSA.R"
    run_r "code/explore_numerator/alternative_market_shares.R"
    run_r "code/explore_numerator/evaluate_numerator.R"
    run_r "code/explore_numerator/representativeness.R"

    # Table 1 + OA D2 — Yipit DiD and price-structure descriptives
    run_r "code/explore_yipit/prepare_DiD_data.R"
    run_r "code/explore_yipit/demand_estimation_DiD.R"
    run_r "code/explore_yipit/process_demand_estimation_DiD.R"
    run_r "code/explore_yipit/yipit_price_structure.R"
    run_r "code/explore_yipit/restaurant_multihoming.R"
    run_r "code/explore_numerator/plot_market_shares.R"
    run_r "code/explore_numerator/excess_inertia.R"
    run_r "code/explore_numerator/assess_leftovers.R"
    run_stata "code/explore_numerator/assess_leftovers_2.do"
    run_r "code/explore_numerator/assess_leftovers_3.R"

    # OA Section F — web-harvested-platform descriptives (Tables F1, F2)
    run_r "code/web_harvest_descriptives/add_doordash_resto_info.R"
    run_r "code/web_harvest_descriptives/add_grubhub_resto_info.R"
    run_r "code/web_harvest_descriptives/add_postmates_resto_info.R"
    run_r "code/web_harvest_descriptives/add_uber_resto_info.R"
    run_r "code/web_harvest_descriptives/data_processing.R"
    run_r "code/web_harvest_descriptives/describe_prices.R"
    run_r "code/web_harvest_descriptives/descr_tabs.R"

    # OA Figure D1a-d — Numerator pricing scatter plots
    run_r "code/numerator_menu_pricing/describe_prices.R"

    # OA Section G — restaurant-price sample
    run_r "code/restaurant_price_sample/price_indices.R"

    # COVID descriptives + neutrality + Prop 22 (mixed exhibits)
    run_r "code/COVID/process_lockdown_data.R"
    run_r "code/COVID/analyze_covid.R"
    run_r "code/neutrality/compare_fixed_prop.R"
    # prop22 currently has no surviving scripts — left for documentation completeness.
fi

# ---------------------------------------------------------------------------
# STAGE 3 — Demand estimation (MATLAB)
# ---------------------------------------------------------------------------
# Heaviest single step in the pipeline — plan for hours, possibly days on the
# first pass. The 100-replicate bootstrap inside fe_estimation_boot is the
# longest tail. All MATLAB outputs are written to fixed paths (no spec
# timestamp), so downstream R stages read them automatically.
#
# See code/demand_estimation/readme.md for the full step-by-step breakdown.

if stage_should_run 3; then
    banner 3 "Demand estimation (MATLAB)"

    run_matlab "run('code/demand_estimation/run_pipeline.m')"
    # run_pipeline.m drives steps 1-5 (estimate_consumer_choice -> fe_estimation
    # -> compute_GMM_SE_v2 -> fe_estimation_boot -> SE_table). The R formatting
    # step is separate:
    run_r "code/demand_estimation/produce_table.R"
    # Table 2 + OA Tables K2/K3 source data:
    run_r "code/demand_estimation/postest_analysis.R"
    run_r "code/demand_estimation/sample_size_table.R"

    # Equilibrium-data assembly (must run after demand parameters are in place,
    # but before any cost recovery, since it builds the eqm_data*.rds files
    # that every downstream stage reads).
    run_r "code/prepare_eqm_data/process_data_for_eqm.R"
fi

# ---------------------------------------------------------------------------
# STAGE 4 — Restaurant marginal costs
# ---------------------------------------------------------------------------
# Recovers per-ZIP/portfolio restaurant marginal costs and the pricing-friction
# parameter vartheta. Produces Tables 3a/3b and the in-text vartheta estimate.

if stage_should_run 4; then
    banner 4 "Restaurant marginal costs"

    # Baseline recovery (writes retaurant_costs.rds)
    run_r "code/recover_restaurant_costs/recover_restaurant_costs_v2.R"
    # NPP variant used by OA Table H1
    run_r "code/recover_restaurant_costs/recover_restaurant_costs_NPP.R"

    # Tables 3a/3b
    run_r "code/recover_restaurant_costs/summarize_costs_v3.R"
    # In-text vartheta point estimate (and SE if stage 6 has already run)
    run_r "code/recover_restaurant_costs/produce_GMM_table.R"
    # OA Table H1
    run_r "code/recover_restaurant_costs/compare_pricing_models.R"
fi

# ---------------------------------------------------------------------------
# STAGE 5 — Restaurant fixed costs (Table 4)
# ---------------------------------------------------------------------------
# CCP-GMM estimation of the restaurant adoption fixed-cost parameters.
# Mirrors code/restaurant_FC_estimation/run_all.R.

if stage_should_run 5; then
    banner 5 "Restaurant fixed-cost estimation"

    run_r "code/restaurant_FC_estimation/estimate_CCPs.R"
    run_r "code/restaurant_FC_estimation/expected_profits_for_estimation.R"
    run_r "code/restaurant_FC_estimation/GMM_CPP_estimation_of_FCs.R"
    # Table 4 (kappas, sigmas) + cost-by-size plots
    run_r "code/restaurant_FC_estimation/GMM_CCP_table.R"
fi

# ---------------------------------------------------------------------------
# STAGE 6 — Restaurant-cost bootstrap (SEs)
# ---------------------------------------------------------------------------
# Bootstraps the restaurant-cost, fixed-cost, and friction-parameter estimates.
# This is the second-heaviest single step after stage 3.

if stage_should_run 6; then
    banner 6 "Restaurant-cost bootstrap"
    run_r "code/bootstrap_restaurant_costs/bootstrap_all_costs.R"
fi

# ---------------------------------------------------------------------------
# STAGE 7 — Platform marginal costs (Table 5)
# ---------------------------------------------------------------------------
# Inverts platforms' fee-setting FOCs county-by-county to recover platform
# marginal costs. The submarket-definition step must run before the inversion.

if stage_should_run 7; then
    banner 7 "Platform marginal costs"

    run_r "code/estimate_platform_costs/determine_submarkets.R"
    # Heavy: per-county FOC inversion. Default uses parallel::detectCores()-1.
    run_r "code/estimate_platform_costs/invert_FOCs_by_county.R"
    # Table 5
    run_r "code/estimate_platform_costs/describe_pMC.R"
fi

# ---------------------------------------------------------------------------
# STAGE 8 — Counterfactuals
# ---------------------------------------------------------------------------
# Heaviest stage overall. Splits into two phases:
#   8a. Equilibrium-solving CFs (baseline, NC, NM, cap, socopt, maxjoint,
#       monopolize, twosided). Each writes per-market .rds files into
#       output/CF_feefirst/spec_nsim50/.
#   8b. Analysis scripts that consume those .rds files to produce the
#       paper's CF exhibits (Tables 6-15, Figures 3-8, and OA K/B exhibits).
#
# Ordering within 8a:
#   - find_pricing_eqm.R must run first (writes baseline_*.rds; many other
#     scripts warm-start from it).
#   - run_all_markets_cap.R then writes cap30_*_take2.rds etc. (consumed by
#     two_sided_cap.R).
#   - The remaining solvers are mutually independent.
#
# CF_NCORES (exported above; default 8) controls the parallel-cluster size
# inside each solver.

if stage_should_run 8; then
    banner 8 "Counterfactuals — equilibrium solves"

    # 8a — solvers
    # Bertrand-Nash baseline + NC + NM
    run_r "code/CF/find_pricing_eqm.R"
    # Commission-cap sweep (cap.levels = 40:15)
    run_r "code/CF/run_all_markets_cap.R"
    # Social-optimum fees
    run_r "code/CF/run_all_markets_socopt.R"
    # Joint-profit maximisation (+ NC / NM variants)
    run_r "code/CF/find_pricing_eqm_maxjoint.R"
    run_r "code/CF/find_pricing_eqm_maxjointNC.R"
    run_r "code/CF/find_pricing_eqm_maxjointNM.R"
    # Monopolisation counterfactuals (sub1000_ prefix)
    run_r "code/CF/monopolize.R"
    run_r "code/CF/monopolize_socopt.R"
    # Two-sided cap (depends on cap30_*_take2.rds from run_all_markets_cap.R)
    run_r "code/CF/two_sided_cap.R"

    banner 8 "Counterfactuals — analysis exhibits"

    # 8b — exhibit-producing scripts
    # Figures 3, 4a-c; Tables 11, 12, 13
    run_r "code/CF/analyze_vary_cap.R"
    # Figure 5
    run_r "code/CF/variety_vs_fixed_costs.R"
    # Figures 6a, 6b
    run_r "code/CF/relative_change_plot_take2.R"
    # Figures 7, 8a-c
    run_r "code/CF/analyze_twosided.R"
    # Tables 6, 8a, 8b; OA Tables K4, K5
    run_r "code/CF/compare_priv_soc.R"
    # Table 7; OA Figure B1
    run_r "code/CF/compute_distortions.R"
    # Table 9
    run_r "code/CF/variety_vs_fixed_costs_soc_priv.R"
    # Table 10
    run_r "code/CF/decompose_rpi_soc_fees.R"
    # Tables 14a, 14b, 15
    run_r "code/CF/analyze_max_joint.R"
    # OA Tables J1a, J1b
    run_r "code/CF/explain_optimal_fees_version2.R"
    # OA Table K1; OA Figure K1
    run_r "code/CF/differential_sales_impacts.R"
    # OA Figure K2
    run_r "code/CF/restaurant_side_distortions.R"
fi

echo
echo "================================================================"
echo "Done. Stages ${FROM}..${TO}."
echo "================================================================"
