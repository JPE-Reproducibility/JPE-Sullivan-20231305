## Demand estimation readme

### Running the pipeline

The fastest path is to launch the full matlab chain via the orchestrator:

```
matlab -nodisplay -nosplash -batch "run_pipeline"
Rscript code/demand_estimation/produce_table.R
```

All outputs are written to fixed paths (no spec-timestamp suffixes), so
downstream steps pick up the latest run automatically.

### Primary estimation sequence
1. `estimate_consumer_choice.m`: GMM on the structural parameters. Matches
   simulated to data moments via a Berry-style log-share contraction with a
   Newton step on the fee-sensitivity parameter. Writes
   `data/demand_estimation/est_results-yipitdata.mat`.
2. `fe_estimation.m`: estimate the per-market platform fixed effects (psi)
   and mu_eta on **all** metros given the structural θ from step 1. Writes
   `output/demand_estimation/Psi_results.mat` and
   `output/demand_estimation/est_table-yipitdata.csv`.
3. `compute_GMM_SE_v2.m`: compute GMM standard errors at the point estimate
   via the exactly-identified sandwich `G^{-1} Ω G^{-1}'`. Writes
   `data/demand_estimation/inference.mat`, consumed by both
   `fe_estimation_boot.m` (step 4) and `SE_table.m` (step 5).
4. `fe_estimation_boot.m`: 100 bootstrap replicates of step 2. Perturbs the
   structural θ by `Z ~ N(0, AVar/N)` (using the variance from step 3) and
   re-runs the per-market FE estimation. Writes per-replicate files under
   `data/bootstrap/dat/Psi/`. Always starts from replicate 1 (overwriting
   previous results in that directory).
5. `SE_table.m`: combine the step-1 estimates with the step-3 SEs into a
   single table. Writes `output/demand_estimation/est_SE_table-yipitdata.csv`.
6. `produce_table.R`: format the point-estimate + SE table for the paper
   (**Table 2**). Writes
   `output/demand_estimation/est_SE-yipitdata_formatted.csv`.

### Configuration

- `spec_opt.n_subset` in `estimate_consumer_choice.m` controls how many metros
  enter the structural estimation. The remaining metros' FEs are filled in
  by step 2.

### Other post-estimation scripts

- `postest_analysis.R`: per-market and cross-market post-estimation analysis.
  Produces **OA Table K2** (`net_ext_average.csv`, network elasticities) and
  **OA Table K3** (`div_average.csv`, between-platform diversion ratios),
  plus per-market elasticities and diversion ratios consumed by
  `cannibalization.R` and downstream `code/CF/` scripts.
- `cannibalization.R`: share of restaurants' direct sales cannibalized by
  platform adoption. Writes
  `output/demand_estimation/postest_analysis/cannibalization.csv`,
  consumed by several `code/CF/` exhibit-producing scripts.
- `evaluate_fit.R`: model-fit diagnostics (market-share fits, $\rho$ comparisons,
  YipitData comparison). Part of the post-estimation sequence documented in
  `replication/README.md`.
- `sample_size_table.R`: estimation-sample descriptive statistics. Writes
  `output/demand_estimation/sample_size.csv` (consumed by
  `code/restaurant_price_sample/price_indices.R` for OA Table G1) and
  `T_i_quantiles.csv`.
