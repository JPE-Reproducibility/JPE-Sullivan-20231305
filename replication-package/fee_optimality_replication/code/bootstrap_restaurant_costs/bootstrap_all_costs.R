# Run the restaurant-cost bootstrap pipeline that supplies SEs to Tables 3 and 4.
# Each step writes B replicates tagged 1..B (B = NBOOT = 100 in boot_utils.R)
# into data/bootstrap/dat/<subdir>/.
#
# PREREQUISITE: data/bootstrap/dat/Psi/est_table_<b>.csv must already
# be present. These are produced by the demand-estimation MATLAB FE bootstrap
# (code/demand_estimation/fe_estimation_boot.m) and supply the bootstrapped
# demand parameters that bootstrap_EPi.R, bootstrap_fc.R and bootstrap_friction.R
# iterate over. Run that MATLAB script first; bootstrap_*.R will stop with a
# clear error if Psi/ is empty.
#
# Pipeline:
#   bootstrap_ccp.R       → data/bootstrap/dat*/CCP/CCP_outputs<b>.rds
#   bootstrap_friction.R  → data/bootstrap/dat*/restoMC/restaurant_price_friction_<b>.rds → Table 3 SEs
#   bootstrap_EPi.R       → data/bootstrap/dat*/EPi/EPi_<b>.rds                              (uses CCP/, restoMC/)
#   bootstrap_fc.R        → data/bootstrap/dat*/restoFC/restaurant_FC_<b>.rds               (uses EPi/) → Table 4 SEs
#
# Parallelism: each step uses parallel::parLapply with ncores from the
# BOOTSTRAP_NCORES environment variable (default: detectCores() - 1).
source('code/bootstrap_restaurant_costs/bootstrap_ccp.R')
source('code/bootstrap_restaurant_costs/bootstrap_friction.R')
source('code/bootstrap_restaurant_costs/bootstrap_EPi.R')
source('code/bootstrap_restaurant_costs/bootstrap_fc.R')
