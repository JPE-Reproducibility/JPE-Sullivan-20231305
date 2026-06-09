# Run the restaurant fixed-cost estimation pipeline that produces Table 4.
#
#   estimate_CCPs.R                  → output/restaurant_FC_estimation/CCPs/CCP_outputs.rds
#   expected_profits_for_estimation  → output/restaurant_FC_estimation/EPi_CCPs_continuum*.rds
#   GMM_CPP_estimation_of_FCs.R      → output/restaurant_FC_estimation/restaurant_cost*_GMM-ccp.rds
#   GMM_CCP_table.R                  → Tables 4a, 4b, 4c (sigmas/kappas CSVs and cost_by_size_pool*.pdf)
source('code/restaurant_FC_estimation/estimate_CCPs.R')
source('code/restaurant_FC_estimation/expected_profits_for_estimation.R')
source('code/restaurant_FC_estimation/GMM_CPP_estimation_of_FCs.R')
source('code/restaurant_FC_estimation/GMM_CCP_table.R')
