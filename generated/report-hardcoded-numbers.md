## Potentially Hardcoded Numeric Constants


We found the following set of hard coded numbers. This may be completely legitimate (parameter input, thresholds for computations, etc), and is hence only for information.

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/restaurant_choice_demos/nearby_demographics.R**

- Line 49, : kilos.per.mile <- 1.60934

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/CF/find_pricing_eqm_maxjointNM.R**

- Line 64, : num.param$tol.R <- 0.005

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/FoodDeliveryTools/R/menu_price_eqm_NPP.R**

- Line 305, : pm.small <- s.0[pm.idx]/sum(s.0) < 0.005

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/CF/run_all_markets_cap.R**

- Line 55, : num.param$tol.R <- 0.005

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/CF/find_pricing_eqm_maxjoint.R**

- Line 112, : num.param$tol.R <- 0.005

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/FoodDeliveryTools/R/load_num_param.R**

- Line 8, : tol.FP          = 0.0001,
- Line 20, : tol.pc.eqm      = 0.005,

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/CF/monopolize_socopt.R**

- Line 72, : num.param$tol.R <- 0.005

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/recover_restaurant_costs/produce_GMM_table.R**

- Line 41, : quantile(phi.results, c(0.025, 0.975)),

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/generate_geo/generate_geo.R**

- Line 44, : latitude = 33.0192, longitude = -96.7029, fips = '48085')
- Line 48, : latitude = 21.3524, longitude = -157.8838, fips = '15003')
- Line 52, : latitude = 21.3100, longitude = -157.8600, fips = '15003')
- Line 56, : latitude = 34.6400, longitude = -120.4600, fips = '06083')
- Line 60, : latitude = 37.7400, longitude = -122.3800, fips = '06075')
- Line 64, : latitude = 45.5097, longitude = -122.8799, fips = '41067')

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/CF/monopolize.R**

- Line 72, : num.param$tol.R <- 0.005

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/CF/find_pricing_eqm_maxjointNC.R**

- Line 64, : num.param$tol.R <- 0.005

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/demand_estimation/sample_size_table.R**

- Line 49, : qtiles <- c(0.5, 0.9, 0.95, 0.96, 0.97, 0.98, 0.99, 0.999)

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/FoodDeliveryTools/R/draw_IS.R**

- Line 18, : phi <- function(Xi, demand.param, base.shr = 0.025, sigma.U = 3){

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/FoodDeliveryTools/R/estimate_restaurant_FCs_v2.R**

- Line 3, : sigma.start = c(0.006, 0.003)){
- Line 46, : control = list(maxit = 1000, abstol = 0.0005))

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/generate_synthetic_data.R**

- Line 260, : rows[, latitude  := as.numeric(lat) + rnorm(.N, 0, 0.005)]
- Line 261, : rows[, longitude := as.numeric(lng) + rnorm(.N, 0, 0.005)]
- Line 398, : #     other) sit on a 4x4 grid with ~0.0035 deg spacing (>= ~260 m apart),
- Line 422, : anchors[, latitude  := as.numeric(lat) + 0.010 + 0.0035 * ((combo - 1L) %%  4L)]
- Line 423, : anchors[, longitude := as.numeric(lng) + 0.010 + 0.0035 * ((combo - 1L) %/% 4L)]
- Line 798, : qsr[, BASKET_TOTAL     := round(BASKET_SUB_TOTAL * 1.085, 2)]  # tax/tip wedge

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/FoodDeliveryTools/R/menu_price_eqm.R**

- Line 292, : pm.small <- s.0[pm.idx]/sum(s.0) < 0.005

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/CF/find_pricing_eqm.R**

- Line 129, : num.param$tol.R <- 0.005

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/CF/restaurant_side_distortions.R**

- Line 266, : C.grid <- seq(from = 0, to = 0.50, by = 0.005)

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/prepare_est_data/zcta_mapping.R**

- Line 7, : kilos.per.mile <- 1.60934

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/prepare_est_data/zcta_resto_data.R**

- Line 43, : kilos.per.mile <- 1.60934

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/demand_estimation/auxiliary_functions/ConstructParam.m**

- Line 7, : psi =  [ 0.21626,  0.49264,  0.46242, 0.51653;
- Line 8, : -0.00424,  0.51957, -0.03856, 0.76359;
- Line 9, : 0.098486, 0.27049, -0.00291, 0.58743];
- Line 12, : psi_extra = [-2.1489, -1.9795, -3.3118, -3.0544];
- Line 18, : alpha        = 0.23894;
- Line 19, : logvar_zeta1 = 0.48301;
- Line 20, : logvar_zeta2 = -0.3895;
- Line 23, : lambda = [ 0.49494,  0.55468, 0.23073,  0.40533 ;
- Line 24, : -1.4366,   -1.5337, -1.3026, -1.9138];
- Line 26, : lambda = [lambda; -0.90376, -0.9349, -0.90374, -1.544];
- Line 29, : lambda = [0.64289; -0.43665];
- Line 36, : psi = [-3.5511, -5.5626, -9.4589, -5.6455;
- Line 37, : -0.8039,  1.0801, -3.2735, -3.2946;
- Line 38, : -4.6695, -5.9107, -6.5289, -4.8682;
- Line 39, : -3.9268, -6.0805, -8.0064, -7.1855;
- Line 40, : -3.0681, -5.3217, -8.0235, -4.5060;
- Line 41, : -5.1650, -6.7142, -7.7784, -3.6553;
- Line 42, : -4.3613, -4.8389, -9.8597, -3.2240;
- Line 43, : -3.3714, -3.1286, -4.4544, -5.0403;
- Line 44, : -2.2680, -4.7202, -4.8687, -4.7116;
- Line 45, : -5.3211, -6.2714, -7.9112, -3.6454;
- Line 46, : -5.1048, -7.4422, -5.6220, -5.7840;
- Line 47, : 0.0412, -2.7932, -4.5296, -4.1043;
- Line 48, : -0.8070, -2.4211, -6.4665, -0.9486;
- Line 49, : -3.3087, -3.1233, -7.4172, -4.8354];
- Line 51, : alpha        = 0.7440;
- Line 52, : logvar_zeta1 = 4.0227;
- Line 53, : logvar_zeta2 = 2.7138;
- Line 56, : lambda = [  1.1907,  1.0593,  0.70206,  0.89379;
- Line 57, : -0.87078, -1.0721,  -0.6349, -1.98440];
- Line 62, : lambda = [1.907; -0.87078];
- Line 73, : mu_eta = [-4.9651;
- Line 74, : -5.0458;
- Line 75, : -3.9829];
- Line 78, : mu_eta = [mu_eta; repmat(-3.8166, spec_opt.n_subset - 3, 1)];
- Line 86, : logvar_eta = 1.4028;
- Line 89, : lambda_np = [-0.69957; ...
- Line 90, : -2.4594];
- Line 92, : lambda_np = [lambda_np; -3.2922];
- Line 97, : logvar_idio = 5.1883;
- Line 102, : alpha_coef = [ -0.011914; -0.11887; -0.14727];
- Line 104, : alpha_low = 0.0013257;
- Line 108, : nu_bar = 3.7994;
- Line 112, : tau = 0.53349;
- Line 116, : phi_chain = 0.89491; %-0.83685;
- Line 120, : logvar_phi =  -0.27226;

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/CF/run_all_markets_socopt.R**

- Line 113, : num.param$tol.R <- 0.005

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/restaurant_FC_estimation/GMM_CCP_table.R**

- Line 214, : z <- qnorm(0.975)
- Line 261, : z <- qnorm(0.975)

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/FoodDeliveryTools/R/compute_eqm_feefirst.R**

- Line 93, : num.param$tol.R <- 0.005
- Line 145, : num.param$tol.FP <- 0.001
- Line 255, : h.r <- 0.00025
- Line 311, : if (pm.shr < 0.005){

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/restaurant_FC_estimation/GMM_CPP_estimation_of_FCs.R**

- Line 28, : sigma.start <- c(0.006, 0.003)

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/demand_estimation/estimate_consumer_choice.m**

- Line 84, : 0.5326;                   % lambda_1
- Line 85, : -0.2830;                   % lambda_2
- Line 86, : 0.1682;                   % lambda_3
- Line 87, : 0.1282;                   % alpha
- Line 88, : -0.3290;                   % logvar_zeta1
- Line 89, : -0.5261;                   % logvar_zeta2
- Line 91, : 2.0296;                   % logvar_eta
- Line 92, : -0.5055;                   % np_young
- Line 93, : 0.1648;                   % np_married
- Line 94, : -0.0925;                   % np_highinc
- Line 95, : 1.1432];                  % phi_chain

**/Users/florianoswald/actions-runner/_work/JPE-Sullivan-20231305/JPE-Sullivan-20231305/replication-package/fee_optimality_replication/code/process_acs/process_acs.R**

- Line 158, : inc$share_low_inc <- suppressWarnings(as.numeric(inc$ShareLess.than..10.000)  +
- Line 159, : as.numeric(inc$Share.10.000.to..14.999) +
- Line 160, : as.numeric(inc$Share.15.000.to..24.999) +
- Line 161, : as.numeric(inc$Share.25.000.to..34.999) +
- Line 162, : (1/3)*as.numeric(inc$Share.35.000.to..49.999))

