% GMM estimation of the structural consumer-choice model via a Berry-style
% log-share contraction. Combines fixed-point updates on the additive
% parameters (psi, mu_eta, lambda, np, phi_chain, logvar) with a Newton
% step on alpha using the DiD fee-effect moment. Writes
% data/demand_estimation/est_results-yipitdata.mat, consumed by
% fe_estimation.m, fe_estimation_boot.m, and compute_GMM_SE_v2.m.

addpath('auxiliary_functions');

rng(1);

%== Specification options ==%
spec_opt = LoadSpecOpts();
spec_opt.rest_het      = 1;     % 1 = chain vs independent restaurant types
spec_opt.rest_RC       = 0;     % random coefficients on restaurant types
spec_opt.match_chain   = 1;     % match chain/indep shares in the objective
spec_opt.market_subset = true;
spec_opt.n_subset      = 5;     % number of metros included in structural estimation
spec_opt.demean        = false;
spec_opt.use_simple    = false; % use ComputeChoiceProbs_faster (not _simple)
spec_opt.sample_rate   = 1;

% Menu price type
% = 1 hedonic
% = 2 price sample indices
% = 3 difference-in-difference prices
% = 4 second resubmission (no chain/indep distinction)
spec_opt.menu_price_type = 4;
spec_opt.static     = 1;        % static sample
spec_opt.connect    = true;
spec_opt.lambda_het = 0;
spec_opt.lambda_inc = 1;
spec_opt.T_i        = 10;
spec_opt.sset       = true;
spec_opt.het_alpha  = false;
spec_opt.het_extra  = false;
% Use importance sampling to integrate over the unobserved heterogeneity Xi
% when computing simulated choice probabilities. The IS proposal and weights
% are implemented in auxiliary_functions/DrawXi_IS_v2.m and reduce simulation
% variance relative to naive Monte Carlo at the same nsim.
spec_opt.IS         = true;
spec_opt.use_WT     = false;
spec_opt.sigma_eps  = 0.33;

%== Numerical options ==%
num_opt      = LoadNumOpt();
num_opt.nsim = 50;

price_version = '_v3';

%== I/O paths ==%
inpath     = DetermineDataPath(spec_opt, price_version);
inpath_DiD = '../../output/explore_yipit/DiD_for_demand_estimation.csv';

outdir = '../../data/demand_estimation';
if ~exist(outdir, 'dir')
    mkdir(outdir);
end
outpath = sprintf('%s/est_results-yipitdata.mat', outdir);

%== Load data ==%
[dat, month_tab, n_cbsa] = LoadData(inpath, spec_opt);
DiD = readtable(inpath_DiD);

% Aggregate per-platform order counts
dat.f_in = dat.f0 + dat.f1 + dat.f2 + dat.f3 + dat.f4;
dat.f_on = dat.f_in - dat.f0;

data_objs = GenerateDataObjs(dat, num_opt, spec_opt, DiD);

% Data moments
[gd_a, moment_names, ~] = ComputeMoments_v3(data_objs, spec_opt);

% Initial parameter vector
n_platforms = 4;
[~, ~, theta_names] = ConstructParam(spec_opt, n_cbsa, n_platforms, data_objs.n_demo);

% Suitable starting values as determined by previous runs.
nmarkets = size(data_objs.CBSA_mat, 2);
theta_0 = [-0.50*ones(nmarkets, 1);   % psi_*-1
           -0.60*ones(nmarkets, 1);   % psi_*-2
           -1.00*ones(nmarkets, 1);   % psi_*-3
           -1.25*ones(nmarkets, 1);   % psi_*-4
            0.5326;                   % lambda_1
           -0.2830;                   % lambda_2
            0.1682;                   % lambda_3
            0.1282;                   % alpha
           -0.3290;                   % logvar_zeta1
           -0.5261;                   % logvar_zeta2
           -7.0*ones(nmarkets, 1);    % mu_eta_*
            2.0296;                   % logvar_eta
           -0.5055;                   % np_young
            0.1648;                   % np_married
           -0.0925;                   % np_highinc
            1.1432];                  % phi_chain

%== Specify which moments and parameters enter the contraction ==%
n_subset  = spec_opt.n_subset;
platforms = {'direct', 'dd', 'uber', 'gh', 'pm'};

% Moments per subset (5 platform counts each), plus demographic covariances,
% chain diff, and three logvar-scale moments
match_moments = cell(0, 1);
for s = 1:n_subset
    for p = 1:numel(platforms)
        match_moments{end + 1, 1} = sprintf('n_%s_%d', platforms{p}, s);
    end
end
match_moments = [match_moments;
                 {'cov_direct_young';  'cov_online_young';
                  'cov_direct_married';'cov_online_married';
                  'cov_direct_hinc';   'cov_online_hinc';
                  'diff_chain';
                  'share_no_online';
                  'mean_n_platforms';
                  'share_heavy_orderer'}];

% Parameters per subset (mu_eta_s, then psi_s-1..psi_s-4), plus demographic
% interactions, chain, and three logvar parameters
match_theta = cell(0, 1);
for s = 1:n_subset
    match_theta{end + 1, 1} = sprintf('mu_eta_%d', s);
    for k = 1:4
        match_theta{end + 1, 1} = sprintf('psi_%d-%d', s, k);
    end
end
match_theta = [match_theta;
               {'np_young';   'lambda_1';
                'np_married'; 'lambda_2';
                'np_highinc'; 'lambda_3';
                'phi_chain';
                'logvar_zeta1'; 'logvar_zeta2'; 'logvar_eta'}];

% Map labels to indices
moment_sub = zeros(1, length(match_moments));
theta_sub  = zeros(1, length(match_theta));
for k = 1:length(match_moments)
    moment_sub(k) = find(strcmp(match_moments{k}, moment_names));
    theta_sub(k)  = find(strcmp(match_theta{k},   theta_names));
end

idx_alpha = find(strcmp(theta_names,   'alpha'));
idx_did   = find(strcmp(moment_names,  'fee_did'));

%== Berry-style contraction settings ==%
nrounds           = 1e3;
uprate_base       = 0.33;
uprate_scale      = 0.05;
uprate_alpha      = 0.05;
tol               = 0.01;
target_diff_alpha = 1e-5;
niter_print       = 5;

% Update rate vector: slow updates for the 3 logvar-scale moments.
% Slower rate aides with convergence for these parameters.
uprate              = uprate_base * ones(length(match_moments), 1);
uprate(end - 2:end) = uprate_scale;

% D aggregates the 5 platform-count moments within each subset into one
% row, so that the mu_eta_s parameters update against the subset's total
% restaurant sales.
D = eye(length(moment_sub));
for k = 1:n_subset
    r_idx           = 1 + 5*(k - 1);
    c_idx           = r_idx:(r_idx + 4);
    D(r_idx, c_idx) = 1;
end

%== Berry contraction iteration ==%
berry_verbose = true;
for iter = 1:nrounds
    gs_a = ComputeSimMoments(theta_0, theta_names, data_objs, spec_opt);

    shrs_dat  = D * gd_a(moment_sub);
    shrs_pred = D * gs_a(moment_sub);
    diff      = log(shrs_dat) - log(shrs_pred);
    % Platform/metro pairs with zero observed share give an infinite log
    % difference; zero them out so they do not contribute to the update.
    diff(abs(diff) == Inf) = 0;

    % Newton update on alpha using the DiD moment
    diff_alpha = gs_a(idx_did) - gd_a(idx_did);
    if abs(diff_alpha) > target_diff_alpha
        theta_p            = theta_0;
        stepsize_alpha     = max(1e-6, 1e-2*abs(theta_0(idx_alpha)));
        theta_p(idx_alpha) = theta_p(idx_alpha) + stepsize_alpha;
        gs_p               = ComputeSimMoments(theta_p, theta_names, data_objs, spec_opt);
        diff_p             = gs_p(idx_did) - gd_a(idx_did);
        deriv_alpha        = (diff_p - diff_alpha) / stepsize_alpha;
        alpha_update       = -diff_alpha / deriv_alpha;
    else
        alpha_update = 0;
    end

    % Apply updates
    theta_0(theta_sub) = theta_0(theta_sub) + uprate .* diff;
    theta_0(idx_alpha) = theta_0(idx_alpha) + uprate_alpha * alpha_update;

    dist = mean(abs([diff; diff_alpha]));
    if berry_verbose && mod(iter, niter_print) == 0
        fprintf('Berry iteration %d; distance = %f\n', iter, dist);
    end

    if dist < tol
        fprintf('Converged at iter %d\n', iter);
        break
    end
end

% Re-evaluate the residuals at the final (saved) theta_0 so that the
% convergence_dist field matches the saved theta_vec. The inner-loop `dist`
% above is the pre-update residual; the saved theta_0 is post-update.
gs_final       = ComputeSimMoments(theta_0, theta_names, data_objs, spec_opt);
shrs_pred_fin  = D * gs_final(moment_sub);
diff_final     = log(shrs_dat) - log(shrs_pred_fin);
diff_final(abs(diff_final) == Inf) = 0;
diff_alpha_fin = gs_final(idx_did) - gd_a(idx_did);
final_dist     = mean(abs([diff_final; diff_alpha_fin]));

did_converge = isfinite(final_dist) && (final_dist < tol);
if ~did_converge
    warning(['estimate_consumer_choice did NOT converge: ' ...
             'final_dist = %g (tol = %g). The saved theta_vec may not be at a minimum.'], ...
            final_dist, tol);
end

%== Save results ==%
results                  = struct();
results.theta_vec        = theta_0;
results.theta_names      = theta_names;
results.convergence_dist = final_dist;
results.did_converge     = did_converge;
results.data_objs        = data_objs;
results.spec_opt         = spec_opt;
results.num_opt          = num_opt;

save(outpath, 'results');
fprintf('Saved estimates to %s\n', outpath);
