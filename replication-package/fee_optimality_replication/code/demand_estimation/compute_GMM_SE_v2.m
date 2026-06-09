% Compute GMM standard errors for the demand estimator.
% Writes data/demand_estimation/inference.mat, consumed by
% SE_table.m and fe_estimation_boot.m.
addpath('auxiliary_functions');

rng(1);


% Specify paths
est_dir = '../../data/demand_estimation';
inpath_est = sprintf('%s/est_results-yipitdata.mat', est_dir);
outpath    = sprintf('%s/inference.mat', est_dir);

est         = load(inpath_est);
est         = est.results;
theta_vec   = est.theta_vec;
theta_names = est.theta_names;
data_objs   = est.data_objs;
spec_opt    = est.spec_opt;

% DiD-based first-stage variance for the fee_did moment (scale by 15%
% commission change to match the units used in estimation).
DiD     = data_objs.DiD;
did_se  = 0.15*DiD.sales(2);
did_var = did_se^2;

% Sample moments in the data (use v3 to match the estimator;
% estimate_consumer_choice.m also uses ComputeMoments_v3).
[gd_a, moment_names, gd_i] = ComputeMoments_v3(data_objs, spec_opt);

% Simulated moments at the point estimate
[prob_f, chain_probs] = ...
    ComputeChoiceProbs_faster(theta_vec, theta_names, data_objs, spec_opt);
[gs_a, ~, gs_i] = ComputeMoments_v3(data_objs, spec_opt, prob_f, chain_probs, ...
                                    theta_vec, theta_names);

% Difference between simulated and data moments (impute mean for any missing
% values in the last column).
diff_i = gs_i - gd_i;
is_missing = isnan(diff_i(:, end));
diff_i(is_missing, end) = mean(diff_i(~is_missing, end));

N = size(diff_i, 1);

% Moment variance matrix with DiD correction
Omega = cov(diff_i);
idx_did = strcmp(moment_names, 'fee_did');
Omega(idx_did, idx_did) = Omega(idx_did, idx_did) + did_var*N;

% Numerical Jacobian of the moments w.r.t. theta. Forward differences with
% step size max(1e-6, 1e-2*|theta_d|) -- matches the step used by the
% estimator's alpha Newton update in estimate_consumer_choice.m. Common
% random numbers are implicit: data_objs carries pre-generated simulation
% draws (set in GenerateDataObjs via rng(1)) that are reused across calls.
n_param = length(theta_vec);
G = zeros(n_param, n_param);
for d = 1:n_param
    theta_p    = theta_vec;
    stepsize_d = max(1e-6, 1e-2*abs(theta_vec(d)));
    theta_p(d) = theta_p(d) + stepsize_d;
    g1         = ComputeSimMoments(theta_p, theta_names, data_objs, spec_opt);
    G(:, d)    = (g1 - gs_a)/stepsize_d;
end

% Asymptotic variance and standard errors
AVar = G^(-1)*Omega*(G^(-1))';
SE   = sqrt(diag(AVar))/sqrt(N);

param_tab = table(theta_names, theta_vec, SE);

outputs           = struct();
outputs.param_tab = param_tab;
outputs.AVar      = AVar;
outputs.SE        = SE;
outputs.N         = N;

save(outpath, 'outputs');
