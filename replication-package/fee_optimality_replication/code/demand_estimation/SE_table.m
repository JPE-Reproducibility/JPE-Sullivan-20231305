% Produce a table that includes the standard errors of parameter estimates.
% Also save the asymptotic variance matrix.

% Specify directories containing results and data
base_dir   = '../../data/demand_estimation';
output_dir = '../../output/demand_estimation';
    

% Specify paths
inpath_est = sprintf('%s/est_results-yipitdata.mat', base_dir);
inpath_inference = sprintf('%s/inference.mat', base_dir);

outpath = sprintf('%s/est_SE_table-yipitdata.csv', output_dir);

% Load parameter estimates
results = load(inpath_est);
results = results.results;

Inference = load(inpath_inference);
Inference = Inference.outputs;

est = results.theta_vec;
theta_names = results.theta_names;

incl_np   = any(strcmp(theta_names, 'mu_eta'));
demo_np   = any(strcmp(theta_names, 'np_young'));
het_alpha = any(strcmp(theta_names, 'alpha_low'));
idio_eta  = any(strcmp(theta_names, 'logvar_idio'));
use_WT    = any(strcmp(theta_names, 'tau'));


% Different application of the delta method
logvar_idx = arrayfun(@(x) regexp(x, '^logvar'), theta_names);
logvar_idx = arrayfun(@(k) numel(logvar_idx{k}) > 0, (1:length(logvar_idx))');
logvar_idx = find(logvar_idx);

if idio_eta
    idio_idx = find(strcmp('logvar_idio', theta_names));
    logvar_idx = setdiff(logvar_idx, idio_idx);
end

est = reshape(est, numel(est), 1);

est(logvar_idx) = sqrt(exp(est(logvar_idx)));
if idio_eta
    est(idio_idx) = sqrt(exp(est(idio_idx)));
end

% Extract SEs
SE = Inference.SE;

% Delta method
SE(logvar_idx) = 0.5*est(logvar_idx).*SE(logvar_idx);

% Change the parameter names
theta_names = arrayfun(@(x) regexprep(x, 'logvar', 'sd'), theta_names);

% Table 
tab = table(theta_names, est, SE);

% Save table
writetable(tab, outpath);
