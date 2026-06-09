% Estimate the per-market platform fixed effects (psi) and mu_eta on all
% metros, given the structural parameter estimates from
% estimate_consumer_choice.m (which estimates the model on a subset of
% metros). Loops over all metros via RunFE -> estimate_cbsa_FEs.
% Writes:
%   output/demand_estimation/Psi_results.mat
%   output/demand_estimation/est_table-yipitdata.csv

addpath('auxiliary_functions');

rng(1);


% I/O paths
data_dir   = '../../data/demand_estimation';
output_dir = '../../output/demand_estimation';
if ~exist(output_dir, 'dir')
    mkdir(output_dir);
end
inpath_est  = sprintf('%s/est_results-yipitdata.mat', data_dir);
outpath_mat = sprintf('%s/Psi_results.mat', output_dir);
outpath_tab = sprintf('%s/est_table-yipitdata.csv', output_dir);

% Load structural estimates from step (i)
est       = load(inpath_est);
est       = est.results;
spec_opt  = est.spec_opt;
data_objs = est.data_objs;

% Underlying data path (derived from spec_opt + price_version)
price_version = '_v3';
inpath_dat    = DetermineDataPath(spec_opt, price_version);

% Numerical options
num_opt      = LoadNumOpt();
num_opt.nsim = 200;

% Non-bootstrap run: b = 0 disables the bootstrap branch in RunFE
b           = 0;
outdir_boot = '';
Z           = [];

[Psi, est_tab, MuEta] = RunFE(b, inpath_dat, outdir_boot, ...
                              num_opt, Z, spec_opt);

% Save: .mat for downstream use; .csv for the SE table to read
save(outpath_mat, 'Psi', 'MuEta');
writetable(est_tab, outpath_tab, 'WriteRowNames', false);
