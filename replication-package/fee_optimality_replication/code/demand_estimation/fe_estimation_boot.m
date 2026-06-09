% Bootstrap the per-market platform fixed-effect estimates from
% fe_estimation.m to obtain their asymptotic variance.
%
% For each replicate b:
%   1. Draw Z ~ N(0, V) where V = AVar/N from compute_GMM_SE_v2.m.
%   2. Perturb the structural estimate: theta_b = theta_hat + Z.
%   3. Re-estimate the market-level FEs at theta_b via RunFE.
%
% Writes per-replicate Psi/est_table files under
% data/bootstrap/dat/Psi/.  If the directory already exists,
% the script resumes from the highest existing replicate number.

addpath('auxiliary_functions');

rng(1);

% Number of bootstrap replicates per batch
NB = 100;


% Input paths
data_dir   = '../../data/demand_estimation';
inpath_est = sprintf('%s/est_results-yipitdata.mat', data_dir);
inpath_var = sprintf('%s/inference.mat', data_dir);

% Output directory (always overwrites replicates 1..NB on each run)
outdir_boot = '../../data/bootstrap';
if ~exist(outdir_boot, 'dir'); mkdir(outdir_boot); end
outdir_boot = sprintf('%s/dat', outdir_boot);
if ~exist(outdir_boot, 'dir'); mkdir(outdir_boot); end
outdir_boot = sprintf('%s/Psi', outdir_boot);
if ~exist(outdir_boot, 'dir'); mkdir(outdir_boot); end

% Load structural estimates and variance matrix
est      = load(inpath_est);
est      = est.results;
spec_opt = est.spec_opt;

num_opt      = LoadNumOpt();
num_opt.nsim = 50;

price_version = '_v3';
inpath_dat    = DetermineDataPath(spec_opt, price_version);

Inference = load(inpath_var);
Inference = Inference.outputs;
V         = Inference.AVar / Inference.N;
nparam    = length(Inference.param_tab.theta_names);

% Pre-generate Z draws (the kth draw uses rng(k); replicate b uses Z_listing{b}).
Z_listing = cell(NB, 1);
for b = 1:NB
    rng(b);
    Z_listing{b} = V^(1/2) * randn(nparam, 1);
end

parfor b = 1:NB
    fprintf('Bootstrap iteration %d\n\n', b);
    RunFE(b, inpath_dat, outdir_boot, num_opt, Z_listing{b}, spec_opt);
end
