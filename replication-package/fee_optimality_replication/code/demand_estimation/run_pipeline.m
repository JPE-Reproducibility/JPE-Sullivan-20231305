% Run the full demand-estimation pipeline end-to-end.
%
%   1. estimate_consumer_choice -- GMM on the structural parameters using a
%      subset of metros. Writes est_results-yipitdata.mat.
%   2. fe_estimation            -- per-market psi/mu_eta on all metros.
%   3. compute_GMM_SE_v2        -- GMM standard errors at the point estimate.
%      MUST precede the bootstrap (it produces inference.mat, which the
%      bootstrap reads).
%   4. fe_estimation_boot       -- 100 bootstrap replicates of step 2.
%   5. SE_table                 -- combines point estimates and SEs into the
%      CSV that produce_table.R formats into the paper's Table 2.

estimate_consumer_choice;
clearvars -except;       % drop all but the workspace identifier

fe_estimation;
clearvars -except;

compute_GMM_SE_v2;
clearvars -except;

fe_estimation_boot;
clearvars -except;

SE_table;
clearvars -except;

fprintf('\nMatlab pipeline complete. Next: Rscript produce_table.R\n');
