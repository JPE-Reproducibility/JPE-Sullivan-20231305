function [g_a, moment_names, g_i] = ...
    ComputeMoments_v2(data_objs, spec_opt, ...
                      prob_f, chain_probs, theta_vec, theta_names)
    % Compute moments
    % i = individual
    % a = aggregate
    %
    % prob_f: underlying probabilities (not draws) --- these are generally
    %   more precise but can't be used everywhere

    sim_moments = (nargin > 2);

    T = spec_opt.T_i;

    if sim_moments
        wgt = data_objs.W;
        % Normalize weights
        wgt = wgt./sum(wgt, 2);

        % Direct from probabilities
        e_inside  = prob_f(:, 2:end, :)*T;
        e_platform = sum(e_inside(:, 2:end, :), 2);

        % Number of inside versus outside
        N_chain = squeeze(e_platform).*chain_probs;
    else
        choice_counts = data_objs.choice_counts;
        n_inside  = choice_counts(:, 2:end);
        n_platform = sum(n_inside(:, 2:end), 2);
        
        N_chain = data_objs.N_chain;
    end

    % Extract data
    CBSA_mat  = data_objs.CBSA_mat;
    demo      = data_objs.demo;
    DiD       = data_objs.DiD;

    % Sample size
    n_indiv = size(data_objs.CBSA_mat, 1);

    % Number of cities
    n_subset = size(CBSA_mat, 2);

    % Expand weights
    if sim_moments
        wgt_exp = reshape(wgt, size(wgt, 1), 1, size(wgt, 2));
    end

    % Overall order counts interacted with CBSA identifiers
    n_alt = 5;

    start_idx = 1;
    end_idx   = n_alt;

    g_a_probs = zeros(1,       n_subset*n_alt);
    g_i_probs = zeros(n_indiv, n_subset*n_alt);
    fe_names  = {};
    for m = 1:n_subset
        if sim_moments
            g_i_probs_m = (e_inside.*CBSA_mat(:, m));
            % Average over the simulates, using the weights
            g_i_probs_m = sum(g_i_probs_m.*wgt_exp, 3);
        else
            g_i_probs_m = (n_inside.*CBSA_mat(:, m));
        end

        % Store results
        g_a_probs(start_idx:end_idx)    = mean(g_i_probs_m);
        g_i_probs(:, start_idx:end_idx) = g_i_probs_m;

        % Generate names
        fe_names_m = {sprintf('n_direct_%d', m), sprintf('n_dd_%d', m), ...
                      sprintf('n_uber_%d', m),   sprintf('n_gh_%d', m), ...
                      sprintf('n_pm_%d', m)};
        fe_names = horzcat(fe_names, fe_names_m);

        % Update indices
        start_idx = start_idx + n_alt;
        end_idx   = end_idx   + n_alt;
    end

    % Demo covariances
    if spec_opt.lambda_het
        if sim_moments
            g_i_dcov1 = e_inside.*demo(:, 1);
            g_i_dcov2 = e_inside.*demo(:, 2);
            g_i_dcov3 = e_inside.*demo(:, 3);
            g_i_dcov1 = sum(g_i_dcov1.*wgt_exp, 3);
            g_i_dcov2 = sum(g_i_dcov2.*wgt_exp, 3);
            g_i_dcov3 = sum(g_i_dcov3.*wgt_exp, 3);
        else
            g_i_dcov1 = n_inside.*demo(:, 1);
            g_i_dcov2 = n_inside.*demo(:, 2);
            g_i_dcov3 = n_inside.*demo(:, 3);
        end
    
        g_a_dcov1 = mean(g_i_dcov1);
        g_a_dcov2 = mean(g_i_dcov2);
        g_a_dcov3 = mean(g_i_dcov3);

        demo_names = ...
            {'cov_direct_young',   'cov_dd_young',   'cov_uber_young',   'cov_gh_young',   'cov_pm_young', ...
             'cov_direct_married', 'cov_dd_married', 'cov_uber_married', 'cov_gh_married', 'cov_pm_married', ...
             'cov_direct_hinc',    'cov_dd_hinc',    'cov_uber_hinc',    'cov_gh_hinc',    'cov_pm_hinc'};
                  
    else
        if sim_moments
            e_summ = [e_inside(:, 1, :), e_platform];

            g_i_dcov1 = e_summ.*demo(:, 1);
            g_i_dcov2 = e_summ.*demo(:, 2);
            g_i_dcov3 = e_summ.*demo(:, 3);
            g_i_dcov1 = sum(g_i_dcov1.*wgt_exp, 3);
            g_i_dcov2 = sum(g_i_dcov2.*wgt_exp, 3);
            g_i_dcov3 = sum(g_i_dcov3.*wgt_exp, 3);
        else
            n_summ = [n_inside(:, 1), n_platform];

            g_i_dcov1 = n_summ.*demo(:, 1);
            g_i_dcov2 = n_summ.*demo(:, 2);
            g_i_dcov3 = n_summ.*demo(:, 3);
        end
    
        g_a_dcov1 = mean(g_i_dcov1);
        g_a_dcov2 = mean(g_i_dcov2);
        g_a_dcov3 = mean(g_i_dcov3);

        demo_names = ...
            {'cov_direct_young',   'cov_online_young',   ...
             'cov_direct_married', 'cov_online_married', ...
             'cov_direct_hinc',    'cov_online_hinc'};
 
    end
   
    % Chain share and chain persistence
    g_i_chain    = N_chain; 
    if sim_moments
        g_i_chain    = sum(g_i_chain.*wgt, 2);
    end
    g_a_chain    = mean(g_i_chain);

    %== Simulated commission cap's impact on sales ==%
    if sim_moments
        data_objs1 = data_objs;
        data_objs1.fees = data_objs1.fees_nocap;
        
        data_objs2 = data_objs;
        data_objs2.fees = data_objs2.fees_cap;
        
        prob_f1 = ComputeChoiceProbs_simple(theta_vec, theta_names, data_objs1, spec_opt);
        prob_f2 = ComputeChoiceProbs_simple(theta_vec, theta_names, data_objs2, spec_opt);
    
        prob_f1 = sum(prob_f1.*wgt_exp, 3);
        prob_f2 = sum(prob_f2.*wgt_exp, 3);
        
        prob_f1 = sum(prob_f1(:, 3:end), 2);
        prob_f2 = sum(prob_f2(:, 3:end), 2);
        
        g_i_did = log(prob_f2) - log(prob_f1);
        g_a_did = mean(g_i_did(~isnan(g_i_did)));
    else
        g_a_did = DiD.sales(1)*-0.15;
        g_i_did = zeros(n_indiv, 1);
    end


    %== Likelihood-based moments ==%
    likeli_param = {'logvar_zeta1'; 'logvar_zeta2'; 'logvar_eta'}; % tau
    n_likeli = length(likeli_param);
    
    score_names = cell(1, n_likeli, 1); 
    for j = 1:n_likeli
        score_names{j} = sprintf('score_%s', likeli_param{j});
    end

    if sim_moments
        % Baseline likelihood
        stepsize = 1e-4;
        verbose = false;
        [logL, log_li] = ...
            ChoiceModelLogLikelihood(theta_vec, theta_names, data_objs, ...
                                     verbose, spec_opt);

        % Likelihood at perturbed parameters
        deriv_likeli_a = zeros(1, n_likeli);
        deriv_likeli_i = zeros(n_indiv, n_likeli);
        for j = 1:n_likeli
            idx_j = strcmp(theta_names, likeli_param{j});
            theta_p = theta_vec;
            theta_p(idx_j) = theta_p(idx_j) + stepsize;

            [logL_p, log_li_p] = ...
                ChoiceModelLogLikelihood(theta_p, theta_names, data_objs, ...
                                         verbose, spec_opt);
            deriv_likeli_a(j)    = (logL_p - logL)/stepsize;
            deriv_likeli_i(:, j) = (log_li_p - log_li)/stepsize;
        end

    else
        deriv_likeli_a = zeros(1, n_likeli);
        deriv_likeli_i = zeros(n_indiv, n_likeli);
    end

    g_a = [g_a_probs, ...
           g_a_dcov1,  g_a_dcov2,  g_a_dcov3, ...
           deriv_likeli_a, g_a_chain, g_a_did];

    g_a = g_a';
    moment_names = horzcat(fe_names, demo_names);
    moment_names = horzcat(moment_names, score_names);
    moment_names = horzcat(moment_names, ...
                    {'diff_chain', 'fee_did'});
    moment_names = moment_names';
        
    if nargout > 2
        g_i = [g_i_probs, ...
               g_i_dcov1,  g_i_dcov2,  g_i_dcov3, ...
               deriv_likeli_i, g_i_chain, g_i_did];
    end
end
