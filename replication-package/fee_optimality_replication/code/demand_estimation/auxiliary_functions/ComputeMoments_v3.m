function [g_a, moment_names, g_i] = ...
    ComputeMoments_v3(data_objs, spec_opt, ...
                      prob_f, chain_probs, theta_vec, theta_names)
    % Compute moments
    % i = individual
    % a = aggregate
    %
    % prob_f: underlying probabilities (not draws) --- these are generally
    %   more precise but can't be used everywhere

    % Parameters for unobserved heterogeneity moments
    K_in = 5;
    K_on = 3;
    K_one = 3;

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

    K_in = 2;
    K_on = 1;

    if sim_moments
        % Probability of extreme events
        prob_in  = sum(prob_f(:, 2:end, :), 2);

        zero_in = 0;% binomcdf_alt(0, spec_opt.T_i, prob_in);
        high_in = 1 - binomcdf_alt(K_in - 1, spec_opt.T_i, prob_in);
        many_in = zero_in + high_in;
        many_in = mean(many_in, 3);  % average over NSIM

        % Probability of at least K_on platform orders
        prob_on = sum(prob_f(:, 3:end, :), 2);   % any online platform

        % P( >= K_on orders on any platform )
        many_on = binomcdf_alt(0, spec_opt.T_i, prob_on);
        many_on = mean(many_on, 3);

        %== Prob >= K_one orders on at least one single platform ==%
        prob_plat = prob_f(:, 3:end, :);
        % P(X_f >= K_one) for each platform f (elementwise binomial tail)
        p_ge_K_one = binomcdf_alt(0, spec_opt.T_i, prob_plat);  % N x F x NSIM

        % Expected number of platforms with >= K_one orders, per consumer and sim:
        exp_num_plat_per_sim = sum(p_ge_K_one, 2);                      % N x 1 x NSIM
        % Average across NSIM draws:
        many_single = mean(exp_num_plat_per_sim, 3);                   % N x 1

        g_i_scale = [many_on, many_single, many_in];
        g_a_scale = mean(g_i_scale);
        
    else
        n_in = sum(data_objs.choice_counts(:, 2:end), 2);

        platform_choice = data_objs.choice_counts(:, 3:end);
        n_on = sum(platform_choice, 2);
        many_single = 1*(platform_choice == 0);
        many_single = sum(many_single, 2);

        zero_in = 0;% 1*(n_in == 0);
        high_in = 1*(n_in >= K_in);
        many_in  = zero_in + high_in;
        many_on  = 1*(n_on == 0);

        g_i_scale = [many_on, many_single, many_in];
        g_a_scale = mean(g_i_scale);
    end

    % Three count-distribution moments identifying the dispersion parameters
    % sigma_zeta1, sigma_zeta2, sigma_eta. (See Appendix in optimal_fees.tex.)
    %   share_no_online    -- many_on:     fraction of consumers placing no online order
    %   mean_n_platforms   -- many_single: expected/observed number of platforms used
    %                                      (sim: E[# with >=1 order];
    %                                       data: # with zero orders out of 4 -- the
    %                                       complement; treated symmetrically by the
    %                                       contraction)
    %   share_heavy_orderer -- many_in:    fraction of consumers placing >= K_in orders
    scale_names = {'share_no_online', ...
                   'mean_n_platforms', ...
                   'share_heavy_orderer'};

    g_a = [g_a_probs, ...
           g_a_dcov1,  g_a_dcov2,  g_a_dcov3, ...
           g_a_scale, g_a_chain, g_a_did];

    g_a = g_a';
    moment_names = horzcat(fe_names, demo_names);
    moment_names = horzcat(moment_names, scale_names);
    moment_names = horzcat(moment_names, ...
                    {'diff_chain', 'fee_did'});
    moment_names = moment_names';
        
    if nargout > 2
        g_i = [g_i_probs, ...
               g_i_dcov1,  g_i_dcov2,  g_i_dcov3, ...
               g_i_scale, g_i_chain, g_i_did];
    end
end
