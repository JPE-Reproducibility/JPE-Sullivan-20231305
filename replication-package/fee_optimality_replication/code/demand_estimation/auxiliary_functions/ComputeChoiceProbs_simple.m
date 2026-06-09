function [prob_f, chain_probs] = ComputeChoiceProbs_simple(theta_vec, theta_names, data_objs, spec_opt)


    alt_param = spec_opt.alt_param;
    simulate_eta = spec_opt.simulate_eta;
            
    
    % Separate rho for each platform portfolio?
    rho_by_g = spec_opt.rho_by_g;

    % Use hedonic price indices that differ across chain and independent restaurants?
    mpt = spec_opt.menu_price_type;
    mpt_new = (mpt == 1 || mpt == 2 || mpt == 3 || mpt == 4); % one of the new type of menu prices

    % All distinct consumer tastes for different types of restaurants?
    rest_het = spec_opt.rest_het;

    % Platform-specific demographic tastes?
    lambda_het = spec_opt.lambda_het;
    
    % Extract data
    portfolios0  = data_objs.portfolios0;
    n_cbsa       = data_objs.n_cbsa;
    n_demo       = data_objs.n_demo;
    n_platforms  = data_objs.n_platforms;
    n_portfolios = data_objs.n_portfolios;
    F            = data_objs.F;
     
    % Include a no-purchase option?
    incl_np = isfield(data_objs, 'Eta');
    % t-specific restaurant tastes eta?
    idio_eta = isfield(data_objs, 'EtaIdio');
    % mu_eta varies by market?
    n_mu_eta = sum(contains(theta_names, 'mu_eta'));
    eta_by_m = (n_mu_eta > 1);

    % Place parameter in struct form
    param = VectorToParam(theta_vec, theta_names, n_cbsa, ...
                          n_demo, n_platforms);
    psi    = param.psi;
    lambda = param.lambda;

    alpha  = param.alpha;
    
    % Variance of platform taste components
    var_zeta1   = exp(param.logvar_zeta1);
    sigma_zeta1 = sqrt(var_zeta1);
    var_zeta2   = exp(param.logvar_zeta2);
    sigma_zeta2 = sqrt(var_zeta2);  
    
    % Parameters governing tastes for restaurant meals
    mu_eta = param.mu_eta;
    var_eta = exp(param.logvar_eta);
    sigma_eta = sqrt(var_eta);

    demo_np = isfield(param, 'lambda_np');
    if demo_np
        lambda_np = param.lambda_np;
        % Ensure lambda_np is a column vector, not a row vector
        lambda_np = reshape(lambda_np, numel(lambda_np), 1);
    end
    
    % Heterogeneous alpha?
    het_alpha = isfield(param, 'alpha_low');
    het_extra = isfield(param, 'alpha_coef');
    if het_extra
        alpha_coef = param.alpha_coef;
    elseif het_alpha
        alpha_low = param.alpha_low;
    end
    
    
    % Control for waiting time?
    use_WT = isfield(param, 'tau');
    if use_WT
        tau = param.tau;
    end  

    % Restaurant heterogeneity?
    use_phi_chain = isfield(param, 'phi_chain');
    if use_phi_chain
        phi_chain = param.phi_chain;
    end
    use_phi_i = isfield(param, 'logvar_phi');
    if use_phi_i
        logvar_phi = param.logvar_phi;
        var_phi = exp(logvar_phi);
        sigma_phi = sqrt(var_phi);
    end

    sigma_eps = spec_opt.sigma_eps;

    % Include zeros for outside platform
    psi0    = [zeros(n_cbsa, 1), psi];
    lambda0 = [zeros(1, n_demo); lambda'];

    % Add together the zeta components
    % We copy Zeta_dag because it is the same for each platform
    Zeta = sigma_zeta1*repmat(data_objs.Zeta_dag, 1, n_platforms, 1) + ...
           sigma_zeta2*(data_objs.Zeta_tilde);
    
    % Rescale and recentre Eta
    if eta_by_m
        Eta1 = (data_objs.CBSA_mat)*mu_eta + sigma_eta*(data_objs.Eta);   
    else
        Eta1 = mu_eta + sigma_eta*(data_objs.Eta);
    end
    
    % Add contribution of demographics
    if demo_np
        Eta1 = Eta1 + data_objs.demo*lambda_np;
    end

    if use_phi_i
        Phi_i = phi_chain + sigma_phi*data_objs.Phi_i;
    else
        Phi_i = phi_chain + zeros(size(data_objs.Zeta_dag, 1), 1);
    end
    Phi_i = reshape(Phi_i, [size(Phi_i, 1), 1, size(Phi_i, 2)]);

    % The Psi matrix contains the appropriate fixed effect for the 
    % current market
    Psi = (data_objs.CBSA_mat)*psi0;
    
    % Compute the utility indices
    if het_extra
        X_p = [data_objs.demo(:, 1:2), data_objs.high_inc];
        alpha_i = alpha + X_p*alpha_coef;
    elseif het_alpha
        alpha_i = alpha + alpha_low.*(data_objs.low_inc);
    else
        alpha_i = alpha;
    end
    
    deltas = Psi - alpha_i.*(data_objs.fees) + data_objs.demo*(lambda0');

    deltas_cap   = deltas - alpha_i.*data_objs.prices_cap;
    deltas_nocap = deltas - alpha_i.*data_objs.prices_nocap;
   
    if use_WT
        % Adjust for waiting times
        deltas_cap(:, 2:end)   = deltas_cap(:, 2:end)   - tau*(data_objs.WT);
        deltas_nocap(:, 2:end) = deltas_nocap(:, 2:end) - tau*(data_objs.WT);
    end
    
    % Add zetas
    n_sim = size(Zeta, 3);
    deltas_cap   = repmat(deltas_cap,   1, 1, n_sim);
    deltas_nocap = repmat(deltas_nocap, 1, 1, n_sim);
    deltas_cap(:, 2:F, :)   = deltas_cap(:, 2:F, :)   + Zeta;
    deltas_nocap(:, 2:F, :) = deltas_nocap(:, 2:F, :) + Zeta;

    Eta_exp = reshape(Eta1, [size(Eta1, 1), 1, size(Eta1, 2)]);
    deltas_cap   = deltas_cap + Eta_exp;
    deltas_nocap = deltas_nocap + Eta_exp;

    % Inclusive values for each ordering channel
    ePhi = exp(Phi_i);
    J_chain_f = data_objs.JG_f_chain;
    J_chain_tot = sum(data_objs.JG_chain, 2);
    J_chain = [J_chain_tot, J_chain_f];
    J_indep_f = data_objs.JG_f_indep;
    J_indep_tot = sum(data_objs.JG_indep, 2);
    J_indep = [J_indep_tot, J_indep_f];
    incl_cap   = deltas_cap   + log(ePhi.*J_chain + J_indep);
    incl_nocap = deltas_nocap + log(ePhi.*J_chain + J_indep);

    % Probabilities
    sigma = 1; % sigma_eps
    eU_cap     = exp(incl_cap/sigma);
    eU_nocap   = exp(incl_nocap/sigma);
    prob_cap   = eU_cap./sum(eU_cap, 2);
    prob_nocap = eU_nocap./sum(eU_nocap, 2);

    eU = zeros(size(eU_cap));
    mu_prob = zeros(size(prob_cap));
    cap_idx   = data_objs.has_cap == 1;
    nocap_idx = data_objs.has_cap == 0;
    eU(cap_idx, :, :)    = eU_cap(cap_idx, :, :);
    eU(nocap_idx, :, :) = eU_nocap(nocap_idx, :, :);
    mu_prob(cap_idx, :, :) = prob_cap(cap_idx, :, :);
    mu_prob(nocap_idx, :, :) = prob_nocap(nocap_idx, :, :);


    eV = sum(eU, 2);
    eV = squeeze(eV);
    % Replace Inf with large number to ensure that Lambda is computed properly
    eV(eV > 1e4) = 1e4;
    Lambda = eV./(1 + eV);

    Lambda_exp = reshape(Lambda, size(Lambda, 1), 1, size(Lambda, 2));
    prob_f = Lambda_exp.*mu_prob;

    prob_f = [1 - Lambda_exp, prob_f];

    % Chain probs (simplified)
    chain_probs = (ePhi.*J_chain)./(ePhi.*J_chain + J_indep);
    chain_probs = squeeze(chain_probs(:, 1, :));
end
