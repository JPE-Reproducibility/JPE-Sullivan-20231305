function [prob_f, chain_probs] = ComputeChoiceProbs_faster(theta_vec, theta_names, data_objs, spec_opt)
    % This version drops code in which mpt_new or rho_by_g are false
    %
    %
    % chain_probs: probability of ordering from a chain

    alt_param = spec_opt.alt_param;
    if isfield(spec_opt, 'simulate_eta')
        simulate_eta = spec_opt.simulate_eta;
    else
        simulate_eta = true;
    end       
    
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
    
    if (~mpt_new || ~rho_by_g || ~incl_np || idio_eta || ~rest_het)
        error('ComputeChoiceProbs_faster requires new price indices, chain/indep heterogeneity, non-idiosyncratic eta, and outside option')
    end
    if (mpt ~= 4)
        error('Please use mpt == 4 (resubmission 2)')
    end

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
    
    % Use nested logit?
    use_NL = isfield(param, 'nu_bar');
    if use_NL
        nu_bar = param.nu_bar;
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

    % all parameters relative to gamma (fixed at one)
    gamma = 1;
    sigma_eps = spec_opt.sigma_eps;
    
    % Convert nu_bar from R to [0, 1], and compute adjusted gamma
    % that appears in choice probability expressions
    if use_NL
        nu_bar0 = exp(nu_bar)/(1 + exp(nu_bar));
    else
        nu_bar0 = 0;
    end
    gamma_adj = gamma*(1 - nu_bar0);

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
        Phi_i = sigma_phi*data_objs.Phi_i;
    else
        Phi_i = zeros(size(data_objs.Zeta_dag, 1), 1);
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
    clear Psi;

    deltas_cap   = deltas - alpha_i.*data_objs.prices_cap;
    deltas_nocap = deltas - alpha_i.*data_objs.prices_nocap;
   
    if use_WT
        % Adjust for waiting times
        deltas_cap(:, 2:end, :)   = deltas_cap(:, 2:end, :)   - tau*(data_objs.WT);
        deltas_nocap(:, 2:end, :) = deltas_nocap(:, 2:end, :) - tau*(data_objs.WT);
    end
    
    % Add zetas
    n_sim = size(Zeta, 3);
    deltas_cap   = repmat(deltas_cap,   1, 1, n_sim);
    deltas_nocap = repmat(deltas_nocap, 1, 1, n_sim);
    deltas_cap(:, 2:F, :)   = deltas_cap(:, 2:F, :)   + Zeta;
    deltas_nocap(:, 2:F, :) = deltas_nocap(:, 2:F, :) + Zeta;

    % Clear memory
    clear Zeta;
    
    %== Compute inclusive value of each platform set ==%
    exp_delta_cap   = exp(deltas_cap/sigma_eps); 
    exp_delta_nocap = exp(deltas_nocap/sigma_eps); 
    clear deltas_cap deltas_nocap;

    P0 = portfolios0';
    exp_delta_cap_sum   = pagemtimes(exp_delta_cap,   P0);
    exp_delta_nocap_sum = pagemtimes(exp_delta_nocap, P0);
    
    %== Add terms relating to network externalities ==%  
    % Compute separate inclusive values by cap and chain status
    V_plus_capchain   = sigma_eps*log(exp_delta_cap_sum(:, :, :))/gamma_adj   + phi_chain + Phi_i + data_objs.log_JG_capchain;
    V_plus_nocapchain = sigma_eps*log(exp_delta_nocap_sum(:, :, :))/gamma_adj + phi_chain + Phi_i + data_objs.log_JG_nocapchain;
    V_plus_capindep   = sigma_eps*log(exp_delta_cap_sum(:, :, :))/gamma_adj   + data_objs.log_JG_capindep;
    V_plus_nocapindep = sigma_eps*log(exp_delta_nocap_sum(:, :, :))/gamma_adj + data_objs.log_JG_nocapindep; 
    
    eV_capchain   = exp(V_plus_capchain);
    eV_nocapchain = exp(V_plus_nocapchain);
    eV_capindep   = exp(V_plus_capindep);
    eV_nocapindep = exp(V_plus_nocapindep);
    clear V_plus_capchain V_plus_nocapchain V_plus_capindep V_plus_nocapindep;
    
    eV_tot = sum(eV_capchain, 2) + sum(eV_nocapchain, 2) + ...
             sum(eV_capindep, 2) + sum(eV_nocapindep, 2);
    G_probs_capchain   = eV_capchain./eV_tot;
    G_probs_nocapchain = eV_nocapchain./eV_tot;         
    G_probs_capindep   = eV_capindep./eV_tot;
    G_probs_nocapindep = eV_nocapindep./eV_tot;
    eV_tot_chain = sum(eV_capchain, 2) + sum(eV_nocapchain, 2);
    chain_probs = squeeze(eV_tot_chain)./squeeze(eV_tot);
    
    % Clear array to save memory
    clear eV_capchain eV_nocapchain eV_capindep eV_nocapindep eV_tot_chain;
   
    % Compute the V-bar inclusive value of inside restaurants
    eV_tot = squeeze(eV_tot);
    V_bar = Eta1/gamma_adj + log(eV_tot);
    clear Eta1 eV_tot;

    eV_bar = exp(V_bar*(1 - nu_bar0));
    clear V_bar;
    % Replace Inf with large number to ensure that Lambda is computed properly
    eV_bar(eV_bar > 1e4) = 1e4;
    Lambda = eV_bar./(1 + eV_bar);
   
    clear eV_bar;

    % Compute probabilities of choosing platforms
    portfolios_md = reshape(portfolios0', 1, F, n_portfolios);
    % Include probability of no purchase
    pf_dim_2 = F + 1;
 
   

    I   = size(exp_delta_cap, 1);
    F   = size(exp_delta_cap, 2);
    NS  = size(exp_delta_cap, 3);
    nP  = size(exp_delta_cap_sum, 2);
    
    % Broadcast helpers
    if size(portfolios_md,1) == 1
        Pm = reshape(portfolios_md, 1, F, nP, 1);   % 1 x F x nP x 1
    else
        Pm = reshape(portfolios_md, I, F, nP, 1);   % I x F x nP x 1
    end
    
    % Numerators (expand to I x F x 1 x NS, then to I x F x nP x NS via .* Pm)
    num_cap   = reshape(exp_delta_cap,   I, F, 1, NS) .* Pm;   % I x F x nP x NS
    num_nocap = reshape(exp_delta_nocap, I, F, 1, NS) .* Pm;   % I x F x nP x NS
    
    % Denominators (I x nP x NS) -> (I x 1 x nP x NS)
    den_cap   = reshape(exp_delta_cap_sum,   I, 1, nP, NS) + eps(0);
    den_nocap = reshape(exp_delta_nocap_sum, I, 1, nP, NS) + eps(0);
    
    % Conditional probabilities mu_* (I x F x nP x NS)
    mu_cap   = num_cap   ./ den_cap;
    mu_nocap = num_nocap ./ den_nocap;
    
    % G-weights expanded to (I x 1 x nP x NS)
    G_cap_chain   = reshape(G_probs_capchain,   I, 1, nP, NS);
    G_cap_indep   = reshape(G_probs_capindep,   I, 1, nP, NS);
    G_nocap_chain = reshape(G_probs_nocapchain, I, 1, nP, NS);
    G_nocap_indep = reshape(G_probs_nocapindep, I, 1, nP, NS);
    
    % Sum over portfolios (dim 3) -> (I x F x NS)
    prob_f_capchain    = sum(mu_cap   .* G_cap_chain,   3);
    prob_f_capindep    = sum(mu_cap   .* G_cap_indep,   3);
    prob_f_nocapchain  = sum(mu_nocap .* G_nocap_chain, 3);
    prob_f_nocapindep  = sum(mu_nocap .* G_nocap_indep, 3);
    
    % Combine states
    prob_f_ns = prob_f_capchain + prob_f_capindep + prob_f_nocapchain + prob_f_nocapindep;  % I x F x NS
    prob_f_ns = squeeze(prob_f_ns);
    Lambda = reshape(Lambda, I, 1, NS);
    
    % Add no-purchase if needed
    if incl_np
        prob_f = cat(2, 1 - Lambda, prob_f_ns.*Lambda);  % I x (F+1) x NS
    else
        prob_f = prob_f_ns;  % I x F x NS
    end

end
