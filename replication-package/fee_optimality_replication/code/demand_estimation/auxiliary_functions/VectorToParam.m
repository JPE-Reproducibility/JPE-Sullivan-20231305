function param = VectorToParam(theta_vec, theta_names, n_cbsa, n_demo, n_platforms)
    % Place parameters in a struct
    %
    % Inputs
    %   theta_vec: vector of parameter values
    %   theta_names: names of the parameters in theta_vec
    %   n_cbsa: number of markets
    %   n_demo: number of demographic groups
    %   n_platforms: number of platforms
    
    n_theta = length(theta_vec);
    
    % Fixed effects
    idx_psi = regexp(theta_names, '^psi');
    idx_psi = arrayfun(@(x) 1*(~isempty(idx_psi{x})), 1:n_theta);
    psi_vec = theta_vec(idx_psi == 1);
    psi = reshape(psi_vec, n_cbsa, n_platforms);
    
    % Lambda
    idx_lambda = regexp(theta_names, '^lambda');
    idx_lambda = arrayfun(@(x) 1*(~isempty(idx_lambda{x})), 1:n_theta);
    lambda_vec = theta_vec(idx_lambda == 1);
    lambda_het = length(lambda_vec) ~= n_demo;
    
    if ~lambda_het
        lambda = reshape(lambda_vec, n_demo, 1);
        lambda = repmat(lambda, 1, n_platforms);
    else
        lambda = reshape(lambda_vec, n_demo, n_platforms);
    end
    
    % Alpha
    idx_a = regexp(theta_names, '^alpha$');
    idx_a = arrayfun(@(x) 1*(~isempty(idx_a{x})), 1:n_theta);
    alpha = theta_vec(idx_a == 1);
    
    % sigma^2_zeta
    % common component
    idx_z = regexp(theta_names, '^logvar_zeta1');
    idx_z = arrayfun(@(x) 1*(~isempty(idx_z{x})), 1:n_theta);
    logvar_zeta1 = theta_vec(idx_z == 1);
    % platform-specific component
    idx_z = regexp(theta_names, '^logvar_zeta2');
    idx_z = arrayfun(@(x) 1*(~isempty(idx_z{x})), 1:n_theta);
    logvar_zeta2 = theta_vec(idx_z == 1);
    
    incl_np = any(contains(theta_names, 'mu_eta'));
    if incl_np
        idx_mu = contains(theta_names, 'mu_eta');
        mu_eta = theta_vec(idx_mu);
        mu_eta = reshape(mu_eta, numel(mu_eta), 1);
        idx_var = strcmp(theta_names, 'logvar_eta');
        logvar_eta = theta_vec(idx_var); 
        
        demo_np = any(strcmp(theta_names, 'np_young'));
        if demo_np
            idx_young = strcmp(theta_names, 'np_young');
            np_young = theta_vec(idx_young);
            idx_married = strcmp(theta_names, 'np_married');
            np_married = theta_vec(idx_married); 
            if n_demo == 3
                idx_hi     = strcmp(theta_names, 'np_highinc');
                np_highinc = theta_vec(idx_hi); 
            end
        end
        
         idio_eta = any(strcmp(theta_names, 'logvar_idio'));
        if idio_eta
            idx_idio = strcmp(theta_names, 'logvar_idio');
            logvar_idio = theta_vec(idx_idio);
        end
    end
    
    het_extra = any(strcmp(theta_names, 'alpha_young'));
    het_alpha = any(strcmp(theta_names, 'alpha_low'));

    if het_extra
        idx_a = regexp(theta_names, '^alpha_');
        idx_a = arrayfun(@(x) 1*(~isempty(idx_a{x})), 1:n_theta);
        alpha_coef = theta_vec(idx_a == 1);
    elseif het_alpha
        idx_alow = strcmp(theta_names, 'alpha_low');
        alpha_low = theta_vec(idx_alow);
    end
    
    use_NL = any(strcmp(theta_names, 'nu_bar'));
    if use_NL
        idx_nu_bar = strcmp(theta_names, 'nu_bar');
        nu_bar = theta_vec(idx_nu_bar);
    end
    
    use_WT = any(strcmp(theta_names, 'tau'));
    if use_WT
        idx_tau = strcmp(theta_names, 'tau');
        tau = theta_vec(idx_tau);
    end

    use_phi_chain = any(strcmp(theta_names, 'phi_chain'));
    if use_phi_chain
        idx_phi_chain = strcmp(theta_names, 'phi_chain');
        phi_chain = theta_vec(idx_phi_chain);
    end

    use_phi_i = any(strcmp(theta_names, 'logvar_phi'));
    if use_phi_i
        idx_vphi = strcmp(theta_names, 'logvar_phi');
        logvar_phi = theta_vec(idx_vphi);
    end

    % Packaging
    param        = struct();
    param.psi    = psi;
    param.lambda = lambda;
    param.alpha  = alpha;
    param.logvar_zeta1 = logvar_zeta1;
    param.logvar_zeta2 = logvar_zeta2;
    
    if incl_np
        param.mu_eta     = mu_eta;
        param.logvar_eta = logvar_eta;
        
        if demo_np
            param.lambda_np = [np_young; np_married];
            if n_demo == 3
                param.lambda_np = [param.lambda_np; np_highinc];
            end
        end
        
        if idio_eta
            param.logvar_idio = logvar_idio;
        end
    end
    
    if het_extra
        param.alpha_coef = alpha_coef;
    elseif het_alpha
        param.alpha_low = alpha_low;
    end
    
    if use_NL
        param.nu_bar = nu_bar;
    end
    
    if use_WT
        param.tau = tau;
    end

    if use_phi_chain
        param.phi_chain = phi_chain;
    end

    if use_phi_i
        param.logvar_phi = logvar_phi;
    end
    
    % Names
    psi_names = generate_name_array('psi', n_cbsa, n_platforms);
    lambda_names = generate_name_array('lambda', n_demo, n_platforms);
    param.psi_names    = psi_names;
    param.lambda_names = lambda_names;
    
    param.lambda_het = lambda_het;
end
