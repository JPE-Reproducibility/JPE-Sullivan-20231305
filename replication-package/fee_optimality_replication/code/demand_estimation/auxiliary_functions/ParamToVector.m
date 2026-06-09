function [theta_vec, theta_names] = ParamToVector(param)
    % Go from distinct structures to vector
    
    % Extract parameter values
    psi    = param.psi;
    lambda = param.lambda;
    alpha  = param.alpha;
    logvar_zeta1 = param.logvar_zeta1;
    logvar_zeta2 = param.logvar_zeta2;
    
    % Use high income in utility?
    lambda_inc = size(lambda, 1) == 3;

    % Extract names
    if nargout > 1
        psi_names    = param.psi_names;
        lambda_names = param.lambda_names;
    end
    
    n_psi    = numel(psi);
    n_lambda = numel(lambda);

    psi_vec = reshape(psi, n_psi, 1);
    lambda_vec = reshape(lambda, n_lambda, 1);
    
    theta_vec = [psi_vec; lambda_vec; alpha; ...
                 logvar_zeta1; logvar_zeta2];
    
    incl_np = isfield(param, 'mu_eta');
    if incl_np
        mu_eta = param.mu_eta;
        n_mu_eta = length(mu_eta);
        if n_mu_eta == 1
            mu_eta_names = {'mu_eta'};
        else
            mu_eta = reshape(mu_eta, n_mu_eta, 1);
            mu_eta_names = cell(n_mu_eta, 1);
            for kx = 1:n_mu_eta
                mu_eta_names{kx} = sprintf('mu_eta_%d', kx);
            end
        end
        logvar_eta = param.logvar_eta;
        theta_vec = [theta_vec; mu_eta; logvar_eta];
        
        demo_np = isfield(param, 'lambda_np');
        if demo_np
            lambda_np = param.lambda_np;
            lambda_np = reshape(lambda_np, numel(lambda_np), 1);
            theta_vec = [theta_vec; lambda_np];
        end
        
        idio_eta = isfield(param, 'logvar_idio');
        if idio_eta
            logvar_idio = param.logvar_idio;
            theta_vec = [theta_vec; logvar_idio];
        end
    end
    
    het_extra = isfield(param, 'alpha_coef');
    het_alpha = isfield(param, 'alpha_low');

    if het_extra
        alpha_coef = param.alpha_coef;
        theta_vec = [theta_vec; alpha_coef];
    elseif het_alpha
        alpha_low = param.alpha_low;
        theta_vec = [theta_vec; alpha_low];
    end
    
    use_NL = isfield(param, 'nu_bar');
    if use_NL
        nu_bar = param.nu_bar;
        theta_vec = [theta_vec; nu_bar];
    end
    
    use_WT = isfield(param, 'tau');
    if use_WT
        tau = param.tau;
        theta_vec = [theta_vec; tau];
    end
          
    use_phi_chain = isfield(param, 'phi_chain');
    if use_phi_chain
        phi_chain = param.phi_chain;
        theta_vec = [theta_vec; phi_chain];
    end

    use_phi_i = isfield(param, 'logvar_phi');
    if use_phi_i
        logvar_phi = param.logvar_phi;
        theta_vec = [theta_vec; logvar_phi];
    end

    if nargout > 1
        % Reshape parameter name arrays
        psi_names_vec    = reshape(psi_names,    n_psi,    1);
        lambda_names_vec = reshape(lambda_names, n_lambda, 1);
        % Combine parameter names into a single array
        theta_names = vertcat(psi_names_vec, lambda_names_vec);
        
        theta_names = vertcat(theta_names, ...
             {'alpha'; 'logvar_zeta1'; 'logvar_zeta2'});
        
        if incl_np
            theta_names = vertcat(theta_names, mu_eta_names);
            theta_names = vertcat(theta_names, {'logvar_eta'});
            
            if demo_np
                theta_names = vertcat(theta_names, {'np_young'; 'np_married'});
                if lambda_inc
                    theta_names = vertcat(theta_names, {'np_highinc'});
                end
            end        
            
            if idio_eta
                theta_names = vertcat(theta_names, {'logvar_idio'});
            end
        end
        
        if het_extra
            alpha_names = {'alpha_young'; 'alpha_married'; 'alpha_highinc'};
            theta_names = vertcat(theta_names, alpha_names);
        elseif het_alpha
            theta_names = vertcat(theta_names, {'alpha_low'});
        end
        
        if use_NL
            theta_names = vertcat(theta_names, {'nu_bar'});
        end
        
        if use_WT      
            theta_names = vertcat(theta_names, {'tau'});
        end

        if use_phi_chain      
            theta_names = vertcat(theta_names, {'phi_chain'});
        end

        if use_phi_i      
            theta_names = vertcat(theta_names, {'logvar_phi'});
        end
    end
end
