function [param, theta_vec, theta_names, lb, ub] = ConstructParam(spec_opt, n_cbsa, n_platforms, n_demo)
    % Construct objects containing parameter values for use in estimation
    % Also produce reasonable lower and upper bounds for estimation
    
    if spec_opt.market_subset

        psi =  [ 0.21626,  0.49264,  0.46242, 0.51653; 
                -0.00424,  0.51957, -0.03856, 0.76359;
                 0.098486, 0.27049, -0.00291, 0.58743];

        if spec_opt.n_subset > 3
            psi_extra = [-2.1489, -1.9795, -3.3118, -3.0544];
            psi = [psi; repmat(psi_extra, spec_opt.n_subset - 3, 1)];
        elseif spec_opt.n_subset < 3
            psi = psi(1:spec_opt.n_subset, :);
        end

        alpha        = 0.23894;
        logvar_zeta1 = 0.48301;
        logvar_zeta2 = -0.3895;

        if spec_opt.lambda_het
            lambda = [ 0.49494,  0.55468, 0.23073,  0.40533 ;
                      -1.4366,   -1.5337, -1.3026, -1.9138];
            if spec_opt.lambda_inc
                lambda = [lambda; -0.90376, -0.9349, -0.90374, -1.544];
            end
        else
            lambda = [0.64289; -0.43665];
            if spec_opt.lambda_inc
                lambda = [lambda; -1.00];
            end

        end
    else
        psi = [-3.5511, -5.5626, -9.4589, -5.6455;
               -0.8039,  1.0801, -3.2735, -3.2946;
               -4.6695, -5.9107, -6.5289, -4.8682;
               -3.9268, -6.0805, -8.0064, -7.1855;
               -3.0681, -5.3217, -8.0235, -4.5060;
               -5.1650, -6.7142, -7.7784, -3.6553;
               -4.3613, -4.8389, -9.8597, -3.2240;
               -3.3714, -3.1286, -4.4544, -5.0403;
               -2.2680, -4.7202, -4.8687, -4.7116;
               -5.3211, -6.2714, -7.9112, -3.6454;
               -5.1048, -7.4422, -5.6220, -5.7840;
                0.0412, -2.7932, -4.5296, -4.1043;
               -0.8070, -2.4211, -6.4665, -0.9486;
               -3.3087, -3.1233, -7.4172, -4.8354];

        alpha        = 0.7440;
        logvar_zeta1 = 4.0227;
        logvar_zeta2 = 2.7138;

        if spec_opt.lambda_het
            lambda = [  1.1907,  1.0593,  0.70206,  0.89379;
                       -0.87078, -1.0721,  -0.6349, -1.98440];
            if spec_opt.lambda_inc
                lambda = [lambda; 0.0, 0.0, 0.0, 0.0];
            end
        else
            lambda = [1.907; -0.87078];
            if spec_opt.lambda_inc
                lambda = [lambda; 0.0];
            end
        end
    end


    if spec_opt.incl_np
        if spec_opt.eta_by_m
            % mu_eta varies by market
            mu_eta = [-4.9651;
                      -5.0458;
                      -3.9829];
            
            if spec_opt.n_subset > 3
                mu_eta = [mu_eta; repmat(-3.8166, spec_opt.n_subset - 3, 1)];
            elseif spec_opt.n_subset < 3
                mu_eta = mu_eta(1:spec_opt.n_subset);
            end
        else
            % Single mu_eta
            mu_eta = 0;
        end
        logvar_eta = 1.4028;

        if spec_opt.demo_np
            lambda_np = [-0.69957; ...
                         -2.4594]; 
            if spec_opt.lambda_inc
                lambda_np = [lambda_np; -3.2922];
            end
        end

        if spec_opt.idio_eta
            logvar_idio = 5.1883;
        end
    end

    if spec_opt.het_extra
        alpha_coef = [ -0.011914; -0.11887; -0.14727];
    elseif spec_opt.het_alpha
        alpha_low = 0.0013257;
    end

    if spec_opt.use_NL
        nu_bar = 3.7994;
    end
    
    if spec_opt.use_WT
        tau = 0.53349;
    end

    if spec_opt.rest_het == 1
        phi_chain = 0.89491; %-0.83685;
    end

    if spec_opt.rest_RC == 1
        logvar_phi =  -0.27226;
    end

    % include column/row of zeros for outside option
    if spec_opt.lambda_het
        lambda_names = generate_name_array('lambda', n_demo, n_platforms);
    else
        lambda_names = vertcat({'lambda_1'}, {'lambda_2'});
        if spec_opt.lambda_inc
            lambda_names = vertcat(lambda_names, {'lambda_3'});
        end
    end

    psi_names = generate_name_array('psi', n_cbsa, n_platforms);

    % Place parameters into a struct
    param = struct();
    param.psi    = psi;
    param.lambda = lambda;
    param.alpha  = alpha;
    param.logvar_zeta1 = logvar_zeta1;
    param.logvar_zeta2 = logvar_zeta2;
    param.psi_names    = psi_names;
    param.lambda_names = lambda_names;
    if spec_opt.incl_np
        param.mu_eta     = mu_eta;
        param.logvar_eta = logvar_eta;

        if spec_opt.demo_np
            param.lambda_np = lambda_np;
        end

        if spec_opt.idio_eta
            param.logvar_idio = logvar_idio;
        end
    end

    if spec_opt.het_extra
        param.alpha_coef = alpha_coef;
    elseif spec_opt.het_alpha
        param.alpha_low = alpha_low;
    end

    if spec_opt.use_NL
        param.nu_bar = nu_bar;
    end

    if spec_opt.use_WT
        param.tau = tau;
    end
    
    if spec_opt.rest_het
        param.phi_chain = phi_chain;
    end
    
    if spec_opt.rest_RC
        param.logvar_phi = logvar_phi;
    end

    param.lambda_het = spec_opt.lambda_het;

    if spec_opt.alt_param >= 1
        % Starting value
        if spec_opt.alt_param == 1
            param.gamma = 1/param.gamma;
        end
        [theta_vec, theta_names] = ParamToVector(param);

        %== Bounds for global solvers ==%
        % FEs and lambdas
        lb = -5*ones(size(theta_vec));
        ub =  5*ones(size(theta_vec)); 

        if spec_opt.sset
            idx = find(strcmp(theta_names, 'psi_1-1'));
            lb(idx) = 1.5;
            ub(idx) = 3.15;
            idx = find(strcmp(theta_names, 'psi_2-1'));
            lb(idx) = 0.5;
            ub(idx) = 1.75;
            idx = find(strcmp(theta_names, 'psi_3-1'));
            lb(idx) = 1.5;
            ub(idx) = 3.15;


            idx = find(strcmp(theta_names, 'psi_1-2'));
            lb(idx) = 2.5;
            ub(idx) = 3.75;
            idx = find(strcmp(theta_names, 'psi_2-2'));
            lb(idx) = 1.1;
            ub(idx) = 2.45;
            idx = find(strcmp(theta_names, 'psi_3-2'));
            lb(idx) = -0.3;
            ub(idx) = 0.3;

            idx = find(strcmp(theta_names, 'psi_1-3'));
            lb(idx) = 1;
            ub(idx) = 3.5;
            idx = find(strcmp(theta_names, 'psi_2-3'));
            lb(idx) = -1.25;
            ub(idx) = 1;
            idx = find(strcmp(theta_names, 'psi_3-3'));
            lb(idx) = -0.75;
            ub(idx) = 0.25;

            idx = find(strcmp(theta_names, 'psi_1-4'));
            lb(idx) = -0.5;
            ub(idx) = 2;
            idx = find(strcmp(theta_names, 'psi_2-4'));
            lb(idx) = 0.8;
            ub(idx) = 3;
            idx = find(strcmp(theta_names, 'psi_3-4'));
            lb(idx) = -1.15;
            ub(idx) = 1;

            idx = find(strcmp(theta_names, 'lambda_1'));
            lb(idx) = 1.1;
            ub(idx) = 2.2;
            idx = find(strcmp(theta_names, 'lambda_2'));
            lb(idx) = -3.3;
            ub(idx) = -2.2;
        else
            lb(idx) = 0.2;
            ub(idx) = 1.25;
        end

        % Alpha
        idx_alpha = find(strcmp(theta_names, 'alpha'));
        lb(idx_alpha) = -0.5;
        ub(idx_alpha) = 1.5;

        % Heterogeneity in alpha
        if spec_opt.het_extra
            ay = find(strcmp(theta_names, 'alpha_young'));
            am = find(strcmp(theta_names, 'alpha_married'));
            ah = find(strcmp(theta_names, 'alpha_highinc'));
            idx_alpha = find(ay | am | ah);
            lb(idx_alpha) = -0.15;
            ub(idx_alpha) = 0.15;

        if spec_opt.het_alpha
            idx_alpha = find(strcmp(theta_names, 'alpha_low'));
            lb(idx_alpha) = -0.5;
            ub(idx_alpha) = 1.5;

            if spec_opt.sset
                lb(idx_alpha) = -0.1;
                ub(idx_alpha) = 1.35;
            else
                lb(idx_alpha) = 0;
                ub(idx_alpha) = 1.25;
            end
        end

        idx_gamma = find(strcmp(theta_names, 'gamma'));
        if spec_opt.alt_param == 1
            % sigma_epsilon
            lb(idx_gamma) = 1e-5;
            ub(idx_gamma) = 1;
        elseif spec_opt.alt_param == 2
            % gamma
            lb(idx_gamma) = 0.5;
            ub(idx_gamma) = 10;  
        end
        % Sigma_zetas
        idx_zeta = arrayfun(@(k) ~isempty(regexp(theta_names{k}, 'logvar_zeta')), 1:length(theta_names));
        idx_zeta = find(idx_zeta);
        lb(idx_zeta) = -2;
        ub(idx_zeta) = 4;

        % Eta parameters
        if spec_opt.incl_np
            idx_mu = find(strcmp(theta_names, 'mu_eta'));
            lb(idx_mu) = -10;
            ub(idx_mu) = 6;
            idx_var = find(strcmp(theta_names, 'logvar_eta'));
            lb(idx_var) = -3;
            ub(idx_var) = 8;

            if spec_opt.sset
                idx_mu = find(strcmp(theta_names, 'mu_eta_1'));
                lb(idx_mu) = -2.25;
                ub(idx_mu) = -1.3;
                idx_mu = find(strcmp(theta_names, 'mu_eta_2'));
                lb(idx_mu) = -0.75;
                ub(idx_mu) = 2.5;
                idx_mu = find(strcmp(theta_names, 'mu_eta_3'));
                lb(idx_mu) = -3.00;
                ub(idx_mu) = -1.80;

                lb(idx_var) = 1.5;
                ub(idx_var) = 2.8;


            else
                lb(idx_mu) = -0.75;
                ub(idx_mu) = 2.5;

                lb(idx_var) = 1.5;
                ub(idx_var) = 3;
            end

            if spec_opt.demo_np
                idx_y = find(strcmp(theta_names, 'np_young'));
                lb(idx_y) = -6;
                ub(idx_y) = 6;
                idx_m = find(strcmp(theta_names, 'np_married'));
                lb(idx_m) = -6;
                ub(idx_m) = 6;

                if spec_opt.sset
                    lb(idx_y) = -1;
                    ub(idx_y) = 1.2;

                    lb(idx_m) = 0.5;
                    ub(idx_m) = 1.6;
                else
                    lb(idx_y) = -1;
                    ub(idx_y) = 2;

                    lb(idx_m) = -0.5;
                    ub(idx_m) = 1.8;
                end
            end

            if spec_opt.idio_eta
                idx_idio = find(strcmp(theta_names, 'logvar_idio'));
                lb(idx_idio) = -3;
                ub(idx_idio) = 8;
            end
        end
        % Nested logit parameter
        if spec_opt.use_NL
            idx_nu = find(strcmp(theta_names, 'nu_bar'));
            lb(idx_nu) = -5;
            ub(idx_nu) = 5;
        end
        
        % Taste for waiting time
        if spec_opt.use_WT
            idx_tau = find(strcmp(theta_names, 'tau'));
            if spec_opt.sset
                lb(idx_tau) = 0.6;
                ub(idx_tau) = 1.75;
            else
                lb(idx_tau) = 0;
                ub(idx_tau) = 2;
            end
        end

        if spec_opt.rest_het
            idx_phi_chain = find(strcmp(theta_names, 'phi_chain'));

            if spec_opt.sset
                lb(idx_phi_chain) = 1.7;
                ub(idx_phi_chain) = 2.8;
            else
                lb(idx_phi_chain) = 1.5;
                ub(idx_phi_chain) = 3.2;
            end
        end

        if spec_opt.rest_RC
            idx_vphi = find(strcmp(theta_names, 'logvar_phi'));

            if spec_opt.sset
                lb(idx_vphi) = -10;
                ub(idx_vphi) = -6;
            else
                lb(idx_vphi) = -9;
                ub(idx_vphi) = -6.6;
            end
        end
    else
        %== Bounds for global solvers ==%
        [theta_vec, theta_names] = ParamToVector(param);
        % FEs
        lb = -15*ones(size(theta_vec));
        ub =  15*ones(size(theta_vec)); 
        % Alpha
        lb(end - 3) = -1.0;
        ub(end - 3) = 4.0;
        % Gamma
        lb(end - 2) = 0;
        ub(end - 2) = 20;
        % Sigmas
        lb(end - 1:end) = -3;
        ub(end - 1:end) = 6;

    end
end


