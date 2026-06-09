function [Psi, est_tab, MuEta] = RunFE(b, inpath_dat, outdir_boot, num_opt, ...
                                       Z, spec_opt)
    % Run Psi fixed effects estimation using parametric bootstrap
    %
    % Inputs
    %   b: Bootstrap replicate number. Use b = 0 to specify a non-bootstrap run
    %   outdir_boot: Output directory
    %   Z: parameter deviations
    %   Z_names: parameter names
   
    data_dir   = '../../data/demand_estimation';
    inpath_est =  sprintf('%s/est_results-yipitdata.mat', data_dir);
    
    if b > 0
        outpath_mat = sprintf('%s/Psi_results_%d.mat', outdir_boot, b);
        outpath_tab = sprintf('%s/est_table_%d.csv', outdir_boot, b);
    end
    
    % Load parameter estimates 
    est = load(inpath_est);
    est = est.results;

    theta_vec   = est.theta_vec;
    theta_names = est.theta_names;

    DiD = est.data_objs.DiD;

    % Load data
    spec_opt.market_subset = false;

    rng(1);
    spec_opt.sample_rate = 1.0;
    [dat, ~, n_cbsa] = LoadData(inpath_dat, spec_opt);

    n_platforms = 4;
    
    if b > 0      
        rng(b);
        dat_b = DrawBootstrapData(dat);
    else
        dat_b = dat;
    end

    data_objs = GenerateDataObjs(dat_b, num_opt, spec_opt, DiD);
    
    % Update parameters with bootstrap deviations
    if b > 0
        theta_vec_1 = theta_vec + Z;
    else
        theta_vec_1 = theta_vec;
    end

    % Estimation
    % Resume here - get this to run, and save the results properly
    est_objs = cell(n_cbsa, 1);  
    n_cbsa
    for m = 1:n_cbsa
        m
        est_objs{m} = estimate_cbsa_FEs(m, data_objs, theta_vec_1, theta_names, ...
                                        spec_opt);
        
    end

    % Combine results
    n_param_est = size(est_objs{m}, 1);
    Psi = zeros(n_cbsa, n_platforms);
    
    for m = 1:n_cbsa
        est_m = est_objs{m};
        psi_idx = startsWith(est_m.Var1, 'psi');
        Psi(m, :) = est_m.Var2(psi_idx);
    end

    spec_opt.eta_by_m = (n_param_est > n_platforms);
    if spec_opt.eta_by_m
       MuEta = zeros(n_cbsa, 1);
       for m = 1:n_cbsa
            est_m = est_objs{m};
            mu_idx = startsWith(est_m.Var1, 'mu_eta');
            MuEta(m) = est_m.Var2(mu_idx);
       end
    else
        MuEta = [];
    end
    
    % Produce a table
    n_platforms = data_objs.n_platforms;
    n_demo = data_objs.n_demo;
    est_param = VectorToParam(theta_vec_1, theta_names, spec_opt.n_subset, n_demo, n_platforms);
    
    psi_names = generate_name_array('psi', n_cbsa, n_platforms);
    est_param.psi = Psi;
    est_param.psi_names = psi_names;
    if spec_opt.eta_by_m
        est_param.mu_eta = MuEta;
    end
    
    [theta_vec_1, theta_names] = ParamToVector(est_param);
    est_tab = table(theta_names, theta_vec_1);
    
    if b > 0
        % Save results as a MATLAB object and also as a table
        % MATLAB object if we are in bootstrap mode
        save(outpath_mat, 'Psi', 'spec_opt');
        writetable(est_tab, outpath_tab, 'WriteRowNames', false);
    end
end



