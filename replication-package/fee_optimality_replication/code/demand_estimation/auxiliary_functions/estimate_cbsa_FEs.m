function results = estimate_cbsa_FEs(m, data_objs, theta_vec, theta_names, ...
                                      spec_opt)
    % Estimate platform/CBSA FEs for a particular CBSA
    %
    % Inputs
    %     m: CBSA for which we are estimating FEs
    
    subset_fields = ...
        {'fees'             ;
         'fees_nocap'       ;
         'fees_cap'         ;
         'prices_nocap'     ;
         'prices_cap'       ;
         'WT'               ;
         'choice_counts'    ;
         'choice_counts_c'  ;
         'choice_counts_i'  ;
         'N_chain'          ;
         'N_indep'          ;
         'demo'             ;
         'JG'               ;
         'JG_f'             ;
         'JG_chain'         ;
         'JG_f_chain'       ;
         'JG_indep'         ;
         'JG_f_indep'       ;
         'JG_cap'           ;
         'JG_f_cap'         ;
         'JG_capchain'      ;
         'JG_f_capchain'    ;
         'JG_capindep'      ;
         'JG_f_capindep'    ;
         'JG_nocap'         ;
         'JG_f_nocap'       ;
         'JG_nocapchain'    ;
         'JG_f_nocapchain'  ;
         'JG_nocapindep'    ;
         'JG_f_nocapindep'  ;
         'log_JG_capchain'  ;
         'log_JG_nocapchain';
         'log_JG_capindep'  ;
         'log_JG_nocapindep';
         'CBSA_mat'         ;
         'has_cap'          ;
         'Zeta_dag'         ;
         'Zeta_tilde'       ;
         'W'                ;
         'Eta'              ;
         'T_out'            ;
         'Phi_i'            };


    idx = data_objs.CBSA_mat(:, m) == 1;

    for k = 1:length(subset_fields)
        field_k = subset_fields{k};
        A = data_objs.(field_k);
        nd = ndims(A);
        if nd == 2
            data_objs.(field_k) = A(idx, :);
        elseif nd == 3
            data_objs.(field_k) = A(idx, :, :);
        end
    end
    data_objs.CBSA_mat = ones(size(data_objs.CBSA_mat, 1), 1);

    % Reflect that only one CBSA is considered
    data_objs.n_cbsa = 1;
    spec_opt.n_subset = 1;

    % Estimate using the Berry inversion method


    % Construct the theta vector appropriately
    psi_idx = find(~cellfun(@isempty, regexp(theta_names, '^psi_([2-9]\d*)-\d+$', 'once')));
    mu_idx = find(~cellfun(@isempty, regexp(theta_names, '^mu_eta_\d+$')));
    mu_idx = mu_idx(cellfun(@(x) str2double(regexp(x, '\d+', 'match', 'once')) > 1, theta_names(mu_idx)));
    bad_idx = [psi_idx; mu_idx];
    bad_idx = sort(bad_idx);
    keep_idx = setdiff(1:length(theta_vec), bad_idx);

    theta_sub = theta_vec(keep_idx);
    names_sub = theta_names(keep_idx);
    
    % Compute baseline data moments
    gd_a = ComputeMoments_v2(data_objs, spec_opt);

    % Specify moments to match
    moment_sub = 1:5;

    % Specify corresponding parameters
    match_theta = {'mu_eta_1'; 'psi_1-1'; 'psi_1-2'; 'psi_1-3'; 'psi_1-4'};
    theta_idx = zeros(length(match_theta), 1);
    for k = 1:length(match_theta)
        theta_idx(k) = find(strcmp(names_sub, match_theta{k}));
    end

    % Matrix for summing over restaurant purchases
    D = eye(length(moment_sub));
    D(1, 1:5) = 1;

    % Use a Berry contraction approach 
    nrounds = 2e3;
    uprate = 0.20;
    tol = 1e-2;
    niter_print = 100;
    berry_verbose = true;
    for iter = 1:nrounds
        % Compute moments
        gs_a = ComputeSimMoments(theta_sub, names_sub, data_objs, spec_opt);
        
        % Some markets have no Postmates orders, making the log share-
        % matching condition undefined. Floor observed shares at 1e-3 so
        % the contraction is well defined.
        shrs_dat = D*gd_a(moment_sub);
        idx = shrs_dat == 0;
        shrs_dat(idx) = 1e-3;

        shrs_pred = D*gs_a(moment_sub);
        
        diff = log(shrs_dat) - log(shrs_pred);
        
        % Updating
        theta_sub(theta_idx) = theta_sub(theta_idx) + uprate*diff;
     
        % Compute distance
        dist = mean(abs(diff));
        
        if berry_verbose && (iter - round(iter/niter_print)*niter_print == 0)
            fprintf('Berry iteration %d; distance = %f\n', iter, dist);
        end
        if dist < tol
            break
        end
    end
    
    results = table(names_sub(theta_idx), theta_sub(theta_idx));
end



