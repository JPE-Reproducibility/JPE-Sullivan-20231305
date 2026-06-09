function data_objs = GenerateDataObjs(dat, num_opt, spec_opt, DiD)
    % Generate the data objects used in estimation
    %
    % Inputs
    %   dat: estimation sample
    %   num_opt: numerical options
    %   spec_opt: specification options

    rng(1);
    
    % Extract options
    menu_adjust = spec_opt.menu_adjust;
    incl_np     = spec_opt.incl_np;
    idio_eta    = spec_opt.idio_eta;
    het_alpha   = spec_opt.het_alpha;
    het_extra   = spec_opt.het_extra;
    rho_by_g    = spec_opt.rho_by_g;
    mpt         = spec_opt.menu_price_type;
    rest_RC     = spec_opt.rest_RC;
    IS          = spec_opt.IS;

    hedonic = (mpt == 1);
    psample = (mpt == 2);
    did     = (mpt == 3);
    resub2  = (mpt == 4);

    % Determine various dimensions
    n_cbsa = length(unique(dat.m));
    n_panelist = size(dat, 1);
    F = 5;
    n_platforms = F - 1;

    % Adjust prices
    if isfield(spec_opt, 'alt_prices')
        if spec_opt.alt_prices && ~rho_by_g
            dat.rho_online = dat.rho_online_cap;
        end
    end
    
    fees = ConstructFeeMatrix(dat, menu_adjust, incl_np, rho_by_g);
   
    platforms = {'direct', 'dd', 'uber', 'gh', 'pm'};
    NF = length(platforms);

    if did
        Taus = {'chain', 'indep'};
        NT = length(Taus);
        for f = 1:NF
            platform_f = platforms{f};
            for nt = 1:NT
                tau = Taus{nt};

                cap_var0   = sprintf('rho_%s_15', platform_f);
                nocap_var0 = sprintf('rho_%s_30', platform_f);

                cap_var1   = sprintf('rho_%s_cap_%s', tau, platform_f);
                nocap_var1 = sprintf('rho_%s_%s',     tau, platform_f);

                dat.(cap_var1)   = dat.(cap_var0);
                dat.(nocap_var1) = dat.(nocap_var0);
            end
        end
    elseif resub2
        idx_nyc = dat.m == 1;
        for f = 1:NF
            platform_f = platforms{f};
            
            cap_var0   = sprintf('rho_%s_15', platform_f);
            cap_alt0   = sprintf('rho_%s_20', platform_f);
            nocap_var0 = sprintf('rho_%s_30', platform_f);

            cap_var1   = sprintf('rho_cap_%s', platform_f);
            nocap_var1 = sprintf('rho_%s',     platform_f);

            dat.(cap_var1)   = dat.(cap_var0);
            dat.(nocap_var1) = dat.(nocap_var0);

            % Override 15% cap with 20% cap for NYC
            dat.(cap_var1)(idx_nyc) = dat.(cap_alt0)(idx_nyc);
        end
    end

    if resub2 
        [prices_cap, prices_nocap] = ConstructPriceMatrix(dat);
    elseif hedonic || psample || did
        [prices_cap, prices_nocap] = ConstructHedonicMatrix(dat);
    elseif rho_by_g
        % Price matrix
        [prices_cap, prices_nocap] = ConstructPriceMatrix(dat);
    else 
        prices_cap   = 0;
        prices_nocap = 0;
    end

    % Compute counterfactual fees for the case in which there is a
    % commission cap and the case in which there is not a commission cap
    %== Preliminaries ==%
    comm_fx = DiD.fees(1);
    comm_change_cap   = -0.15;
    comm_change_nocap = 0.15;
    change_factor_cap = exp(comm_fx*comm_change_cap);
    change_factor_nocap = exp(comm_fx*comm_change_nocap);

    baseline_fee = fees(:, 2:end);
    
    %== CF no cap to cap ==%
    % For each platform, compute the total ordering cost
    baseline_rho = prices_nocap(:, 2:end);
    baseline_combo = baseline_rho + baseline_fee;
    change_amt = (change_factor_cap - 1)*baseline_combo;
    % Add to fees
    fees_CF1 = fees;
    fees_CF1(:, 2:end) = fees_CF1(:, 2:end) + change_amt;

    %== CF cap to no cap ==%
    baseline_rho = prices_cap(:, 2:end);
    baseline_combo = baseline_rho + baseline_fee;
    change_amt = (change_factor_nocap - 1)*baseline_combo;
    % Add to fees
    fees_CF2 = fees;
    fees_CF2(:, 2:end) = fees_CF2(:, 2:end) + change_amt;

    fees_nocap = fees;
    fees_cap   = fees;
    has_cap = dat.cap < 0.30;
    fees_nocap(has_cap, :)  = fees_CF2(has_cap, :);
    fees_nocap(~has_cap, :) = fees(~has_cap, :);
    fees_cap(has_cap, :)    = fees(has_cap, :);
    fees_cap(~has_cap, :)   = fees_CF1(~has_cap, :);


    WT = ConstructWaitingTimeMatrix(dat);

    % CBSA matrix
    CBSA_mat = zeros(n_panelist, n_cbsa);
    for m = 1:n_cbsa
       CBSA_mat(:, m) = 1*(dat.m == m);
    end
   
    % Demographics
    if ~spec_opt.lambda_inc
        demo = [dat.young, dat.married];
    else
        demo = [dat.young, dat.married, dat.high_income];
    end
    n_demo = size(demo, 2);

    % Platform portfolio counts
    [JG,       JG_f]       = ConstructJGMatrix(dat, 'all');
    [JG_chain, JG_f_chain] = ConstructJGMatrix(dat, 'chain');
    [JG_indep, JG_f_indep] = ConstructJGMatrix(dat, 'indep');

    % Decomposed by cap status
    if rho_by_g
        % Cap
        [JG_cap,      JG_f_cap]      = ConstructJGMatrix(dat, 'cap');
        [JG_capchain, JG_f_capchain] = ConstructJGMatrix(dat, 'capchain');
        [JG_capindep, JG_f_capindep] = ConstructJGMatrix(dat, 'capindep');

        % No cap
        [JG_nocap,      JG_f_nocap]      = ConstructJGMatrix(dat, 'nocap');
        [JG_nocapchain, JG_f_nocapchain] = ConstructJGMatrix(dat, 'nocapchain');
        [JG_nocapindep, JG_f_nocapindep] = ConstructJGMatrix(dat, 'nocapindep');
    else
        JG_cap   = 0;
        JG_f_cap = 0;

        JG_capchain   = 0;
        JG_f_capchain = 0;

        JG_capindep   = 0;
        JG_f_capindep = 0;

        JG_nocap   = 0;
        JG_f_nocap = 0;

        JG_nocapchain   = 0;
        JG_f_nocapchain = 0;

        JG_nocapindep   = 0;
        JG_f_nocapindep = 0;
    end
    
    % Generate a matrix encoding which platforms belong to which portfolios
    vnames = dat.Properties.VariableNames;
    nvars = length(vnames);
    idx_g = arrayfun(@(k) length(regexp(vnames{k}, '^G[01]{4}$')), 1:nvars);
    idx_g = find(logical(idx_g));
    n_portfolios = length(idx_g);
    portfolios = zeros(n_portfolios, n_platforms);
    for k = 1:n_portfolios
        idx_k = idx_g(k);
        var_k = vnames{idx_k};
        spec_k = extractBetween(var_k, 2, length(var_k));
        spec_k = spec_k{1};
        for p = 1:n_platforms
            in_p = extractBetween(spec_k, p, p);
            in_p = in_p{1};
            if strcmp(in_p, '1')
               portfolios(k, p) = 1; 
            end
        end
    end
    
    % Version of portfolios matrix with the outside platform, which is
    % assumed to belong to every portfolio
    portfolios0 = [ones(n_portfolios, 1),  portfolios];
    
    % Take unobservable taste draws
    % "_p" signifies "panelist-level"
    if IS
        % Importance sampling
        [Zeta_dag, Zeta_tilde, Eta, W] = DrawXi_IS_v2(n_panelist, n_platforms, ...
            num_opt.nsim);
    else
        Zeta_dag   = randn(n_panelist, 1,           num_opt.nsim);
        Zeta_tilde = randn(n_panelist, n_platforms, num_opt.nsim);
        if incl_np
            % These Etas are not recentred or rescaled
            Eta     = randn(n_panelist, num_opt.nsim);
            EtaIdio = randn(num_opt.neta, 1);
        end
        W = ones(n_panelist, num_opt.nsim);
    end

    if rest_RC
        % Random effects on chain preference
        Phi_i = randn(n_panelist, num_opt.nsim);
    else
        Phi_i = zeros(n_panelist, num_opt.nsim);
    end

    % Choice counts
    choice_counts = table2array(dat(:, {'f0', 'f1', 'f2', 'f3', 'f4'}));
    if any(strcmp(dat.Properties.VariableNames, 'f1c'))
        choice_counts_c = table2array(dat(:, {'f0c', 'f1c', 'f2c', 'f3c', 'f4c'}));
        choice_counts_i = table2array(dat(:, {'f0i', 'f1i', 'f2i', 'f3i', 'f4i'}));
    else
        choice_counts_c = [];
        choice_counts_i = [];
    end
    N_chain = sum(choice_counts_c(:, 2:end), 2);
    N_indep = sum(choice_counts_i(:, 2:end), 2);

    % Place data objects into a struct
    data_objs = struct();
    data_objs.fees = fees;
    data_objs.fees_nocap = fees_nocap;
    data_objs.fees_cap   = fees_cap;
    data_objs.prices_nocap = prices_nocap;
    data_objs.prices_cap   = prices_cap;
    data_objs.WT = WT;
    data_objs.choice_counts   = choice_counts;
    data_objs.choice_counts_c = choice_counts_c;
    data_objs.choice_counts_i = choice_counts_i;
    data_objs.N_chain         = N_chain;
    data_objs.N_indep         = N_indep;
    data_objs.demo = demo;

    % Include the DiD results
    data_objs.DiD = DiD;
    
    % Nearby restaurants with various portfolios
    data_objs.JG   = JG;
    data_objs.JG_f = JG_f;
    % Chain restaurants
    data_objs.JG_chain   = JG_chain;
    data_objs.JG_f_chain = JG_f_chain;
    % Independent restaurants
    data_objs.JG_indep   = JG_indep;
    data_objs.JG_f_indep = JG_f_indep;

    % ... with cap
    data_objs.JG_cap   = JG_cap;
    data_objs.JG_f_cap = JG_f_cap;
    % Chains
    data_objs.JG_capchain   = JG_capchain;
    data_objs.JG_f_capchain = JG_f_capchain;
    % Independents
    data_objs.JG_capindep   = JG_capindep;
    data_objs.JG_f_capindep = JG_f_capindep;

    % ... without cap
    data_objs.JG_nocap   = JG_nocap;
    data_objs.JG_f_nocap = JG_f_nocap;
    % Chains
    data_objs.JG_nocapchain   = JG_nocapchain;
    data_objs.JG_f_nocapchain = JG_f_nocapchain;
    % Independents
    data_objs.JG_nocapindep   = JG_nocapindep;
    data_objs.JG_f_nocapindep = JG_f_nocapindep;

    % Log versions
    data_objs.log_JG_capchain   = log(data_objs.JG_capchain);
    data_objs.log_JG_nocapchain = log(data_objs.JG_nocapchain);
    data_objs.log_JG_capindep   = log(data_objs.JG_capindep);
    data_objs.log_JG_nocapindep = log(data_objs.JG_nocapindep);

    data_objs.portfolios0 = portfolios0;
    data_objs.n_cbsa = n_cbsa;
    data_objs.n_demo = n_demo;
    data_objs.n_platforms = n_platforms;
    data_objs.n_portfolios = n_portfolios;
    data_objs.F = F;
    data_objs.CBSA_mat = CBSA_mat;

    data_objs.has_cap = 1*(dat.cap < 0.30);

    % Persistent tastes
    data_objs.Zeta_dag   = Zeta_dag;
    data_objs.Zeta_tilde = Zeta_tilde;
    data_objs.W          = W;
    if incl_np
        data_objs.Eta = Eta;
        if idio_eta
            data_objs.EtaIdio = EtaIdio;
        end
        data_objs.T_i = spec_opt.T_i; 

        % Add the number of nonpurchases
        T_in = dat.T_i;
        T_out = spec_opt.T_i - T_in;
        T_out(T_out < 0) = 0;
        data_objs.T_out = T_out;
        data_objs.choice_counts = [T_out, data_objs.choice_counts];
    end

    data_objs.Phi_i = Phi_i;

    if het_alpha || het_extra
        data_objs.low_inc = 1 - dat.high_income;
        data_objs.high_inc = dat.high_income;
    end
end

