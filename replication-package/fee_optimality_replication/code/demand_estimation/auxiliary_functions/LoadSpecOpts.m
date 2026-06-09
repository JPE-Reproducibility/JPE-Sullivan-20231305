function spec_opt = LoadSpecOpts
    spec_opt = struct();
    % Include non-partenered restaurants?
    spec_opt.use_nonp = false;
    % Use a subset of markets?
    spec_opt.market_subset = true;
    spec_opt.n_subset = 3;%4;
    % Adjust for menu prices?
    spec_opt.menu_adjust = true;
    % Alternative parameterization?
    % 1 for ratios with gamma, and estimate 1/gamma instead of gamma
    % 2 for ratios with gamma, and estimate gamma directly
    spec_opt.alt_param = 2;
    % Include no purchase option?
    spec_opt.incl_np = true;
    % Demographic effects for no purchase option?
    spec_opt.demo_np = true;
    % Separate mu_eta for each market?
    spec_opt.eta_by_m = true;
    % If incl_nl = true, we need to specify a number of potential orders
    spec_opt.T_i = 20;
    % Heterogenous alpha?
    spec_opt.het_alpha = true;
    % Nested logit?
    spec_opt.use_NL = false;
    % Idiosyncratic eta shock
    spec_opt.idio_eta = false;
    % Control for waiting time?
    spec_opt.use_WT = true;
    % Use simulation to integrate eta idio?
    spec_opt.simulate_eta = false;
    % Set quadrature options
    spec_opt.M = 4; % number of quadrature points = 2*M + 1
    % Static panelists only?
    spec_opt.static = false;
    % Connected panelists only? (Takes precedence over static)
    spec_opt.connect = false;
    % Include zero counts?
    spec_opt.include_zeros = true;
    % Which months?
    spec_opt.which_months = [4, 5, 6];
    % Use menu prices adjusted for commission cap?l
    spec_opt.alt_prices = true;
    % Subsetting for low-value transactions?
    spec_opt.sset = true;

    % Use hedonic price regression estimates?
    spec_opt.hedonic = false;
    
    % Smoothing parameter
    spec_opt.sigma_eps = 0.05;
    
    % Menu prices that vary by G and by platform?
    spec_opt.rho_by_g = true;
end
