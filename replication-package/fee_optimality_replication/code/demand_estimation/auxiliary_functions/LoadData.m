function [dat, month_tab, n_cbsa, mean_x] = LoadData(inpath, spec_opt)
    % Load data used in estimating the consumer choice model
    dat = readtable(inpath, 'ReadVariableNames', true);
    dat = dat(dat.J_total > 0, :);
    
    if spec_opt.market_subset 
        if spec_opt.n_subset == 1
            idx = dat.m == 8;
            dat = dat(idx, :);
            % relabel CBSAs
            dat.m(dat.m == 8) = 1;
        elseif spec_opt.n_subset == 2
            idx = dat.m == 8 | dat.m == 6;
            dat = dat(idx, :);
            % relabel CBSAs
            dat.m(dat.m == 8) = 1;
            dat.m(dat.m == 6) = 2;
        elseif spec_opt.n_subset == 3
            idx = dat.m == 8 | dat.m == 6 | dat.m == 3;
            dat = dat(idx, :);
            % relabel CBSAs
            dat.m(dat.m == 8) = 1;
            dat.m(dat.m == 6) = 2;
        elseif spec_opt.n_subset == 4
            idx = dat.m == 8 | dat.m == 6 | dat.m == 3 | dat.m == 4;
            dat = dat(idx, :);
            % relabel CBSAs
            dat.m(dat.m == 8) = 1;
            dat.m(dat.m == 6) = 2;
        elseif spec_opt.n_subset == 5
            idx = dat.m == 8 | dat.m == 6 | dat.m == 3 | dat.m == 4 | dat.m == 1;
            dat = dat(idx, :);
            % relabel CBSAs
            dat.m(dat.m == 1) = 5;
            dat.m(dat.m == 8) = 1;
            dat.m(dat.m == 6) = 2;
            
        end
    end

    n_cbsa = length(unique(dat.m));


    %== Compute mean fees, waiting times, and prices ==%
    % Fees, waiting times, and prices
    fees   = [dat.dd_price, dat.uber_price, dat.gh_price, dat.pm_price];
    WTs    = [dat.dd_WT,    dat.uber_WT,    dat.gh_WT,    dat.pm_WT];
    prices = [dat.rho_direct_30, dat.rho_dd_30, dat.rho_uber_30, ...
              dat.rho_gh_30, dat.rho_pm_30];
    NF = size(fees, 2);

    % Initialize outputs
    mean_fees   = zeros(NF,     n_cbsa);
    mean_WTs    = zeros(NF,     n_cbsa);
    mean_prices = zeros(NF + 1, n_cbsa);

    for k = 1:n_cbsa
        %== Compute the mean fee, WT, and price for each platform ==%
        % Find observations from the metro in question
        idx_k = dat.m == k;
       
        % Intialize outputs
        fees_k   = zeros(NF, 1);
        WT_k     = zeros(NF, 1);
        prices_k = zeros(NF + 1, 1);

        for f = 1:NF
            fees_k(f) = mean(fees(idx_k, f));
            WT_k(f)   = mean(WTs(idx_k, f));
            prices_k(f + 1) = mean(prices(idx_k, f + 1));
        end
        prices_k(1) = mean(prices(idx_k, 1));

        % Place in array
        mean_fees(:, k)   = fees_k;
        mean_WTs(:, k)    = WT_k;
        mean_prices(:, k) = prices_k;
    end

    mean_x = struct();
    mean_x.fees   = mean_fees;
    mean_x.WTs    = mean_WTs;
    mean_x.prices = mean_prices;

    if spec_opt.demean
        for k = 1:n_cbsa
            idx_k = dat.m == k;

            % Fees
            fees_k = mean_x.fees(:, k);
            dat.dd_price(idx_k)   = dat.dd_price(idx_k)   - fees_k(1);
            dat.uber_price(idx_k) = dat.uber_price(idx_k) - fees_k(2);
            dat.gh_price(idx_k)   = dat.gh_price(idx_k)   - fees_k(3);
            dat.pm_price(idx_k)   = dat.pm_price(idx_k)   - fees_k(4);

            % Waiting times
            WT_k = mean_x.WTs(:, k);
            dat.dd_WT(idx_k)   = dat.dd_WT(idx_k)   - WT_k(1);
            dat.uber_WT(idx_k) = dat.uber_WT(idx_k) - WT_k(2);
            dat.gh_WT(idx_k)   = dat.gh_WT(idx_k)   - WT_k(3);
            dat.pm_WT(idx_k)   = dat.pm_WT(idx_k)   - WT_k(4);

            % Prices
            prices_k = mean_x.prices(:, k);
            dat.rho_direct_10(idx_k)   = dat.rho_direct_10(idx_k) - prices_k(1);
            dat.rho_direct_15(idx_k)   = dat.rho_direct_15(idx_k) - prices_k(1);
            dat.rho_direct_18(idx_k)   = dat.rho_direct_18(idx_k) - prices_k(1);
            dat.rho_direct_20(idx_k)   = dat.rho_direct_20(idx_k) - prices_k(1);
            dat.rho_direct_30(idx_k)   = dat.rho_direct_30(idx_k) - prices_k(1);

            dat.rho_dd_10(idx_k)   = dat.rho_dd_10(idx_k) - prices_k(2);
            dat.rho_dd_15(idx_k)   = dat.rho_dd_15(idx_k) - prices_k(2);
            dat.rho_dd_18(idx_k)   = dat.rho_dd_18(idx_k) - prices_k(2);
            dat.rho_dd_20(idx_k)   = dat.rho_dd_20(idx_k) - prices_k(2);
            dat.rho_dd_30(idx_k)   = dat.rho_dd_30(idx_k) - prices_k(2);

            dat.rho_uber_10(idx_k)   = dat.rho_uber_10(idx_k) - prices_k(3);
            dat.rho_uber_15(idx_k)   = dat.rho_uber_15(idx_k) - prices_k(3);
            dat.rho_uber_18(idx_k)   = dat.rho_uber_18(idx_k) - prices_k(3);
            dat.rho_uber_20(idx_k)   = dat.rho_uber_20(idx_k) - prices_k(3);
            dat.rho_uber_30(idx_k)   = dat.rho_uber_30(idx_k) - prices_k(3);

            dat.rho_gh_10(idx_k)   = dat.rho_gh_10(idx_k) - prices_k(4);
            dat.rho_gh_15(idx_k)   = dat.rho_gh_15(idx_k) - prices_k(4);
            dat.rho_gh_18(idx_k)   = dat.rho_gh_18(idx_k) - prices_k(4);
            dat.rho_gh_20(idx_k)   = dat.rho_gh_20(idx_k) - prices_k(4);
            dat.rho_gh_30(idx_k)   = dat.rho_gh_30(idx_k) - prices_k(4);

            dat.rho_pm_10(idx_k)   = dat.rho_pm_10(idx_k) - prices_k(5);
            dat.rho_pm_15(idx_k)   = dat.rho_pm_15(idx_k) - prices_k(5);
            dat.rho_pm_18(idx_k)   = dat.rho_pm_18(idx_k) - prices_k(5);
            dat.rho_pm_20(idx_k)   = dat.rho_pm_20(idx_k) - prices_k(5);
            dat.rho_pm_30(idx_k)   = dat.rho_pm_30(idx_k) - prices_k(5);       
        end
    end

    % Limit months
    dat = dat(ismember(dat.month, spec_opt.which_months), :);
    month_tab = tabulate(dat.month);

    % Ensure that the data is sorted by ID
    dat = sortrows(dat, 'id');

    % Censor over T_i restaurant purchases
    if spec_opt.incl_np
        dat = dat(dat.T_i <= spec_opt.T_i, :);
    end

    sample_rate = spec_opt.sample_rate;
    if sample_rate < 1
        nsample = round(sample_rate*size(dat, 1));
        idx = randsample(size(dat, 1), nsample, false);
        dat = dat(idx, :);
    end
end

