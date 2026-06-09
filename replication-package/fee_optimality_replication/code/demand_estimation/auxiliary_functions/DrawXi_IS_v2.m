function [Zeta_dag, Zeta_tilde, Eta, W] = DrawXi_IS_v2(n_panelist, n_platforms, nsim)
    %
    % Outputs
    %   Zeta_dag: general shock
    %   Zeta_tilde: platform-specific shock
    %   Eta: restaurant dining shock
    %   W: weights

    % Set parameters (
    tau = 3; % Governs overall acceptance rate
    coefs = [2; 1.3; 1.3; 1.8; 3.0; 1]; % platform, dd, uber, gh, pm, restaurant
    d_Xi = length(coefs); % dimension of unobservable
    sigma = 3; % scale factor

    Zeta_dag   = zeros(n_panelist, 1,           nsim);
    Zeta_tilde = zeros(n_panelist, n_platforms, nsim);
    Eta        = zeros(n_panelist, nsim);
    W          = zeros(n_panelist, nsim);

    % Compute the overall acceptance probability (required for 
    % computing adjustment weights)
    NS = 1e6;

    Xi = randn(NS, d_Xi);
    U = Xi*coefs;
    eU = exp((U - tau)/sigma);
    Scores = eU./(1 + eU);
    S = mean(Scores);

    % Expected number of draws to get nsim acceptances
    edraws = nsim*n_panelist/S;
    % Number of draws to actually take
    ndraws = round(edraws*2);
    % Take draws
    Xi = randn(ndraws, d_Xi);

    % Compute scores
    U = Xi*coefs;
    eU = exp((U - tau)/sigma);
    Scores = eU./(1 + eU);

    accept = Scores >= rand(ndraws, 1);

    acceptance_rate = mean(accept);
    fprintf('IS acceptance rate = %0.3f\n', acceptance_rate);

    idx = find(accept, nsim*n_panelist);
    if length(idx) < nsim*n_panelist
        warning('too few acceptances');
    end

    % Subset Xi
    Xi_sub = Xi(idx, :);
    Scores_sub = Scores(idx, :);

    % Load the panelist-level matrices
    start_idx = 1;
    end_idx   = nsim;

    % ESS: preallocate diagnostics
    ESS_vec      = zeros(n_panelist,1);
    ESS_ratiovec = zeros(n_panelist,1);

    for i = 1:n_panelist
        
        Zeta_dag_i = Xi_sub(start_idx:end_idx, 1);
        Zeta_til_i = Xi_sub(start_idx:end_idx, 2:end - 1);
        Eta_i      = Xi_sub(start_idx:end_idx, end);
        Scores_i   = Scores_sub(start_idx:end_idx, 1);

        Zeta_dag(i, 1, :)   = Zeta_dag_i';
        Zeta_tilde(i, :, :) = Zeta_til_i';
        Eta(i, :)           = Eta_i';

        W(i, :) = S*(reshape(Scores_i, 1, nsim)).^(-1);

        % Effective sample side calculation
        wi = W(i, :);
        s1 = sum(wi);
        s2 = sum(wi.^2);
        ess_i = (s1*s1)/s2;
        ESS_vec(i, 1) = ess_i;
        ESS_ratiovec(i, 1) = ess_i/nsim;

        % Increment indices
        start_idx = start_idx + nsim;
        end_idx   = end_idx   + nsim;
    end

   % ESS: report summary diagnostics
    fprintf('ESS per i: mean %.1f, median %.1f, min %.1f (of %d)\n', ...
            mean(ESS_vec), median(ESS_vec), min(ESS_vec), nsim);
    fprintf('ESS ratio: mean %.2f, min %.2f\n', ...
            mean(ESS_ratiovec), min(ESS_ratiovec));
    % Pooled ESS across all i (optional)
    w_all = W(:);
    ESS_all = (sum(w_all)^2) / sum(w_all.^2);
    fprintf('Pooled ESS = %.1f (of %d)\n', ESS_all, numel(w_all));

end

