 function [prices_cap, prices_nocap] = ConstructHedonicMatrix(dat)
    % Construct matrix of hedonic price indices for chain and for
    % independent restaurants
    platforms = {'direct', 'dd', 'uber', 'gh', 'pm'};
    n_platforms = length(platforms);
    n_panelist = size(dat, 1);
    
    rtypes = {'chain'; 'indep'};
    NTau = length(rtypes); % Number of restaurant types
    
    % Initialize the price matrix
    prices_cap   = zeros(n_panelist, n_platforms, NTau);
    prices_nocap = zeros(n_panelist, n_platforms, NTau);
    
    for g = 1:NTau
        tau = rtypes{g};
        
        for f = 1:n_platforms
            platform = platforms{f};
            if strcmp(platform, 'pm')
                plabel = 'uber';
            else
                plabel = platform;
            end
            vname_cap   = sprintf('rho_%s_cap_%s', tau, plabel);
            vname_nocap = sprintf('rho_%s_%s', tau, plabel);
            prices_cap(:, f, g)   = dat.(vname_cap);
            prices_nocap(:, f, g) = dat.(vname_nocap);
        end
    end  
end

