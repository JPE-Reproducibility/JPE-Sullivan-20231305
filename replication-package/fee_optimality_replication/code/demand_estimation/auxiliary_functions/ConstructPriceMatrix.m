 function [prices_cap, prices_nocap] = ConstructPriceMatrix(dat)
    % Construct matrix of prices without chain/independent distinction

    platforms = {'direct', 'dd', 'uber', 'gh', 'pm'};
    n_platforms = length(platforms);
    n_panelist = size(dat, 1);
    
    % Initialize the price matrix
    prices_cap   = zeros(n_panelist, n_platforms);
    prices_nocap = zeros(n_panelist, n_platforms);
    
    for f = 1:n_platforms
        platform = platforms{f};
        if strcmp(platform, 'pm')
            plabel = 'uber';
        else
            plabel = platform;
        end
        vname_cap   = sprintf('rho_cap_%s', plabel);
        vname_nocap = sprintf('rho_%s',     plabel);
        prices_cap(:, f)   = dat.(vname_cap);
        prices_nocap(:, f) = dat.(vname_nocap);
    end
end

