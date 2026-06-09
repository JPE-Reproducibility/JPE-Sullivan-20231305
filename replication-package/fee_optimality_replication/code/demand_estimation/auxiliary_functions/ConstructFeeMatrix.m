function fees = ConstructFeeMatrix(dat, menu_adjust, incl_np, rho_by_g)
    % Compute matrix of prices that has same column ordering as
    % the platforms cell array
    % 
    % Inputs
    %   dat: data
    %   menu_adjust: adjust the fees for menu prices?
    %   incl_np: include the no-purchase option?
    %   rho_by_g: prices by platform portfolio? (do not adjust in this case)
    
    % Compute dimensions
    platforms = {'dd', 'uber', 'gh', 'pm'};
    n_platforms = length(platforms);
    F = n_platforms + 1;
    n_panelist = size(dat, 1);
    
    % Fill in fee matrix
    fees = zeros(n_panelist, F);
    for f = 1:n_platforms
        vname = sprintf('%s_price', platforms{f});
        fees(:, f + 1) = dat.(vname);

        if menu_adjust && incl_np && ~rho_by_g
            fees(:, f + 1) = fees(:, f + 1) + dat.rho_online;
        elseif ~rho_by_g
            % Enter difference of online and offline price
            fees(:, f + 1) = fees(:, f + 1) + dat.rho_online - dat.rho_offline;
        end
    end

    if menu_adjust && incl_np && ~rho_by_g
        fees(:, 1) = dat.rho_offline;
    end
end
