function boot_tab = DrawBootstrapData(dat)
    % Draw a bootstrap sample from each market in dat
    
    n_cbsa = length(unique(dat.m));
    boot_dat = cell(n_cbsa, 1);

    for m = 1:n_cbsa
        idx_m = dat.m == m;
        dat_m = dat(idx_m, :);
        nm = size(dat_m, 1);      
        boot_dat_m = datasample(dat_m, nm);
        boot_dat_m.id = 1e7*m + (1:nm)';
        boot_dat{m} = boot_dat_m;
    end

    boot_tab = vertcat(boot_dat{1}, boot_dat{2});
    if n_cbsa > 2
        for m = 3:n_cbsa
            boot_tab = vertcat(boot_tab, boot_dat{m});
        end
    end
end
