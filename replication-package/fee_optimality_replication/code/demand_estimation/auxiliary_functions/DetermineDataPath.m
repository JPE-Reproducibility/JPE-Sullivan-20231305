function inpath = DetermineDataPath(spec_opt, price_version)
    % Determine path to the estimation sample
    if nargin < 2
        price_version = '_v3';
    end
    
    suffix = '';
    if spec_opt.use_nonp
        suffix = strcat(suffix, '_nonp');
    end
    suffix = strcat(suffix, price_version);
    
  
    if isfield(spec_opt, 'menu_price_type')
        mpt = spec_opt.menu_price_type;
        hedonic = 0;
        psample = 0;
        did     = 0;
        resub2  = 0;
        if (mpt == 1)
            suffix = strcat(suffix, '_hedonic');
            hedonic = 1;
        elseif (mpt == 2)
            suffix = strcat(suffix, '_psample');
            psample = 1;
        elseif (mpt == 3)
            suffix = strcat(suffix, '_did');
            did = 1;
        elseif (mpt == 4)
            suffix = strcat(suffix, '_did');
            resub2 = 1;
        end
    else
        hedonic = 1;
        psample = 0;
        did     = 0;
        resub2  = 0;
    end

    if spec_opt.sset
        suffix = strcat(suffix, '_sset');
    end

    if isfield(spec_opt, 'rho_by_g') && ~hedonic && ~psample && ~did && ~resub2
        if spec_opt.rho_by_g
            suffix = strcat(suffix, '_rhoByG');
        end
    end
    
    if isfield(spec_opt, 'include_zeros')
        if spec_opt.include_zeros
            suffix = strcat(suffix, '_0s');
        end
    end
    
    if spec_opt.connect
        inpath = sprintf('../../data/est_dat/connect_sample%s.csv', suffix);
    elseif isfield(spec_opt, 'static')
        inpath = sprintf('../../data/est_dat/static_sample%s.csv', suffix);
    else
        inpath = sprintf('../../data/est_dat/combined_sample%s.csv', suffix);
    end
end
