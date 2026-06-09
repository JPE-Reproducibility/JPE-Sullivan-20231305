function [JG, JG_f] = ConstructJGMatrix(dat, G_subset)
    % Construct matrix with counts of nearby restaurants
    %
    % Options for G_subset:
    %   all: produce a matrix of counts of all restaurants
    %   cap: ... of restaurants facing commission caps
    %   nocap: ... of restaurants not facing commission caps
    %   chain: ... of chain restaurants
    %   indep: ... of independent restaurants
    %   capchain: ... of chain restaurants subject to caps
    %   nocapchain: ... of chain restaurants not subject to caps
    %   capindep: ... of independent restaurants subject to caps
    %   nocapindep: ... of independent restaurants not subject to caps

    vnames = dat.Properties.VariableNames;
    nvars = length(vnames);
  
    if strcmp(G_subset, 'all')
        idx_g = arrayfun(@(k) length(regexp(vnames{k}, '^G[01]{4}$')), 1:nvars);
        
        idx_f1 = arrayfun(@(k) length(regexp(vnames{k}, '^G1[01]{3}$')),     1:nvars);
        idx_f2 = arrayfun(@(k) length(regexp(vnames{k}, '^G[01]1[01]{2}$')), 1:nvars);
        idx_f3 = arrayfun(@(k) length(regexp(vnames{k}, '^G[01]{2}1[01]$')), 1:nvars);
        idx_f4 = arrayfun(@(k) length(regexp(vnames{k}, '^G[01]{3}1$')),     1:nvars);

    elseif strcmp(G_subset, 'cap')
        idx_g = arrayfun(@(k) length(regexp(vnames{k}, '^cap_G[01]{4}$')), 1:nvars);
         
        idx_f1 = arrayfun(@(k) length(regexp(vnames{k}, '^cap_G1[01]{3}$')),     1:nvars);
        idx_f2 = arrayfun(@(k) length(regexp(vnames{k}, '^cap_G[01]1[01]{2}$')), 1:nvars);
        idx_f3 = arrayfun(@(k) length(regexp(vnames{k}, '^cap_G[01]{2}1[01]$')), 1:nvars);
        idx_f4 = arrayfun(@(k) length(regexp(vnames{k}, '^cap_G[01]{3}1$')),     1:nvars);

    elseif strcmp(G_subset, 'nocap')
        idx_g = arrayfun(@(k) length(regexp(vnames{k}, '^nocap_G[01]{4}$')), 1:nvars);
         
        idx_f1 = arrayfun(@(k) length(regexp(vnames{k}, '^nocap_G1[01]{3}$')),     1:nvars);
        idx_f2 = arrayfun(@(k) length(regexp(vnames{k}, '^nocap_G[01]1[01]{2}$')), 1:nvars);
        idx_f3 = arrayfun(@(k) length(regexp(vnames{k}, '^nocap_G[01]{2}1[01]$')), 1:nvars);
        idx_f4 = arrayfun(@(k) length(regexp(vnames{k}, '^nocap_G[01]{3}1$')),     1:nvars);

    elseif strcmp(G_subset, 'chain')
        idx_g = arrayfun(@(k) length(regexp(vnames{k}, '^G[01]{4}_chain$')), 1:nvars);
         
        idx_f1 = arrayfun(@(k) length(regexp(vnames{k}, '^G1[01]{3}_chain$')),     1:nvars);
        idx_f2 = arrayfun(@(k) length(regexp(vnames{k}, '^G[01]1[01]{2}_chain$')), 1:nvars);
        idx_f3 = arrayfun(@(k) length(regexp(vnames{k}, '^G[01]{2}1[01]_chain$')), 1:nvars);
        idx_f4 = arrayfun(@(k) length(regexp(vnames{k}, '^G[01]{3}1_chain$')),     1:nvars);

    elseif strcmp(G_subset, 'indep')
        idx_g = arrayfun(@(k) length(regexp(vnames{k}, '^G[01]{4}_indep$')), 1:nvars);
         
        idx_f1 = arrayfun(@(k) length(regexp(vnames{k}, '^G1[01]{3}_indep$')),     1:nvars);
        idx_f2 = arrayfun(@(k) length(regexp(vnames{k}, '^G[01]1[01]{2}_indep$')), 1:nvars);
        idx_f3 = arrayfun(@(k) length(regexp(vnames{k}, '^G[01]{2}1[01]_indep$')), 1:nvars);
        idx_f4 = arrayfun(@(k) length(regexp(vnames{k}, '^G[01]{3}1_indep$')),     1:nvars);

    elseif strcmp(G_subset, 'capchain')
        idx_g = arrayfun(@(k) length(regexp(vnames{k}, '^cap_G[01]{4}_chain$')), 1:nvars);
         
        idx_f1 = arrayfun(@(k) length(regexp(vnames{k}, '^cap_G1[01]{3}_chain$')),     1:nvars);
        idx_f2 = arrayfun(@(k) length(regexp(vnames{k}, '^cap_G[01]1[01]{2}_chain$')), 1:nvars);
        idx_f3 = arrayfun(@(k) length(regexp(vnames{k}, '^cap_G[01]{2}1[01]_chain$')), 1:nvars);
        idx_f4 = arrayfun(@(k) length(regexp(vnames{k}, '^cap_G[01]{3}1_chain$')),     1:nvars);

    elseif strcmp(G_subset, 'capindep')
        idx_g = arrayfun(@(k) length(regexp(vnames{k}, '^cap_G[01]{4}_indep$')), 1:nvars);
         
        idx_f1 = arrayfun(@(k) length(regexp(vnames{k}, '^cap_G1[01]{3}_indep$')),     1:nvars);
        idx_f2 = arrayfun(@(k) length(regexp(vnames{k}, '^cap_G[01]1[01]{2}_indep$')), 1:nvars);
        idx_f3 = arrayfun(@(k) length(regexp(vnames{k}, '^cap_G[01]{2}1[01]_indep$')), 1:nvars);
        idx_f4 = arrayfun(@(k) length(regexp(vnames{k}, '^cap_G[01]{3}1_indep$')),     1:nvars);

    elseif strcmp(G_subset, 'nocapchain')
        idx_g = arrayfun(@(k) length(regexp(vnames{k}, '^nocap_G[01]{4}_chain$')), 1:nvars);
         
        idx_f1 = arrayfun(@(k) length(regexp(vnames{k}, '^nocap_G1[01]{3}_chain$')),     1:nvars);
        idx_f2 = arrayfun(@(k) length(regexp(vnames{k}, '^nocap_G[01]1[01]{2}_chain$')), 1:nvars);
        idx_f3 = arrayfun(@(k) length(regexp(vnames{k}, '^nocap_G[01]{2}1[01]_chain$')), 1:nvars);
        idx_f4 = arrayfun(@(k) length(regexp(vnames{k}, '^nocap_G[01]{3}1_chain$')),     1:nvars);

    elseif strcmp(G_subset, 'nocapindep')
        idx_g = arrayfun(@(k) length(regexp(vnames{k}, '^nocap_G[01]{4}_indep$')), 1:nvars);
         
        idx_f1 = arrayfun(@(k) length(regexp(vnames{k}, '^nocap_G1[01]{3}_indep$')),     1:nvars);
        idx_f2 = arrayfun(@(k) length(regexp(vnames{k}, '^nocap_G[01]1[01]{2}_indep$')), 1:nvars);
        idx_f3 = arrayfun(@(k) length(regexp(vnames{k}, '^nocap_G[01]{2}1[01]_indep$')), 1:nvars);
        idx_f4 = arrayfun(@(k) length(regexp(vnames{k}, '^nocap_G[01]{3}1_indep$')),     1:nvars);
    end
    
    idx_g = find(logical(idx_g));
    JG = dat(:, idx_g);
    JG = table2array(JG);
    
    idx_f1 = find(logical(idx_f1));
    idx_f2 = find(logical(idx_f2));
    idx_f3 = find(logical(idx_f3));
    idx_f4 = find(logical(idx_f4));
    JG_f1 = sum(table2array(dat(:, idx_f1)), 2);
    JG_f2 = sum(table2array(dat(:, idx_f2)), 2);
    JG_f3 = sum(table2array(dat(:, idx_f3)), 2);
    JG_f4 = sum(table2array(dat(:, idx_f4)), 2);
    JG_f = [JG_f1, JG_f2, JG_f3, JG_f4];
end

