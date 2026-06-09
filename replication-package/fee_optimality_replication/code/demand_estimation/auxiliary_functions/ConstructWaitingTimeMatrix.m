function WT = ConstructWaitingTimeMatrix(dat)
    % Compute matrix of waiting times that has same column ordering as
    % the platforms cell array
    
    % Compute dimensions
    platforms = {'dd', 'uber', 'gh', 'pm'};
    n_platforms = length(platforms);
    n_panelist = size(dat, 1);
    
    % Fill in waiting times matrix
    WT = zeros(n_panelist, n_platforms);
    for f = 1:n_platforms
        vname = sprintf('%s_WT', platforms{f});
        WT(:, f) = dat.(vname);
    end
    % Convert to hours
    WT = WT/60;
end
