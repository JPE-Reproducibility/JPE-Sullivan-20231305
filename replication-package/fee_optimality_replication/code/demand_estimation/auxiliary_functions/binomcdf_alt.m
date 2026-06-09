function F = binomcdf_alt(K, T, p)
%BINOMCDF_ALT  Fast exact binomial CDF for scalar K, T and array p.
%   F = BINOMCDF_ALT(K, T, p) returns P(X <= K) for X ~ Binomial(T, p),
%   where p can be any-sized array. K and T must be scalars.
%
%   This is optimised for small T (e.g. T=10) and small-ish K, and is
%   faster than MATLAB's binocdf on large arrays of p.

    % Ensure p is in [0,1]
    p = max(min(p, 1), 0);

    % Trivial bounds on K
    if K < 0
        F = zeros(size(p), 'like', p);
        return
    end
    if K >= T
        F = ones(size(p), 'like', p);
        return
    end

    F = zeros(size(p), 'like', p);

    % Handle p = 0 and p = 1 separately to avoid 0/0 etc.
    mask0 = (p == 0);
    mask1 = (p == 1);
    mask  = ~(mask0 | mask1);

    % p = 0 => X = 0 with prob 1, so CDF(K) = 1 for any K >= 0
    if any(mask0(:))
        F(mask0) = 1;
    end

    % p = 1 => X = T with prob 1, so CDF(K) = 0 for K < T (and we already
    % excluded K >= T above)
    if any(mask1(:))
        F(mask1) = 0;
    end

    % Interior case: 0 < p < 1
    if any(mask(:))
        pp = p(mask);
        qq = 1 - pp;

        % Start at j = 0: pmf(0) = (1-p)^T
        pmf = qq.^T;
        cdf = pmf;   % cumulative from j = 0

        % Recursively build pmf(j) from pmf(j-1):
        % pmf(j) = pmf(j-1) * ( (T-j+1)/j ) * (p/(1-p) )
        for j = 1:K
            pmf = pmf .* ((T - j + 1)/j) .* (pp ./ qq);
            cdf = cdf + pmf;
        end

        F(mask) = cdf;
    end
end
