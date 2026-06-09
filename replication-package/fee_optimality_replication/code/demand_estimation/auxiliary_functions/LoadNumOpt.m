function num_opt = LoadNumOpt()
    % Numerical options
    num_opt = struct();
    num_opt.maxit     = 1e5;
    num_opt.maxfevals = 1e5;
    num_opt.nsim      = 300;%500;
    num_opt.neta      = 100; % Number of \tilde eta draws
end