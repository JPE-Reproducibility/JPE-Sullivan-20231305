function [logL, log_li, chain_dist] = ChoiceModelLogLikelihood(theta_vec, theta_names, data_objs, verbose, spec_opt)
    % Choice model log-likelihood

    if nargin < 4
        verbose = true;
    end
   
    if spec_opt.use_simple
        [prob_f, chain_probs] = ComputeChoiceProbs_simple(theta_vec, theta_names, data_objs, spec_opt);
    else
        [prob_f, chain_probs] = ComputeChoiceProbs_faster(theta_vec, theta_names, data_objs, spec_opt);
    end

    if spec_opt.match_chain
        % Compute probability of chain purchase (model)
        prob_online = squeeze(sum(prob_f(:, 3:end, :), 2));
        prob_chain_online = mean(mean(chain_probs.*prob_online))/mean(mean(prob_online));
    
        % Compute probability of chain purchase (data)
        nbuy_chain = sum(sum(data_objs.choice_counts_c(:, 2:end)));
        nbuy_indep = sum(sum(data_objs.choice_counts_i(:, 2:end)));
        nbuy_tot   = sum(sum(data_objs.choice_counts(:, 3:end)));
        freq_chain_online = nbuy_chain/nbuy_tot;

        chain_dist = freq_chain_online - prob_chain_online;
        dist_chain_online = 20*chain_dist^2;
    end

    likeli = prob_f.^(data_objs.choice_counts);
    likeli = squeeze(prod(likeli, 2));
    % Integrate across simulation draws
    if spec_opt.IS
        likeli = mean(likeli.*data_objs.W, 2);
    else
        likeli = mean(likeli, 2);
    end
    likeli = mean(likeli, 2);
    log_li = log(likeli + 1e-22);
    logL = mean(log_li);
    
    % To avoid negative gamma
    gamma_idx = strcmp(theta_names, 'gamma');
    gamma     = theta_vec(gamma_idx);
    if gamma < 0.2
        logL = logL - 10*exp(0.2 - gamma);
    end

    if spec_opt.match_chain
        logL = logL - dist_chain_online;
    end
    
    if verbose
        disp(table(reshape(theta_vec, numel(theta_vec), 1), theta_names));
        fprintf('logL = %f\n', logL);
    end
end
