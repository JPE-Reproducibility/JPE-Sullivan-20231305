function gs_a = ComputeSimMoments(theta_vec, theta_names, data_objs, spec_opt)

    if spec_opt.use_simple
        [prob_f, chain_probs] = ComputeChoiceProbs_simple(theta_vec, theta_names, data_objs, spec_opt);
    else
        [prob_f, chain_probs] = ComputeChoiceProbs_faster(theta_vec, theta_names, data_objs, spec_opt);
    end
    
    gs_a = ComputeMoments_v3(data_objs, spec_opt, prob_f, chain_probs, ...
                             theta_vec, theta_names);
end
