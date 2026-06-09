function param_names = generate_name_array(varname, dx, dy)
    % Generate an array of parameter names
    param_names = cell(dx, dy);
    for kx = 1:dx
        for ky = 1:dy
            param_names{kx, ky} = ...
                sprintf('%s_%d-%d', varname, kx, ky);
        end
    end
end
