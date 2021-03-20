#This file contains functions for post effect evaluation and validation

#Shorthand function for validating all data setups under post effect 1-10 mins.
function validate()
    validate_all(1:10, "NL", "Web")
    validate_all(1:10, "BE", "Web")
    validate_all(1:10, "NL", "App")
    validate_all(1:10, "BE", "App")

    validate_all_custom(1:10, "NL", "Web", type = "direct")
    validate_all_custom(1:10, "NL", "Web", type = "dir_and_search")
    validate_all_custom(1:10, "NL", "Web", type = "no_paid")

    validate_all_custom(1:10, "BE", "Web", type = "direct")
    validate_all_custom(1:10, "BE", "Web", type = "dir_and_search")
    validate_all_custom(1:10, "BE", "Web", type = "no_paid")

    validate_all_custom(1:10, "NL", "App", type = "direct")
    validate_all_custom(1:10, "NL", "App", type = "dir_and_search")
    validate_all_custom(1:10, "NL", "App", type = "no_paid")

    validate_all_custom(1:10, "BE", "App", type = "direct")
    validate_all_custom(1:10, "BE", "App", type = "dir_and_search")
    validate_all_custom(1:10, "BE", "App", type = "no_paid")
end

#Shorthand function for validating all regular data setups
function validate_all(smpl::UnitRange = 1:10, country::String = "NL", medium::String = "Web")
    for i in smpl
        validate_postEff(i, country, medium)
    end
end

#Shorthand function for validating all special data setups
function validate_all_custom(smpl::UnitRange = 1:10, country::String = "NL", medium::String = "Web"; type::String = "")
    for i in smpl
        validate_postEff_custom(i, country, medium, type=type)
    end
end

#Function to validate spot lift for certain post effect window, country and medium combination
function validate_postEff(w::Integer, country::String = "NL", medium::String = "Web")
    country, medium = lowercase(country), lowercase(medium)

    dt, ft = load_X(country)
    visits = vec(load_data(country, medium)[:,1])
    start = get_ad_amount(dt)
    posteff = genr_overlap(start, w, split=false)

    #Code for old optimization routine (better minima, but takes a lot longer):
    #f = x->impute_traffic(visits,posteff,x, set = "train")[2]
    #res = bboptimize(f; SearchRange=(0.0,1.0), NumDimensions = 10, MaxSteps = 10000, TraceInterval = 1000)
    #param,loss = best_candidate(res), best_fitness(res)
    #param = param/sum(param)
    #loss = round(loss, digits = 7)
    #bl, sl = impute_traffic(visits,posteff, param,set="test")

    #Code for new optimization
    bl, sl, info = impute_traffic_fast(visits, posteff, info = true)
    loss, param = round(info[1], digits = 7), info[2]
    diagnostics = [length(sl[sl.<0.0])/length(sl) sum(sl[sl.>0.0])/w sum(sl)/w median(sl[sl.>0.0]) mad(sl[sl.>0.0], normalize = true) sum(sl[sl.>0.0])]
    diagnostics_header = ["neg%" "avg_pos_min" "avg_min" "med_pos" "mad_pos" "sum_pos"]

    val , prop = averagePostEff(start, sl, w)
    title = "Average post effect $country $medium [$w]"
    #temp_plot = bar(val, title = title, label="Value", xlabel = "Post effect min.", ylabel = "Value", xticks = 1:1:w)
    temp_plot2 = bar(prop, title = title, label="Proportion", xlabel = "Post effect min.", ylabel = "Proportion", xticks = 1:1:w)
    prop = round.(prop / sum(prop), digits = 3)
    writedlm("Coolblue_greedy_$country"*"_$medium"*"_$w"*"_param_($loss).csv", param, ',')
    writedlm("Coolblue_greedy_$country"*"_$medium"*"_$w"*"_prop.csv", prop, ',')
    writedlm("Coolblue_greedy_$country"*"_$medium"*"_$w"*"_diagnostics.csv", [diagnostics_header; diagnostics], ',')
    writedlm("Coolblue_greedy_$country"*"_$medium"*"_$w"*".csv", ["Baseline" "Spotlift"; bl sl], ',')
    #png(temp_plot, "$title.png")
    png(temp_plot2, "$title"*"_prop.png")
end

#Function to validate spot lift for certain post effect window, country and medium combination (special data setups)
function validate_postEff_custom(w::Integer, country::String = "nl", medium::String = "web"; type::String = "")
    #type should be either "direct", "no_paid" or "dir_and_search"
    country, medium = lowercase(country), lowercase(medium)

    dt, ft = load_X(country)
    visits = vec(load_custom("$country"*"_$medium"*"_8min_"*"$type"*".csv")[:,1])
    start = get_ad_amount(dt)
    posteff = genr_overlap(start, w, split=false)

    #Code for old optimization routine (better minima, but takes a lot longer):
    #f = x->impute_traffic(visits,posteff,x, set = "train")[2]
    #res = bboptimize(f; SearchRange=(0.0,1.0), NumDimensions = 10, MaxSteps = 1000, TraceInterval = 60)
    #param,loss = best_candidate(res), best_fitness(res)
    #param = param/sum(param)
    #loss = round(loss, digits = 7)
    #bl, sl = impute_traffic(visits,posteff, param,set="test")

    #Code for new optimization
    bl, sl, info = impute_traffic_fast(visits, posteff, info = true)
    loss, param = round(info[1], digits = 7), info[2]
    diagnostics = [length(sl[sl.<0.0])/length(sl) sum(sl[sl.>0.0])/w sum(sl)/w median(sl[sl.>0.0]) mad(sl[sl.>0.0], normalize = true) sum(sl[sl.>0.0])]
    diagnostics_header = ["neg%" "avg_pos_min" "avg_min" "med_pos" "mad_pos" "sum_pos"]

    val , prop = averagePostEff(start, sl, w)
    title = "Average post effect $type $country $medium [$w]"
    #temp_plot = bar(val, title = title, label="Value", xlabel = "Post effect min.", ylabel = "Value", xticks = 1:1:w)
    temp_plot2 = bar(prop, title = title, label="Proportion", xlabel = "Post effect min.", ylabel = "Proportion", xticks = 1:1:w)
    prop = round.(prop / sum(prop), digits = 3)
    writedlm("Coolblue_greedy_$country"*"_$medium"*"_$type"*"_$w"*"_param_($loss).csv", param, ',')
    writedlm("Coolblue_greedy_$country"*"_$medium"*"_$type"*"_$w"*"_prop.csv", prop, ',')
    writedlm("Coolblue_greedy_$country"*"_$medium"*"_$type"*"_$w"*"_diagnostics.csv", [diagnostics_header; diagnostics], ',')
    writedlm("Coolblue_greedy_$country"*"_$medium"*"_$type"*"_$w"*".csv", ["Baseline" "Spotlift"; bl sl], ',')
    #png(temp_plot, "$title.png")
    png(temp_plot2, "$title"*"_prop.png")
end

#Function to get results for largest detected spot lifts, using feature per min. method.
function large_postEff(num_sl::Integer = 100, method::String = "BSTS", w::Integer = 4)

    if method == "Greedy" || method == "greedy"
        data= readdlm("Coolblue_greedy_nl"*"_web"*"_4.csv", ',', Float64, header = true)[1]
        sl = data[:,2]
    elseif method == "Bayes" || method == "BSTS" || country == "bsts"
        sl = load_custom_R("lift_nl_web_case1_4min.csv")
    end

    header = load_X_ext("NL")[3]
    header = ["constant"; vec(header); "total_prop"]

    sl = sl[sl.!=0.0]
    dt, ft = generate_feat_min("NL", method, w)

    ind = sortperm(sl, rev = true)[1:num_sl]
    sl = sl[ind]
    ft = ft[ind, :]
    coeff = zeros(size(ft)[2] + 1)

    #Not all channels appear in top spot lifts (constant predictors)
    constant_ind = findall(x->x==0.0, diag(cov(ft)))
    if length(constant_ind) > 0
        ft = ft[:, setdiff(1:end, constant_ind)]
    end

    result = fit(LassoModel, ft, sl, intercept = true)
    #display(result)

    fitted = predict(result)
    error = sl - fitted
    sl2 = sl.-mean(sl)
    R2 = 1 - error'error / sl2'sl2

    coeff[setdiff(1:end, constant_ind.+1)] = coef(result)

    return [vec(header) coeff], R2
end

#Function to get optimal results for largest detected spot lifts, using feature per min. method.
function optimal_largest_postEff(method::String = "BSTS", w::Integer = 4; param_eval::Bool = false)
    if method == "Greedy" || method == "greedy"
        data= readdlm("Coolblue_greedy_nl"*"_web"*"_4.csv", ',', Float64, header = true)[1]
        sl = data[:,2]
    elseif method == "Bayes" || method == "BSTS" || country == "bsts"
        sl = load_custom_R("lift_nl_web_case1_4min.csv")
    end

    header = load_X_ext("NL")[3]
    header = ["constant"; vec(header); "total_prop"]

    sl = sl[sl.!=0.0]
    dt, ft = generate_feat_min("NL", method, w)

    rng_factor = factor(Vector, length(sl))
    rng = rng_factor[end]:rng_factor[end]:length(sl)
    obs = rng[1]
    @assert prod(rng_factor[1:end-1]) <= 100
    results = large_postEff.(rng, method, w)
    all_coeff, all_R2 = [results[i][1] for i in 1:length(results)], [results[i][2] for i in 1:length(results)]

    if param_eval
        all_params = zeros(length(results), length(header))
        for i in 1:length(results)
            all_params[i,:] = results[i][1][:,2]
        end

        optimal_coeff = all_coeff[findmax(all_R2)[2]]
        filter_ind = findall(x->x!=0.0, optimal_coeff[:,2])
        cols = distinguishable_colors(length(filter_ind))

        temp_plot = plot(all_params[:,filter_ind], title = "Parameter importance for all included features", label = reshape(header[filter_ind], (1,length(filter_ind))),  legend = :outerright, color = cols[1:length(filter_ind)]', ylabel = "Feature importance", xlabel = "Included partitions ($obs lifts per partition)", size = (750,425))
        display(temp_plot)
        return [reshape(header, (1,length(header))); all_params], temp_plot
    end

    optimal_R2, optimal_sl = (1, rng_factor[end]) .* findmax(all_R2)
    optimal_coeff = all_coeff[findmax(all_R2)[2]]
    coeff = all_coeff[end]
    res = [["Only_$optimal_sl"*"_sl" "Coeff." "All_sl" "Coeff."]; [optimal_coeff coeff]]
    temp_plot = plot(all_R2, title = "Optimal R"*'\u00B2'*" over sorted spot lifts", label = "R"*'\u00B2', ylabel = "Values of R"*'\u00B2', xlabel = "Included partitions ($obs lifts per partition)", size = (400,300))
    scatter!([findmax(all_R2)[2]], [optimal_R2], label = "Optimal R"*'\u00B2')
    display(temp_plot)
    display_res(res)

    return res, optimal_R2, results
end

#Function to display entire results matrix in the Julia REPL
function display_res(matrix::AbstractMatrix)
    show(stdout, "text/plain", matrix)
    return nothing
end

#Old function for numerical derrivation (no longer used)
function derr_num(f::Function, x::Real, h::Real = 0.001)
    return (f(x+h)-f(x-h))/(2*h)

    #More accurate and expensive version:
    #return ( f(x-2*h)-f(x+2*h) + 8*(f(x+h)-f(x-h)) )/(12*h)
end
