#This file contains functions for fitting spot lifts

#This function will give all things required for the report)
function run_all()
    #Validate all post effect windows via greedy approach:
    validate()

    #Generate the feature per minute dataset for NL web (BSTS + 4 mins)
    generate_feat_min(save = true)

    #Evaluate the features per min LASSO models
    optimal_largest_postEff(param_eval = true)

    #Get the optimal features per min LASSO model
    optimal_largest_postEff(param_eval = false)

    #Return all figures
    plot_all()

end

#Finds the optimal hyperparameters for the LASSO model
function validate_lasso_min(country::String, medium::String, method::String, w::Integer; best::Bool = false)
    smpl = [0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1]
    res = zeros(10)

    for i in 1:10
        res[i] = spotlift_lasso_min(country, medium, method, w; α = smpl[i])[3]
    end

    if best
        return findmax(res)
    else
        return res
    end
end

#Perform LASSO for spot lifts of a specific method and minute set-up
function spotlift_lasso_min(country::String, medium::String, method::String, w::Integer; α::Number = 0.01, ext::Bool = false)
    if country == "Netherlands" || country == "NL" || country == "nl"
        if method == "Greedy" || method == "greedy"
            data, header = readdlm("Coolblue_greedy_nl"*"_web"*"_4.csv", ',', Float64, header = true)
            lift = data[:,2]
        elseif method == "Bayes" || method == "BSTS" || country == "bsts"
            lift = load_custom_R("lift_nl_web_case1_4min.csv")
        end
    elseif country == "Belgium" || country == "BE" || country == "be"
        if method == "Greedy" || method == "greedy"
            data, header = readdlm("Coolblue_greedy_BE"*"_Web"*"_4.csv", ',', Float64, header = true)
            lift = data[:,2]
        else method == "Bayes" || method == "BSTS" || country == "bsts"
            lift = load_custom_R("lift_be_web_case1_4min.csv")
        end
    end

    datetimes, feat = generate_feat_min(country, method, w)
    dt = load_X(country)[1]
    start = get_ad_amount(dt)
    ind = genr_overlap(start,w; split=false)
    ind[ind.>1] .= 1
    lift = lift[ind.==1]

    lift[lift.<0.0] .= 0.0

    result = fit(LassoModel, feat, lift, intercept = true, α = α )

    lift2 = lift .- mean(lift)
    fitted = predict(result)
    error = lift - fitted
    R2 = 1 - error'error / lift2'lift2

    #coef(result)

    display(result)

    if !ext
        return fitted, error, R2
    else
        return lift, fitted, error, coef(result), R2
    end
end

#Perform LASSO for spot lifts of a specific method and minute set-up (aggregated version)
function spotlift_lasso_agg(country::String, medium::String, method::String, w::Integer; ext::Bool = false)
    if country == "Netherlands" || country == "NL" || country == "nl"
        if method == "Greedy" || method == "greedy"
            data, header = readdlm("Coolblue_greedy_nl"*"_web"*"_4.csv", ',', Float64, header = true)
            lift = data[:,2]
        elseif method == "Bayes" || method == "BSTS" || country == "bsts"
            lift = load_custom_R("lift_nl_web_case1_4min.csv")
        end
    elseif country == "Belgium" || country == "BE" || country == "be"
        if method == "Greedy" || method == "greedy"
            data, header = readdlm("Coolblue_greedy_BE"*"_Web"*"_4.csv", ',', Float64, header = true)
            lift = data[:,2]
        else method == "Bayes" || method == "BSTS" || country == "bsts"
            lift = load_custom_R("lift_be_web_case1_4min.csv")
        end
    end

    #test, test2 = readdlm("X_nl_4min.csv", ',', Any, header = true)

    datetimes, feat = generate_feat_min(country, method, w)
    dt = load_X(country)[1]
    start = get_ad_amount(dt)
    ind = genr_overlap(start,w; split=false)
    ind[ind.>1] .= 1
    lift = lift[ind.==1]

    lift[lift.<0.0] .= 0.0

    #ind_del = lift.<=0.0
    #feat = feat[setdiff(1:end,ind_del),:]
    #lift = lift[setdiff(1:end,ind_del)]

    result = fit(LassoModel, feat, lift, intercept = true)
    fitted = predict(result)
    error = lift - fitted
    R2 = 1 - error'error / lift'lift

    display(result)

    if !ext
        return fitted, error, R2
    else
        return lift, fitted, error, R2
    end
end

#Auxilary function for aggregated model
function allPostEff_dur(ads::AbstractVector, lift::AbstractVector, ext::Bool = false)
    res = Array{Float64, 1}[]
    AllFound = false
    ind = 1

    while !AllFound
        eff, ind = nextPostEff_dur(ads, lift, ind, true)
        push!(res, eff)

        if isnothing(ind)
            break
            #AllFound = true
        end
    end

    if isempty(res[end])
        deleteat!(res, length(a))
    end

    sz = maximum(length.(res))

    if ext
        return res, sz
    else
        return res
    end
end

#Auxilary function for aggregated model
function nextPostEff_dur(ads::AbstractVector, lift::AbstractVector, i::Integer = 1, ext::Bool = true)

    b1 = findnext(x->x>0, ads,i)

    if !isnothing(b1)
        b2 = findnext(x->x<1, ads,b1)
    else
        return Float64[], nothing
    end

    if isnothing(b2)
        return lift[b1:end], b2
    end

    if ext
         return lift[b1:b2-1], b2
    else
        return lift[b1:b2-1]
    end
end

#Perform Basic OLS
function ols(X::AbstractMatrix,y::AbstractVector; intercept = true)
    obs,par = size(X)

    if intercept
        X = [ones(obs) X]
        obs,par = size(X)
    end

    beta_ols = X'X \ X'y
    resid_ols = y - X * beta_ols
    sig2_ols = resid_ols'resid_ols / (obs-par)
    cov_beta = sig2_ols * (X'X)^-1

    beta_se = sqrt.(diag(cov_beta))

    R2 = 1 - resid_ols'resid_ols / y'y
    return beta_ols, resid_ols, beta_se, R2
end
