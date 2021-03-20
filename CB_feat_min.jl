#functions for features per minute method

#Generate the features per minute .csv files
function generate_feat_min(country::String, method::String, w::Integer; save::Bool = false)
    #Multiple minutes only supported for greedy algorithm

    if country == "Netherlands" || country == "NL" || country == "nl"
        dt, feat, misc = load_X_ext("NL")
        start = get_ad_amount(dt)
        if method == "Greedy" || method == "greedy"
            data, header = readdlm("Coolblue_greedy_nl"*"_web"*"_4.csv", ',', Float64, header = true)
            sl = data[:,2]
            prop = averagePostEff(start, sl, w)[2]
        elseif method == "Bayes" || method == "BSTS" || country == "bsts"
            lift = load_custom_R("lift_nl_web_case1_4min.csv")
            prop = averagePostEff(start, lift, w)[2]
        end
    elseif country == "Belgium" || country == "BE" || country == "be"
        dt, feat, misc = load_X("BE")
        start = get_ad_amount(dt)
        if method == "Greedy" || method == "greedy"
            data, header = readdlm("Coolblue_greedy_BE"*"_Web"*"_4.csv", ',', Float64, header = true)
            sl = data[:,2]
            prop = averagePostEff(start, sl, w)[2]
        else method == "Bayes" || method == "BSTS" || country == "bsts"
            lift = load_custom_R("lift_be_web_case1_4min.csv")
            prop = averagePostEff(start, lift, w)[2]
        end
    end

    posteff = genr_overlap_ads(start, w, ad_index = true)

    datetimes, features, total_prop = genr_adaptive_timeblocks(dt,feat, posteff, prop; ext = true)
    features = [features total_prop]
    header = [load_X_ext(country)[3] "Total_prop"]

    if save
        #writedlm("Coolblue_features_$country"*"_$w"*" [$method].csv", [header; datetimes features], ',')
        writedlm("Coolblue_features_$country"*"_$w"*" [$method].csv", [datetimes features], ',')
    end

    return datetimes, features
end

#Function for static timeblocks (aggregate features)
function genr_static_timeblocks(window::Integer)
    res = genr_datetime()
    ind = 1:window:260640


    for  i in ind
        if i+window-1 <= 260640
            res[i:i+window-1] .= res[i]
        else
            res[i:260640] .= res[i]
        end
    end

    return res
end

#Function for adaptive timeblocks (features per min)
function genr_adaptive_timeblocks(dt::AbstractVector, feat::AbstractVecOrMat, overlap::AbstractVecOrMat,  prop::AbstractVector; ext::Bool = false)
    obs, w = size(overlap)
    obs_f, par_f = size(feat)
    res = zeros(obs, par_f)
    total_prop = zeros(obs)
    datetimes = genr_datetime()

    for i in 1:obs
        for j in 1:w
            if overlap[i,j] != 0
                temp_dt = get_datetime(overlap[i,j])
                temp_ind = findall(x->x==temp_dt, dt)

                if length(temp_ind) > 1
                    res[i,:] = res[i,:] + prop[j]*sum(feat[temp_ind,:], dims = 1)'
                else
                    res[i,:] = res[i,:] + prop[j]*feat[temp_ind,:]'
                end

                total_prop[i] += length(temp_ind) * prop[j]
            end
        end
    end
    row_filter = findall(x->x==0.0, total_prop)
    res = res ./ total_prop

    if ext
        return datetimes[setdiff(1:end,row_filter),:], res[setdiff(1:end,row_filter),:], total_prop[setdiff(1:end,row_filter)]
    else
        return datetimes[setdiff(1:end,row_filter),:], res[setdiff(1:end,row_filter),:]
    end
end

#Function for fixing ads starting at the same time
function preproces_ads(data::AbstractVector, feat::AbstractVecOrMat; ext::Bool = false)
    dat = copy(data)
    res = zeros(size(feat))
    sz = zeros(Int64, length(data))
    ind_del = Int64[]

    for i in 1:length(data)
        ind = get_ad(data, data[i])
        res[i,:] = sum(feat[ind,:], dims = 1)
        sz[i] = length(ind)

        deleteat!(ind,1)
        append!(ind_del, ind)
    end

    dat = dat[setdiff(1:end, ind_del), :]
    res = res[setdiff(1:end, ind_del), :]
    sz = sz[setdiff(1:end, ind_del)]

    if !ext
        return vec(dat), res
    else
        return vec(dat), res, sz
    end
end
