#This file contain functions for calculating the greedy spot lifts and various temporal means

#This function returns the in-sample baseline and mse (train) or baseline and spot lift (test) via the greedy method
function impute_traffic(visits::AbstractVector, ads_effect::AbstractVector, weights::AbstractVector; set::String = "train")
    visits = convert(Array{Float64,1}, visits)
    ads_effect[ads_effect.>1] .= 1
    ads = convert(Array{Bool,1}, ads_effect)
    visits_adj = copy(visits)

    if set == "train" || set == "Train"
        func = <(1)
    elseif set == "test" || set == "Test"
        func = >(0)
    end

    ind = findall(func,ads)
    ind_n = length(ind)

    for i in ind
        w,val = adj_weights(ads,i, weights)
        visits_adj[i] = dot(w,visits[val])
    end

    if set == "train" || set == "Train"
        loss = msd(visits[ind], visits_adj[ind])
        return visits_adj, loss
    elseif set == "test" || set == "Test"
        spotlift = visits - visits_adj
        return visits_adj, spotlift
    end
end

#This function returns the in-sample baseline and mse (train) or baseline and spot lift (test) via the greedy method quickly
function impute_traffic_fast(visits::AbstractVector, ads_effect::AbstractVector; info::Bool = false)
    obs = length(visits)

    visits = convert(Array{Float64,1}, visits)
    visits_ext = [copy(visits); 0]
    ads_effect[ads_effect.>1] .= 1
    ads = convert(Array{Bool,1}, ads_effect)
    baseline = copy(visits)
    func, func_ad = <(1), >(0)

    ind = findall(func,ads)
    ind_n = length(ind)
    y = visits[ind]
    X = zeros(ind_n, 10)

    ind_ad = findall(func_ad,ads)
    ind_n_ad = length(ind_ad)
    y_ad = visits[ind_ad]
    X_ad = zeros(ind_n_ad, 10)

    for i in 1:ind_n
        val = adj_weights_fast(ads,ind[i])
        X[i,:] = visits_ext[val]
    end

    for i in 1:ind_n_ad
        val = adj_weights_fast(ads,ind_ad[i])
        X_ad[i,:] = visits_ext[val]
    end

    X = [ones(ind_n) X]
    beta = X'X \ X'y

    X_ad = [ones(ind_n_ad) X_ad]
    baseline[ind_ad] = X_ad*beta
    spotlift = visits - baseline

    error = y - X*beta
    temp_y = y .- mean(y)
    loss = msd(visits[ind], X*beta)
    R2 = 1 - error'error / temp_y'temp_y
    diagnostics = (loss, beta, R2)

    if !info
        return baseline, spotlift
    else
        return baseline, spotlift, diagnostics
    end
end

#Find the allowed in-sample indices/weights for impute_traffic
function adj_weights(X::AbstractVector, ind::Integer, weights::AbstractVector; sym::Bool = false)

    init_ind = [prevWeek(ind,4), prevWeek(ind,2), prevWeek(ind,1), prevDay(ind,1), prevMin(ind,1), nextMin(ind,1), nextDay(ind,1), nextWeek(ind,1), nextWeek(ind,2), nextWeek(ind,4)]
    init_weights = copy(weights)

    for j=1:length(init_ind)
        if j<=floor(Int64, length(init_ind)/2)
            try
                init_ind[j] = findnext(<(1),X,init_ind[j])
            catch MethodError
                #init_ind[j] = findnext(<(1),X,nextWeek(init_ind[j],3))
                #init_weights[j] = 0.05
                init_ind[j] = 1
                init_weights[j] = 0.0
            end
        else
            try
                init_ind[j] = findprev(<(1),X,init_ind[j])
            catch MethodError
                init_ind[j] = 1
                init_weights[j] = 0.0
            end
        end
    end

    init_weights = init_weights ./ sum(init_weights)

    return init_weights, init_ind
end

#Find the allowed in-sample indices/weights for impute_traffic_fast
function adj_weights_fast(X::AbstractVector, ind::Integer; sym::Bool = false)
    empty_ind = length(X) + 1
    init_ind = [prevWeek(ind,4), prevWeek(ind,2), prevWeek(ind,1), prevDay(ind,1), prevMin(ind,1), nextMin(ind,1), nextDay(ind,1), nextWeek(ind,1), nextWeek(ind,2), nextWeek(ind,4)]


    for j=1:length(init_ind)
        if j<=floor(Int64, length(init_ind)/2)
            try
                init_ind[j] = findnext(<(1),X,init_ind[j])
            catch MethodError
                init_ind[j] = empty_ind
            end
        else
            try
                init_ind[j] = findprev(<(1),X,init_ind[j])
            catch MethodError
                init_ind[j] = empty_ind
            end
        end
    end

    return init_ind
end

#Grid optimizer for impute_traffic (can take a long time)
function tune_weights(visits::AbstractVector, ads_effect::AbstractVector, sym::Bool = true, rng::UnitRange{Int64} = 0:10, rep::Integer = 250000; mvp::Tuple{Array{Float64,1},Float64} = (zeros(10), Inf))

    best_params = mvp[1]
    best_loss = mvp[2]
    curr_loss = Inf

    #rng1 = 0:floor(Int64,(1 + rng[end]) / 4)
    #rng2 = 0:floor(Int64,(1 + rng[end]) / 2)
    #rng3 = 0:floor(Int64,(1 + rng[end]) / 2)

    smpl = zeros(10)
    if sym
        temp = [[a,b,c,d,e] for a=rng for b=rng for c=rng for d=rng for e=rng]
        all_smpl = unique(temp ./ sum.(temp))
        all_smpl = vcat.(all_smpl, reverse.(all_smpl))
    else
        temp = [[a,b,c,d,e,v,w,x,y,z] for a=rng for b=rng for c=rng for d=rng for e=rng for v=rng for w=rng for x=rng for y=rng for z=rng]
        all_smpl = unique(temp ./ sum.(temp))
    end

    if isnan(sum(all_smpl[1]))
        #deleteat!(all_smpl,1)
        all_smpl[1] = zeros(10)
    end


    bl = zeros(260640)
    sl = zeros(260640)

    iter = minimum([rep, length(all_smpl)])
    display(iter)
    @showprogress for i = 1:iter
        smpl = all_smpl[i]

        bl, curr_loss = impute_traffic(visits, ads_effect, smpl, set="train")

        if curr_loss < best_loss
            best_params = smpl
            best_loss = curr_loss
        end
    end

    best_params = best_params ./ sum(best_params)
    return best_params, best_loss
end

#This function returns the daily mean
function daily_mean(X::AbstractVector, filter::Bool = true, ind::Integer = 0)
    res = zeros(1440)
    x = reshape(X, (1440,181))
    r,c = size(x)

    for i in 1:r
        temp  = x[i,:]


        if length(temp[temp.>0.0]) == 0 || sum(abs.(temp)) == 0
            res[i] = 0.0
        elseif filter
            res[i] = mean(temp[temp.>0.0])
        else
            res[i] = mean(temp[temp.!=0.0])
        end
    end

    if ind <= 0
        return res
    else
        return res[ind]
    end
end

#This function returns the weekly mean mean
function weekly_mean(X::AbstractVector, filter::Bool = true, ind::Integer = 0)
    res = zeros(37440)
    x = reshape(X, (1440,181))
    r,c = size(x)

    for i in 1:r
        temp  = x[i,:]


        if length(temp[temp.>0.0]) == 0 || sum(abs.(temp)) == 0
            res[i] = 0.0
        elseif filter
            res[i] = mean(temp[temp.>0.0])
        else
            res[i] = mean(temp[temp.!=0.0])
        end
    end

    if ind <= 0
        return res
    else
        return res[ind]
    end
end

#This function coputes the average over a week
function average_week(X::AbstractVecOrMat)
    visits = convert(Array{Float64,1}, X[:,1])

    adj_visits = reshape([visits; zeros(1440)], (10080,26))

    adj_visits[8641:10080,26] = mean(adj_visits[8641:10080,1:25], dims = 2)

    return mean(adj_visits, dims=2)

end

#This function scores the spot lifts for pos/neg proportion matches, can be used to compare greedy and bsts
function daily_score(X::AbstractVector, Y::AbstractVector)
    score  = 0
    x = X[X.!=0.0]
    y = Y[Y.!=0.0]

    ind = zeros(260640)

    for i in 1:length(x)
        check1 = ((x[i]>0.0) * (y[i]<0.0))
        check2 = ((x[i]<0.0) * (y[i]>0.0))
        if check1 || check2
            score += 1
            ind[i] = i
        end
    end
    display(score)

    return score/length(x), ind[ind.>0.0]
end

#Old counterfactual function (no longer used for anything)
# function counterfactual(data::AbstractVecOrMat, opts::Integer = 1)
#
#     baseline, spotlift = impute_traffic(data)
#
#     #baseline2, spotlift2 = impute_traffic(data2)
#     #baseline3, spotlift3 = impute_traffic(data3)
#     #baseline4, spotlift4 = impute_traffic(data4)param_nw
#
#
#     #TODO: add general filters
#     #Uncomment to filter the anomaly:
#     #spotlift1[spotlift1.>0.1] .= 0.0
#
#     #spotlifts = [spotlift1 spotlift2 spotlift3 spotlift4]
#
#     writedlm("Coolblue_basic_spotlift_$opts.csv", spotlift, ',')
#
#     return baseline, spotlift
# end
