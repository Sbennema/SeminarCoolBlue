#This file contains all functions related to post effects

#Function return all (raw) post effects for a certain window
function allPostEff(ads_start::AbstractVector, lift::AbstractVector, w::Integer)
    ind_vec = all_ad_ind(ads_start)
    obs = length(ind_vec)
    res = zeros(obs,w)


    for i in 1:obs
        ind = ind_vec[i]
        limit = minimum([ind+w-1, length(lift)])
        temp_eff = lift[ind:1:limit]

        if length(temp_eff) < w
            temp_eff = [temp_eff; zeros(w-length(temp_eff))]
        end

        res[i,:] = temp_eff
    end

    return res
end

#Function for average post effects in 2 steps
function averagePostEff(ads_start::AbstractVector, lift::AbstractVector, w::Integer; filter::String = "all", ext=false)
    obs = length(ads_start)
    res_1 = zeros(w)
    res_2 = zeros(w)
    total_n1 = zeros(w)
    total_n2 = zeros(w)

    overlap = genr_overlap(ads_start,w)
    step1_ind, step2_ind  = overlap_ind(overlap,w)

    #Calculate the averages - Step 1
    for i in step1_ind
        ind = findall(>(0), overlap[i,:])
        for j in ind
            if i+j-1 <= obs
                res_1[j] += maximum([lift[i+j-1] 0.0])
                total_n1[j] += overlap[i,j]
            end
        end
    end

    res_1 = res_1 ./ total_n1
    prop_1 = res_1 / sum(res_1)

    #Calculate the averages - Step 2
    for i in step2_ind
        ind = findall(>(0), overlap[i,:])
        n_ind = overlap[i,ind]
        total_prop = dot(n_ind,prop_1[ind])

        for j in ind
            if i+j-1 <= obs
                n = overlap[i,j]
                res_2[j] += n * prop_1[j] * maximum([lift[i+j-1] 0.0]) / total_prop
                total_n2[j] += n
            end
        end
    end
    res_2 = res_2 ./ total_n2
    prop_2 = res_2 / sum(res_2)

    total_n = total_n1+total_n2
    res = (total_n1.*res_1 + total_n2.*res_2)./total_n
    prop = (total_n1.*prop_1 + total_n2.*prop_2)./total_n

    #Return the results
    if ext
        return res, prop, res_1, prop_1, res_2, prop_2
    else
        return res, prop
    end
end


#Auxilary function for adaptive feat per min timeblocks
function genr_overlap(ads_start::AbstractVector, window::Integer; split::Bool = true)

    obs = length(ads_start)
    res = zeros(Int64, obs , window)

    for i in findall(>(0),ads_start)
        for j in 1:window
            if i+j-1 <= obs
                if split
                    #res[i+j-1, j] += i => moved to other function below
                    res[i+j-1, j] += ads_start[i]
                else
                    res[i+j-1, j] += ads_start[i]
                end
            end
        end
    end

    if split
        return res
    else
        return vec(sum(res, dims = 2))
    end
end

#Auxilary function for adaptive timeblocks, but returns ad identifiers
function genr_overlap_ads(ads_start::AbstractVector, window::Integer; ad_index::Bool = true)

    obs = length(ads_start)
    res = zeros(Int64, obs , window)

    for i in findall(>(0),ads_start)
        for j in 1:window
            if i+j-1 <= obs
                if ad_index
                    res[i+j-1, j] += i
                else
                    res[i+j-1, j] += ads_start[i]
                end
            end
        end
    end


    return res
end

#Function for getting amount of ads on certain time
function get_ad_amount(data::AbstractVector)
    temp = genr_datetime()
    res = zeros(Int64, length(temp))

    for i in 1:length(temp)
        res[i] = length(get_ad(data, temp[i]))
    end

    return res
end

#Function to get the indices of overlapping ads
function overlap_ind(ads_overlap::AbstractMatrix, w::Integer)
    all_ind = findall(>(0), vec(sum(ads_overlap, dims = 2)))

    step1_ind = Int64[]

    for i in all_ind
        if length(findall(>(0), ads_overlap[i,:])) == 1
            push!(step1_ind, i)
        end

    end

    step2_ind = setdiff(all_ind, step1_ind)

    return step1_ind, step2_ind
end

#Returns the start of all ads starting indices
function all_ad_ind(ads_start::AbstractVector)
    return findall(x->x>0, ads_start)
end
