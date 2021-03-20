#This file contains fucntions related to laoding the data

#Shorthand for data exported via Python (visits and ads)
function load_data(country::String, medium::String)

    if country == "Netherlands" || country == "NL" || country == "nl"
        if medium == "App" || medium == "app"
            data, header = readdlm("nl_app_adj.csv", ',', Any, header=true)
        elseif medium == "Web" || medium == "web"
            data, header = readdlm("nl_web_adj.csv", ',', Any, header=true)
        end
    end

    if country == "Belgium" || country == "BE" || country == "be"
        if medium == "App" || medium == "app"
            data, header = readdlm("be_app_adj.csv", ',', Any, header=true)
        elseif medium == "Web" || medium == "web"
            data, header = readdlm("be_web_adj.csv", ',', Any, header=true)
        end
    end

    visits = convert(Array{Float64,1}, data[:,2])
    ads = convert(Array{Float64,1}, data[:,3])

    return [visits ads]
end

#Shorthand for loading all datasets (visits and ads)
function load_all()
    data_nl_app, header_nl_app = readdlm("nl_app_adj.csv", ',', Any, header=true)
    data_nl_web, header_nl_web = readdlm("nl_web_adj.csv", ',', Any, header=true)
    data_be_app, header_be_app = readdlm("be_app_adj.csv", ',', Any, header=true)
    data_be_web, header_be_web = readdlm("be_web_adj.csv", ',', Any, header=true)

    return [data_nl_app[:,2:3] data_nl_web[:,2:3] data_be_app[:,2:3] data_be_web[:,2:3]]
end

#Shorthand for data exported via R (spotlifts)
function load_r(country::String, medium::String)

    if country == "Netherlands" || country == "NL" || country == "nl"
        if medium == "App" || medium == "app"
            lifts, header = readdlm("NL_app_lifts.csv", ';', Any, header=true)
        elseif medium == "Web" || medium == "web"
            lifts, header = readdlm("NL_web_lifts.csv", ';', Any, header=true)
        end
    end

    if country == "Belgium" || country == "BE" || country == "be"
        if medium == "App" || medium == "app"
            lifts, header = readdlm("BE_app_lifts.csv", ';', Any, header=true)
        elseif medium == "Web" || medium == "web"
            lifts, header = readdlm("BE_web_lifts.csv", ';', Any, header=true)
        end
    end

    lift = lifts[:,3]
    lift[lift.== "NA"] .= 0.00
    lift = convert(Array{Float64,1}, lift)

    return lift
end

#Shorthand for laod feature matrix with operators
function load_X(country::String)
    if country == "Netherlands" || country == "NL" || country == "nl"
        data, header = readdlm("X_nl_block.csv", ',', header = true)
        col = 2:19
    elseif country == "Belgium" || country == "BE" || country == "be"
        data, header = readdlm("X_be_block.csv", ',', header = true)
        col = 2:17
    end

    data[:,2] = replace.(data[:,2], '\x20' => "T")

    #Columns to keep

    data = data[:,col]
    header = header[:,col]

    return DateTime.(data[:,1]), convert(Array{Float64, 2}, data[:,2:end]), header
end

#Shorthand for laod feature matrix with channels and pos_in_break fix
function load_X_ext(country::String)
    #Not implemented correctly for BE
    if country == "Netherlands" || country == "NL" || country == "nl"
        data, header = readdlm("X_nl_4min_breaks_blocks.csv", ',', Any, header = true)
        col = [6:49; 51:53]
    elseif country == "Belgium" || country == "BE" || country == "be"
        data, header = readdlm("X_be_4min_breaks_blocks.csv", ',', Any, header = true)
        col = 2:17
    end

    data[:,2] = replace.(data[:,2], '\x20' => "T")
    dt = DateTime.(data[:,2])

    #Columns to keep

    data = data[:,col]
    header = header[:,col]

    return dt, convert(Array{Float64, 2}, data), header
end

#Read the tv ad data via the original file
function load_tv_ads(country::String="All")
    x,y = readdlm("broadcasting_data", ',', header = true);
    d = Date.(x[:,3])
    t = Time[]

    if country == "Netherlands" || country == "NL" || country == "nl"
        dates = x[:,4][x[:,13].=="Netherlands"]
        d = d[x[:,13].=="Netherlands"]
    elseif country == "Belgium" || country == "BE" || country == "be"
        dates = x[:,4][x[:,13].=="Belgium"]
        d = d[x[:,13].=="Belgium"]
    elseif country == "All"
        dates = x[:,4]
    else
        throw(ArgumentError("Invalid specification"))
    end

    for i in 1:length(dates)
        if length(dates[i]) > 8
            push!(t, Time(dates[i][1:8]) + Dates.Second(1))
        else
            push!(t, Time(dates[i]))
        end
    end
    #t = round.(t, Dates.Minute)

    #t = Time.(x[:,4]);
    dt = DateTime.(d,t)
    dt = round.(dt, Dates.Minute)

    return dt
end

#Load files by their name (if exported via python)
function load_custom(file::String)
    #For data exported from python:
    data, header = readdlm(file, ',', Any, header=true)


    visits = convert(Array{Float64,1}, data[:,2])
    ads = convert(Array{Float64,1}, data[:,3])

    return [visits ads]
end

#Load files by their name (if exported via R)
function load_custom_R(file::String)

    #For data exported from R:
    data, header = readdlm(file, ';', Any, header=true)
    lift = data[:,3]
    lift[lift.== "NA"] .= 0.00
    lift = convert(Array{Float64,1}, lift)


    return lift
end
