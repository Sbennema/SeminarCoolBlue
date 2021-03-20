#This file contains fucntions for generating the graphs from the report

#This function will generate .png files of all plots of the report
function plot_all()
    plot_daily_visits()
    plot_correction()
    plot_daily_mean()
    plot_spot_lift_example()
    plot_window_prop()
end

function plot_daily_visits()
    visits1 = load_data("NL", "web")[:,1]
    visits2 = load_data("BE", "web")[:,1]
    visits1 = reshape(visits1, (1440,181))
    visits2 = reshape(visits2, (1440,181))
    res1 = vec(sum(visits1, dims = 1))
    res2 = vec(sum(visits2, dims = 1))
    dt = unique(Date.(genr_datetime()))
    p1 = bar(dt, res1, xlabel = "Days", ylabel = "Visits", title = "Total visits per day (NL, website)")
    p2 = bar(dt, res2, xlabel = "Days", ylabel = "Visits", title = "Total visits per day (BE, website)")
    res = plot(p1, p2, legend= false, layout = 2, size = (1500, 600), margin=5mm)

    png(res, "Visits.png")
end

function plot_correction()
    visits = load_data("NL", "web")[:,1]
    i,j = nextDay(1,30), nextDay(1,31)
    dt, visits = genr_datetime()[i:j-1], visits[i:j-1]
    adj_vis = copy(visits)
    adj_vis[1] += 0.4
    p1 = plot(Time.(dt), adj_vis, xlabel = "Time", ylabel = "Visits", title = "Visits of a day without correction", legend = false)
    scatter!([Time(0,0)], [adj_vis[1]])
    p2 = plot(Time.(dt), visits, xlabel = "Time", ylabel = "Visits", title = "Visits of a day with correction", legend = false)
    res = plot(p1, p2, layout = 2, size = (1500, 600), margin=5mm)

    png(res, "Correction.png")
end

function plot_daily_mean()
    visits = load_data("NL", "web")[:,1]
    res = plot(Time.(genr_datetime()[1:1440]), daily_mean(visits), xlabel = "Time", ylabel = "Visits", legend=:topleft, title = "Average visits per minute", label = "Visits index", xticks = Time.(Hour.([2,6,10,14,18,22])))
    plot!([Time(16,00), Time(18,14)], color="grey", seriestype = :vspan, seriesalpha = 0.3, label  = "Primetime [16:00 - 18:14]")
    plot!([Time(18,15), Time(22,14)], color="grey", seriestype = :vspan, seriesalpha = 0.5, label  = "Primetime [18:15 - 22:14]")
    plot!([Time(22,15), Time(23,59)], color="grey", seriestype = :vspan, seriesalpha = 0.7, label  = "Primetime [22:15 - 23:59]")

    display(res)
    png(res, "Daily.png")
end

function plot_spot_lift_example()
    visits = load_data("NL", "web")[:,1]
    baseline = readdlm("Coolblue_greedy_NL_Web_6.csv", ',', Any, header = true)[1][:,1]
    i,j = 12011, nextMin(12000,22)
    res = plot(Time.(genr_datetime())[i:j], [visits[i:j], baseline[i:j]], label = ["Actual visits" "Estimated baseline"], xlabel = "Time", ylabel = "Visits", legend=:topleft, title = "Example of spot lift")
    plot!([Time(8,15), Time(8,21)], color="grey", seriestype = :vspan, seriesalpha = 0.25, label  = "Advertisement effect period")
    png(res, "Spot_lift_example.png")
end

function plot_window_prop()
    temp = [readdlm("Coolblue_greedy_nl_web_"* "$i" *"_prop.csv", ',', Float64) for i in 2:10]
    data =  match_plot_dims(temp)
    res = groupedbar(data[1:8, [3,7]], label = ["Window=4" "Window=8" ], title = "Proportion of spot lift per minute", xlabel = "Minutes", ylabel = "Proportion", xticks = 1:8)
    png(res, "Post_effect_prop.png")
end

#Auxillary funtion for the plot_window_prop function
function match_plot_dims(data::Array{Array{Float64,2},1})
    lengths = length.(data)
    res = zeros(maximum(lengths), length(data))

    for i in 1:length(data)
        res[1:lengths[i], i] = vec(data[i])
    end

    return res

end
