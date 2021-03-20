module CB

#Set the directory and check/download all dependencies
cd(@__DIR__)
include("CB_require.jl")

using LinearAlgebra, Primes
using Distributions, Random, StatsBase,  Statistics, MultivariateStats
using BlackBoxOptim, Optim
using Dates, DelimitedFiles, ProgressMeter
using Plots, Colors, Measures, StatsPlots
using Lasso

#Export the function that can be used via CB.SomeFunction
export
    #Greedy baseline related:
    impute_traffic,
    tune_weights,
    impute_traffic_fast,

    #Data related:
    load_data,
    load_all,
    load_X,
    load_X_ext,
    load_r,
    load_custom,
    load_custom_r,

    #For the pictures in the report:
    plot_all,

    #For all results:
    run_all,

    #Spotlift validation and generation:
    validate,
    validate_all,
    validate_all_custom,
    large_postEff,
    optimal_largest_postEff,

    #Time/indexing related
    get_date,
    get_time,
    get_datetime,
    genr_datetime,
    nextCustom,
    prevCustom,

    #For the feature per min dataset:
    generate_feat_min

# include("CB_baseline_basic.jl")
# include("CB_data.jl")
# include("CB_plots.jl")
# include("CB_models.jl")
# include("CB_post_effect.jl")
# include("CB_post_effect_validation.jl")
# include("CB_time_indexing.jl")
# include("CB_timeblocks.jl")

#Load all source files
include("CB_load_scripts.jl")

end
