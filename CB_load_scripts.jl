#This file loads all seperate files

include("CB_data.jl")
include("CB_baseline_greedy.jl")
include("CB_time_indexing.jl")
include("CB_feat_min.jl")
include("CB_post_effect.jl")
include("CB_post_effect_validation.jl")
include("CB_models.jl")
include("CB_plots.jl")

cd(@__DIR__)
