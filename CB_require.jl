#This file downloads all required packages

using Pkg

dependencies = [
    "LinearAlgebra",
    "Primes",
    "Distributions",
    "Random",
    "StatsBase",
    "Statistics",
    "MultivariateStats",
    "BlackBoxOptim",
    "Optim",
    "Dates",
    "DelimitedFiles",
    "ProgressMeter",
    "Plots",
    "Colors",
    "Measures",
    "StatsPlots",
    "Lasso"
]

Pkg.add(dependencies)
