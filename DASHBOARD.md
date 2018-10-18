### Dashboard

- **Topics**:

  - [ ] Non-parametric Inference & Statistical Learning: 
    - [x] Bootstrap resampling & sampling noise measurement
    - [x] Average shifted histogram (`ASH`)
    - [ ] Adaptive Gaussian kernel smoothing density (`KernSmooth`)
    - [ ] Distance matrix (generated from CRRA)
    - [x] Hierarchical clustering (diff methods)
  - [ ] Empirical asset pricing experiments: predictive signals research 
    - [ ] Momentum (systematic and individual/sector)
    - [ ] Volatility clustering (systematic and individual/sector)
    - [ ] Dynamic factor loadings (individual/sector)
    - [ ] Alpha dynamics (individual/sector)
    - [ ] Market turbulence (systematic)
    - [ ] Tail risk modeling and forecasts (systematic)
  - [ ] Numerical Simulations: 
    - [x] Multi-asset T-Copulas for better tail risk fits (for systematic signals)
    - [ ] Conditional correlation structure measurement
    - [ ] Empirical Measure Change (for individual/sector signals)
  - [ ] Single-period Convex Stochastic Control
    - [ ] Risk-adjusted obj
    - [ ] Beta constraints (for alpha signals)
  - [x] Implementation: 
    - [x] R (xts, tidyverse)
    - [x] Julia (JuliaStats, JuMP)
    - [x] Python (Pandas, TensorFlow, Keras)




- **Signal Generation and Optimization Pipelines**:

  - `NumericSignal(df::1dxts/2dxts, f::ScoringMethod, period::Int64, dimdf::Number)::1dxts{Number}` 

    - in Julia write multiple methods using Multiple Dispatch, rolling window apply embedded
    - i.e. `NumericSignal(SPYret, IfPositive,)`

  - `BoolSignal(numsignal::1dxts{Number}, ngroup::Number=10, f::DiffMethod, cuts::Vector{Number} = collect(0:0.1:1))::Vector{1dxts{Bool}}` 
    - Differentiating method, default using cuts to cut empirical quantiles (for continuous signal)
    - Also a BoolSignal() for binary signal

  - `DensityBound(ret::1dxts{Number}, qs::Vector{2, Number})` 

    - Generate bounds for density estimation, unify the breaks series obtained by density estimation

  - `EstDensity(idx::1dxts{Bool}, ret::1dxts{Number}, bounds::Vector{2, Number}; rkernel::Number, nbreaks::Number)::Vector/Dict{Number}` 
    - index not lagged, add more methods in this function to improve the fitting effect, including resampling and adaptive kernel fitting
    - output density and breaks

  - `BootBand(idx::1dxts{Bool}, ret::1dxts{Number}, bounds::Vector{2, Number})` # resample 

  - `NoiseBB(idx::1dxts{Bool}, ret::1dxts{Number}, bounds::Vector{2, Number})`

  - `DensityDist(d1::Vector{Number}, d2::Vector{Number}, measure::Vector{Number})::Number`

  - `DistMatrix(boolsignal::Vector{1dxts{Bool}}, est = EstDensity, dis = DensityDist)::Array{2, Number}`

  - `HieClustering(dist::Array{2, Number}, ngroup::Int64)::Vector{Vector{Int64}; dthreshold::Number}::Number` 

    - Output a number measuring differentiating power, can be the distance, distance should be comparable for all density functions, then to decide whether this signal can be used or not

  - Select signal sets (systematic and individual signals) and obtain conditional distributions, copulas, individual measure change for each asset every day.

     





