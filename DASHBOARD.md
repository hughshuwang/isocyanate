### Dashboard

- **Topics**:
  - [x] Non-para Inf & Stat Learning: 
    - [x] Bootstrap resampling & sampling noise measurement
    - [x] Average shifted histogram (`ASH`)
    - [x] Adaptive Gaussian kernel smoothing density (`KernSmooth`)
      - [x] *PENDING:* Bootstrap-based/Density-based adaptive radius, Learning algorithm
    - [x] Fitting CRRA for returns (asymmetric)
    - [x] Define distances (Measure for Modeling Risks and differentiating power)
    - [x] Hierarchical clustering (diff methods)
  - [ ] Predictive Signals (Forward Looking):
    - [ ] Momentum (systematic and individual/sector)
    - [ ] Volatility clustering (systematic and individual/sector)
    - [ ] Factor loadings dynamics (individual/sector, Kalman Filter, Regime Identification)
      - [ ] Alpha dynamics (individual/sector)
    - [ ] Market turbulence (systematic)
    - [ ] Tail risk modeling and forecasts (systematic)
  - [ ] Numerical Simulations:
    - [x] Multi-asset T-Copulas for better tail risk fits (for systematic signals)
    - [ ] Conditional correlation structure measurement
    - [ ] Empirical Measure Change (for individual/sector signals)
  - [ ] Single-period Convex Stochastic Control:
    - [ ] Convex risk-adjusted objective functions
    - [ ] Convex constraint sets
      - [ ] Self financing constraints
      - [ ] Beta constraints for alpha signals



- **Signal Generation and Optimization Pipelines**:

  - `NumericSignal(df::1dxts/2dxts, f::ScoringMethod, period::Int64, dimdf::Number)::1dxts{Number}` `

  - `BoolSignal(numsignal::1dxts{Number/Binary}, ngroup::Number=10, f::DiffMethod, cuts::Vector{Number} = collect(0:0.1:1))::Vector{1dxts{Bool}}` 
    - Default using cuts to cut empirical quantiles (for continuous signal)

  - `DensityBound(ret::1dxts{Number}, qs::Vector{2, Number})` 
    - Generate bounds for density estimation, unify the breaks series obtained by density estimation

  - `EstDensity(idx::1dxts{Bool}, ret::1dxts{Number}, bounds::Vector{2, Number}; rkernel::Number, nbreaks::Number)::Vector/Dict{Number}` (more adaptive/resampling methods required, index not lagged)

  - `BootBand(idx::1dxts{Bool}, ret::1dxts{Number}, bounds::Vector{2, Number})` 

  - `NoiseBB(idx::1dxts{Bool}, ret::1dxts{Number}, bounds::Vector{2, Number})`

  - `DensityDist(d1::Vector{Number}, d2::Vector{Number}, measure::Vector{Number})::Number`

  - `DistMatrix(boolsignal::Vector{1dxts{Bool}}, est = EstDensity, dis = DensityDist)::Array{2, Number}`

  - `HieClustering(dist::Array{2, Number}, ngroup::Int64)::Vector{Vector{Int64}; dthreshold::Number}::Number` 
    - Output a number measuring differentiating power, can be the distance, distance should be comparable for all density functions, then to decide whether this signal can be used or not

  - Select signal sets (systematic and individual signals) and obtain conditional distributions, copulas, individual measure change for each asset every day.

