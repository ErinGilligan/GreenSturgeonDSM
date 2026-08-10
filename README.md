# GreenSturgeonDSM
GreenSturgeonDSM is an R-based decision support model developed to evaluate green sturgeon population dynamics under varying environmental conditions, habitat characteristics, and management actions. The model integrates data on flow, habitat, temperature, calibration datasets, and user-defined scenarios to support fisheries research and conservation planning.

This repository includes all components needed to run the base model, calibrate parameters, perform optimization, and conduct sensitivity analyses.

## Running the green sturgeon DSM
The Green Sturgeon DSM simulates population dynamics across river tributaries, Delta, Bay, and ocean over a 20‑year period. The model uses:
* CalSim III temperature, flow, and water year type datasets
* habitat–flow relationships
* diversion/entrainment data
* a 20‑year sequence of management decisions (strategies)

### Strategies
Management strategies are stored in a list (`decis.cmb`), where each strategy is a 20-element list representing annual restoration actions. Example:
```r
decis.cmb$do_nothing
decis.cmb$Feather_strategy
...
```
### Running a Single Strategy
The main simulation is run using:
```r
run.scenarios(decis.X, runz = 1, years = 20, stochastic = 0)
```
Example:
```r
result <- run.scenarios(
  decis.X    = decis.cmb$Screening_strategy,
  runz       = 1,
  years      = 20,
  stochastic = 0
)
```
Output includes annual dynamics:
* Total population abundance
* Recruitment
* Spawner metrics
* Spatial diversity
* rearing duration

### Running All Strategies in Parallel
To evaluate all strategies:
```r
cl <- makeCluster(detectCores() - 1)
clusterExport(cl, c("run_single_scenario", "run.scenarios", "decis.cmb", ...))

results <- parLapply(cl, all_params, run_single_scenario)
stopCluster(cl)

all.decis <- do.call(rbind, results)
```

## Model Calibration
### Overview
Calibration adjusts several key parameters controlling:
* egg incubation survival
* larval survival
* juvenile survival
* delta survival
* aging transitions (juvenile → subadult, subadult → small adult, small adult → large adult)

The goal is to minimize the sum of squared error (SSE) between simulated mean abundances over a 20‑year run and the target abundances reported in Dudley et al. (2024):

The calibration script `GSmodCalibrate.R` evaluates parameter vectors by running the DSM and comparing simulated stage means against target abundances:
```r
TARGET <- c(1730, 1449, 1298, 2432, 2403, 1471, 960)
```

### Calibration Fucntion
To run the calibration model use the function:
```r
run.all(vect, decis.X, mode = "cal", stochastic = 0)
```
Where:
* `vect` is a 7-element vector of calibration parameters
* `decis.X` is a 20-year vector of decisions (set to "do nothing" for calibration purposes)
* `mode` = "cal" activates calibration mode
* `stochastic` = 0 ensures reproducible deterministic runs
  
The function returns a sum of squared error (SSE) between model‑predicted mean abundances and the target abundances.
Example:
```r
decis_vector <- rep(1, 20)
run.all(c(0.001, 0.90, 0.99, 0.99, 0.17, 0.22, 0.39),
        decis.X = decis_vector,
        mode = "cal",
        stochastic = 0)
```

### Parallel Parameter Search
The calibration script performs a two‑phase parameter search:
#### 1. Coarse Grid Search
A wide parameter grid is evaluated using parallel workers:
```r
search_coarse <- expand.grid(
  seq(0.0005, 0.002, 0.0003),   # egg survival
  seq(0.80, 0.95, 0.05),        # larval survival
  seq(0.95, 1.00, 0.01),        # juvenile survival
  seq(0.95, 1.00, 0.01),        # delta survival
  seq(0.10, 0.25, 0.01),        # aging: small adult transition
  seq(0.15, 0.30, 0.05),        # aging: subadult transition
  seq(0.30, 0.45, 0.05)         # aging: juvenile transition
)

shot.coarse <- parApply(cl, search_coarse, 1, yo.dog)
best.coarse <- search_coarse[which.min(shot.coarse), ]
```
#### 2. Ultra-Fine Refinement
```r
search_ultra_fine <- expand.grid(
  seq(0.0009, 0.0011, 0.00005),
  seq(0.89, 0.91, 0.005),
  seq(0.98, 0.99, 0.01),
  seq(0.98, 0.99, 0.01),
  seq(0.16, 0.18, 0.005),
  seq(0.21, 0.23, 0.005),
  seq(0.37, 0.39, 0.005)
)
shot <- parApply(cl, search_ultra_fine, 1, yo.dog)
best <- search_ultra_fine[which.min(shot), ]
```

### Outputs
Calibration produces:
* optimal parameter vector
* minimal SSE
* diagnostic comparisons of simulated vs. target stage abundances

## Sensitivity Analysis
The Green Sturgeon DSM supports systematic sensitivity analysis to evaluate how uncertainty in biological, environmental, and habitat parameters influences model outcomes. Sensitivity analysis is implemented in the script `GSmodSens.R` and can be run for any of the predefined management strategies

### One-way
