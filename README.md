# GreenSturgeonDSM
GreenSturgeonDSM is an R-based decision support model developed to evaluate green sturgeon population dynamics under varying environmental conditions, habitat characteristics, and management actions. The model integrates data on flow, habitat, temperature, calibration datasets, and user-defined scenarios to support fisheries research and conservation planning.

This repository includes all components needed to run the base model, calibrate parameters, perform optimization, and conduct sensitivity analyses.

## Repository Structure
The repository is organized into directories that separate the core model, calibration tools, optimization workflows, sensitivity analyses, and required input data.

### R/
Primary model scripts and function libraries.
* `GSmod.R`- main green sturgeon DSM model
* `StrategiestoRun.R`- Run SIT strategies through the DSM
* `Functions/`- all sub-model components (movement, spawning, survival, entrainment, habitat capacity, etc.)
* `Results`- outputs generated from model runs

### Optimize/
Contains scripts and outputs related to optimization of mode parameters or strategies.
* `GSmodOpt.R`- optimization model
* `OPTIMIZE.R`- processing and running optimization
* `OptimizeResults.rds`- saved optimization results

### calibration/
Contains calibration functions, scripts, and figures.
* `GSmodCalibrate.R`- calibration model
* `CalibrationResults.R`- processing/plotting of calibration outputs
* `calibration_Fig.png` and `CalibFig_Supplement.png`- calibration figures
* `functions/`- calibration-specific functions

### data/
All data required to run the DSM.
* Tributary habitat-flow curves
* `EntrainmentProbs.csv`
* `CalSim3_metrics.rda`
* `stream_temperature.rda`
* `delta_temperature.rda`
* Other required hydrological and biological input datasets

### sensitivity analysis/
Scripts, datasets, EVPI tools, and output figures related to sensitivity analysis.
* `GSmodSens.R`- sensitivity model
* `GSmodOneWayAnalysis.R`- Running the sensitivity model runs
* `OneWaySensResults.R`-process model parameter sensitivity results
* `OneWaySensINPUTS.R`- process input sensitivity results
* `.csv`files containing biological and input parameters for analyses
* `Color_tornado.png`- tornado diagram (model parameters)
* `sensitivity_tornadoInput.png`- tornado diagram (model inputs)
* `functions/`- sensitivity-specific functions

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
The Green Sturgeon DSM includes an extensive sensitivity analysis framework designed to quantify how uncertainties in biological parameters, environmental inputs, and habitat characteristics influence model outputs and management strategy performance. Sensitivity analyses are implemented through GSmodSens.R, GSmodOneWayAnalysis.R, and EVPI scripts (EVPI_GS.R, EVPI_GSINPUTS.R).
This analysis provides insight into:
* parameter influence on abundance, recruitment, spawner returns, holding adults, river usage, and rearing/spawning duration
* robustness of management strategies under uncertainty
* which uncertainties matter most for population outcomes
* where additional monitoring or data collection would most improve decision making


### 1. One-way Sensitivity Analysis
Each parameter listed in GSsensParms.csv is varied across a defined range of proportional change. For each parameter increment:

1. The DSM is run for every management strategy.
2. Outputs (abundance, recruitment, spawners, number of rivers, rearing duration, spawning duration, and composite utility) are recorded.
#### Outputs Produced
* Strategy attribute correlation matrix
* Tornado diagrams for each attribute-Each tornado diagram shows the range of possible outcomes for each parameter and ranks parameters by influence.
* Utility-based tornado diagrams identifying model parameters and inputs with the strongest influence on the overall SIT decision metrics.
  
### 2. Expected Value of Perfect Information (EVPI)
EVPI quantifies how much improved knowledge about uncertain parameters or inputs would increase expected population performance.

#### EVPI on Biological Model Parameters
Using `EVPI_GS.R`:
• Probability distributions are assigned to each parameter (normal, beta, uniform depending on the parameter).
• Model outputs are weighted across parameter distributions.
• Weighted expected abundance is compared to the best strategy under uncertainty.

#### EVPI on Model Inputs
Using EVPI_GSINPUTS.R, the same framework is applied to uncertain model inputs:
* stream temperature
* Delta temperature
* flow (CalSim III)
* river diversions
* Delta diversions
* predation intensity
* tributary habitat availability
* initial population size
* HAB event probability
  
Output files:
```r
EVPI_res.csv
EVPI_InputRes.csv
EVPI_InputResDIST.csv
```

These rank input uncertainties based on their decision-relevant importance.

### 3. Response-Profile Sensitivity Analysis
Response-profile analysis evaluates how strategy ranking changes across parameter distributions. Code is available in `OneWaySensResults.R`

For each parameter:
*Strategy utilities are computed across all increments.
*Parameter response curves are generated (expected utility vs. parameter percentile).
*The number of times the top strategy or top three strategies change is counted.

This produces:
• Parameter stability scores
• Strategy robustness rankings
• Combined response-profile plots
• Individual attribute-based response profiles (abundance, recruitment, spawners, large spawners, holders, rivers, rearing duration, spawning duration, utility)

### 4. Interpretation
Sensitivity analysis results can be used to:
* identify influential biological processes (e.g., egg survival, juvenile migration, predation, entrainment)
* identify critical environmental uncertainties (flows, temperature, habitat, diversions)
* rank management strategies under uncertainty
* prioritize monitoring or research
* support structured decision making
Together, these analyses provide a complete picture of uncertainty propagation in the DSM and how it affects management strategy evaluation.
