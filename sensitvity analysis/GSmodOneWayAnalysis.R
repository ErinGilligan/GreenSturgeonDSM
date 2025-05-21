#####################################################
####   Green Sturgeon SIT MODEL for Sensitivity   ###
#####################################################
setwd("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code")
source("strategiestoRun.R")
source("GSmodsens.R")


setwd("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/SensitivityAnalysis")
sen.table<-read.csv("GSsensParms.csv", stringsAsFactors = F)

library(parallel)
library(foreach)
library(doParallel)

# set up parallel
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# Export necessary variables and functions to the cluster
all_funcs <- lsf.str()
clusterExport(cl, c("run.scenarios", all_funcs,"rtri"))
clusterExport(cl, c("Qmean", "stream_temp", "Hab", "diversions", "dltadivs", "pred", 
                    "entrain", "tribs2use", "tribsDist", "N0", "st.fish",
                    "delta_temperature","contact_points","delta_contact_points",
                    "UpSacHab",'UpMidSacHab','MidSacHab','LowSacHab','FeatherHab',
                    'YubaHab','BearHab','SJHab','CalSim3_metrics','prop_high_predation'))

parm<-sen.table$Parameter


# Create dummy vector because one way
dumm<-rep("XXX",length(parm))
parm<-cbind(parm,dumm)
incr<-cbind(seq(0.05,0.95,by=0.1),0.5)



decis.df <- data.frame(matrix(nrow=yrs, ncol=length(decis.cmb)))
colnames(decis.df) <- names(decis.cmb)

# Fill the data frame
for(strat in names(decis.cmb)) {
  decis.df[[strat]] <- sapply(decis.cmb[[strat]], 
                              function(x) paste(x, collapse=","))
}

# Modify how you set up parameter combinations
param_combinations <- list()
counter <- 1

for (strat_name in names(decis.cmb)) {
  current_strategy <- decis.cmb[[strat_name]]
  
  for(pp in 1:nrow(parm)) {
    for(pct in 1:nrow(incr)) {
      param_combinations[[counter]] <- list(
        strategy_name = strat_name,
        strategy = current_strategy,
        param1 = parm[pp,1],
        param2 = parm[pp,2],
        incr1 = incr[pct,1],
        incr2 = incr[pct,2]
      )
      counter <- counter + 1
    }
  }
}

# Run sensitivity analysis in parallel
one.way_results <- foreach(combo = param_combinations, .combine = rbind, .packages = c("base")) %dopar% {
  # Run the scenario with the specific strategy
  util.score <- run.scenarios(
    decis.X = combo$strategy,
    runz = 1,
    years = 20,
    stochastic = 0,
    parms.X = c(combo$param1, combo$param2),
    incr.X = c(combo$incr1, combo$incr2)
  )
  
  # Return results with strategy name included
  data.frame(
    Strategy = combo$strategy_name,
    Util = util.score,
    parm1 = combo$param1,
    parm2 = combo$param2,
    incr1 = combo$incr1,
    incr2 = combo$incr2
  )
}

# Stop the cluster
stopCluster(cl)

# View results
one.way_results

# Optionally save results
write.csv(one.way_results, "C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/sensitivity_analysis_resultsFINALIZED2.csv", row.names = FALSE)

